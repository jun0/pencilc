//===- PencilCompiler.cpp ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Example clang plugin which simply prints the names of all the top-level decls
// in the input file.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/AST.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/PriorityQueue.h"
using namespace clang;

namespace {

class DiagnosticsFormatter {
  CompilerInstance &CI;
public:
  DiagnosticsFormatter (CompilerInstance &_CI) : CI (_CI) {}

  DiagnosticBuilder operator () (DiagnosticsEngine::Level level,
                                 const SourceLocation &loc,
                                 const char *message)
  {
    DiagnosticsEngine &Diagnostics = CI.getDiagnostics ();

    return Diagnostics.Report (loc,
                               Diagnostics.getCustomDiagID (level, message));
  }

  DiagnosticBuilder operator () (DiagnosticsEngine::Level level,
                                 const SourceLocation &pos,
                                 const std::string &message)
  {
    return (*this) (level, pos, message.c_str ());
  }
};

// Not sure if the "visitor pattern" is supposed to include structural
// recursion, but this class implements only the pattern-match part.  Recursion
// is achieved by having the for*() methods building a new visitor object as
// needed.
template<typename Result> class TypeVisitor {
protected:
  ASTContext &ctx;
  virtual Result forPointer (QualType, const PointerType &) = 0;
  virtual Result forScalar (QualType, const BuiltinType &) = 0;
  virtual Result forStruct (QualType, const RecordType &) = 0;
  virtual Result forUnion (QualType, const RecordType &) = 0;
  virtual Result forEnum (QualType, const EnumType &) = 0;
  virtual Result forArray (QualType, const ArrayType &) = 0;
  // TODO: complex

public:
  Result operator () (const QualType &type)
  {
    if (const PointerType *t = type->getAs<PointerType> ())
      return forPointer (type, *t);
    else if (const ArrayType *t = ctx.getAsArrayType (type))
      return forArray (type, *t);
    else if (const BuiltinType *t = type->getAs<BuiltinType> ())
      return forScalar (type, *t);
    else if (const RecordType *t = type->getAsStructureType ())
      return forStruct (type, *t);
    else if (const RecordType *t = type->getAsUnionType ())
      return forUnion (type, *t);
    else if (const EnumType *t = type->getAs<EnumType> ())
      return forEnum (type, *t);

    abort ();
  }

  TypeVisitor (ASTContext &_ctx) : ctx (_ctx) {}
};

template<typename Result> class TypeVisitorWithArrayDetails
  : public TypeVisitor<Result> {
protected:
  virtual Result forIncompleteArray (QualType, const IncompleteArrayType &) = 0;
  virtual Result forConstantArray (QualType, const ConstantArrayType &) = 0;
  virtual Result forVariableArray (QualType, const VariableArrayType &) = 0;

  // C++ types shouldn't appear, but just a note that Clang has this.
  // virtual Result forDependentSizedArray (QualType,
  //                                        const DependentSizedArrayType*) = 0;

  virtual Result forArray (QualType t, const ArrayType &a)
  {
    ASTContext &ctx = TypeVisitor<Result>::ctx;
    if (const VariableArrayType *a = ctx.getAsVariableArrayType (t))
      return forVariableArray (t, *a);
    if (const IncompleteArrayType *a = ctx.getAsIncompleteArrayType (t))
      return forIncompleteArray (t, *a);
    if (const ConstantArrayType *a = ctx.getAsConstantArrayType (t))
      return forConstantArray (t, *a);

    // Could get here only if the compilation mode is C++.
    abort ();
  }

public:
  TypeVisitorWithArrayDetails (ASTContext &ctx) : TypeVisitor<Result> (ctx) {}
};



// Checks PENCIL violations in a single function parameter declaration.
class FunctionParamChecker : private TypeVisitorWithArrayDetails<bool> {
  CompilerInstance &CI;
  ParmVarDecl &decl;

  // Error messages will be queued up in `violations', indexed by priority.
  // Only the highest-priority violations will be reported, so that we don't
  // report, e.g., that an array type must have this or that qualifier when its
  // element type is a function pointer.
  enum ViolationPriority {
    MissingQualOrSize = 1,
    NonPencilType = 5
  };

  std::map< ViolationPriority, std::deque<std::string> > violations;

  void addViolation (ViolationPriority p, const std::string &msg)
  {
    violations[p].push_back ("PENCIL violation: " + msg);
  }

  // Code for walking the type tree.  forPointer() and forArray() are never
  // invoked on the first dimension; they are always called at constituent
  // types.

  // The boolean parameter is meant to be used to trace the tree walk, but this
  // is not implemented yet.  Returns true if a violation was found.

  virtual bool forPointer (QualType t, const PointerType &pt)
  {
    if (pt.isFunctionPointerType ())
      addViolation (NonPencilType,
                    "argument type contains function pointer type '"
                    + t.getAsString () + "'");
    else
      addViolation(NonPencilType,
                   "argument type contains pointer type '"
                   + t.getAsString () + "' as element or member");
    return true;
  }

  virtual bool forScalar (QualType t, const BuiltinType &)
  {
    // Scalars are always OK.
    return false;
  }

  virtual bool forIncompleteArray (QualType t, const IncompleteArrayType &a)
  {
    addViolation (MissingQualOrSize,
                  "argument type contains unsized array type '"
                  + t.getAsString ()
                  + "' - say e.g. f (int n, T a[n]) if you want to"
                  + " assign dynamic sizes");
    return true;
  }

  virtual bool forConstantArray (QualType t, const ConstantArrayType &a)
  {
    return (*this) (a.getElementType ());
  }

  virtual bool forVariableArray (QualType t, const VariableArrayType &a)
  {
    bool violation_found = false;
    // FIXME: Restrict to linear (or polynomial) expressions.
    if (a.getSizeExpr ()->hasNonTrivialCall (ctx))
      {
        addViolation (NonPencilType,
                      "argument type contains array type '"
                      + t.getAsString () + "' whose size"
                      + " expression is too complex");
        violation_found = true;
      }
    return (*this) (a.getElementType ()) || violation_found;
  }

  virtual bool forStruct (QualType t, const RecordType &r)
  {
    RecordDecl *decl = r.getDecl ();
    assert (decl);
    bool found_violation = false;

    // Note: incomplete structs are handled by Clang proper.  Since you can't
    // use pointers, incomplete structs cannot be used anywhere at all.
    for (RecordDecl::field_iterator i = decl->field_begin ();
         i != decl->field_end ();
         ++i)
      found_violation = (*this) (i->getType ()) || found_violation;
    return found_violation;
  }

  virtual bool forUnion (QualType t, const RecordType &r)
  {
    addViolation (NonPencilType,
                  "argument type contains union type '"
                  + t.getAsString () + "'");
    return true;
  }

  virtual bool forEnum (QualType t, const EnumType &r)
  {
    // Enum types are always OK.
    return false;
  }

public:

  bool hasErrors () const
  {
    return !violations.empty ();
  }

  // Report the highest-priority PENCIL violations found in this parameter
  // declaration.
  void report (DiagnosticsFormatter &formatter) const
  {
    if (violations.empty ()) return;

    const std::deque<std::string> &msgs = violations.rbegin ()->second;
    for (std::deque<std::string>::const_iterator i = msgs.begin ();
         i != msgs.end ();
         ++i)
      formatter (DiagnosticsEngine::Error,
                 decl.getLocStart (),
                 *i);
  }

  FunctionParamChecker (CompilerInstance &_CI, ParmVarDecl &_decl)
    : TypeVisitorWithArrayDetails (_CI.getASTContext ()), CI (_CI), decl (_decl)
  {
    QualType orig_type = decl.getOriginalType ();
    QualType type = decl.getType ();

    if (orig_type->isArrayType () && type->isPointerType ())
      {
        // Array parameters are implicitly cast to pointer types in the first
        // dimension.  The original type information preserves the array type,
        // but it seems to discard qualifiers in the first dimension.  We need
        // to slap the qualifiers back on.
        const ArrayType *a = ctx.getAsArrayType (orig_type);
        assert (a);
        bool missingStatic = a->getSizeModifier () != ArrayType::Static;
        bool missingConst = !type.isConstQualified ();
        bool missingRestrict = !type.isRestrictQualified ();

        if (missingStatic || missingConst || missingRestrict)
          {
            std::string msg = "array parameter must be qualified static const"
                              " restrict (missing";
            if (missingStatic) msg += " static";
            if (missingConst) msg += " const";
            if (missingRestrict) msg += " restrict";
            addViolation (MissingQualOrSize, msg + ")");
          }
        (*this) (a->getElementType ());
      }
    else
      (*this) (type);
  }
};

class PencilCheckConsumer : public ASTConsumer {
  CompilerInstance &CI;
  DiagnosticsFormatter diagnostics;

public:
  PencilCheckConsumer (CompilerInstance& _CI)
  : CI (_CI), diagnostics (_CI)
  {}

  virtual bool HandleTopLevelDecl(DeclGroupRef DG) {
    for (DeclGroupRef::iterator i = DG.begin (), e = DG.end (); i != e; ++i) {
      Decl *D = *i;
      if (FunctionDecl *fun = dyn_cast<FunctionDecl>(D))
        {
          if (fun->getResultType ()->isPointerType ())
            {
              diagnostics (DiagnosticsEngine::Error,
                           D->getLocStart (),
                           "function " + fun->getNameAsString ()
                           + " returns pointer type; PENCIL only allows"
                           + " returning scalars and structs");
            }
          for (FunctionDecl::param_iterator param = fun->param_begin ();
               param != fun->param_end ();
               ++param)
            {
              FunctionParamChecker check (CI, **param);
              if (check.hasErrors())
                check.report (diagnostics);
            }
        }
    }

    return true;
  }
};

class PencilCompilerAction : public PluginASTAction {
protected:
  ASTConsumer *CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) {
    return new PencilCheckConsumer(CI);
  }

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string>& args) {
    if (args.size() && args[0] == "help")
      PrintHelp(llvm::errs());

    return true;
  }
  void PrintHelp(llvm::raw_ostream& ros) {
    ros << "pencilc: this plugin checks that the input source conforms to PENCIL specs.\n";
  }

};

}

static FrontendPluginRegistry::Add<PencilCompilerAction>
X("pencilc", "PENCIL compilation frontend");
