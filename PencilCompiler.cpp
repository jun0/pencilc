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
//
// Function pointers are treated separately from data pointers, although they
// carry the exact same types of data.  A FunctionType should *not* be visited
// directly with this visitor.  Instead break it down manually and visit its
// argument types and return type individually.
template<typename Result> class TypeVisitor {
protected:
  ASTContext &ctx;
  virtual Result forFunctionPointer (QualType, const PointerType &) = 0;
  virtual Result forPointer (QualType, const PointerType &) = 0;
  virtual Result forScalar (QualType, const BuiltinType &) = 0;
  virtual Result forStruct (QualType, const RecordType &) = 0;
  virtual Result forUnion (QualType, const RecordType &) = 0;
  virtual Result forEnum (QualType, const EnumType &) = 0;
  virtual Result forIncompleteArray (QualType, const IncompleteArrayType &) = 0;
  virtual Result forConstantArray (QualType, const ConstantArrayType &) = 0;
  virtual Result forVariableArray (QualType, const VariableArrayType &) = 0;

public:
  Result operator () (const QualType &type)
  {
    ASTContext &ctx = TypeVisitor<Result>::ctx;
    if (const PointerType *t = type->getAs<PointerType> ())
      {
        if (t->isFunctionPointerType ())
          return forFunctionPointer (type, *t);
        else return forPointer (type, *t);
      }
    else if (const VariableArrayType *a = ctx.getAsVariableArrayType (type))
      return forVariableArray (type, *a);
    else if (const IncompleteArrayType *a = ctx.getAsIncompleteArrayType (type))
      return forIncompleteArray (type, *a);
    else if (const ConstantArrayType *a = ctx.getAsConstantArrayType (type))
      return forConstantArray (type, *a);
    else if (const BuiltinType *t = type->getAs<BuiltinType> ())
      return forScalar (type, *t);
    else if (const RecordType *t = type->getAsStructureType ())
      return forStruct (type, *t);
    else if (const RecordType *t = type->getAsUnionType ())
      return forUnion (type, *t);
    else if (const EnumType *t = type->getAs<EnumType> ())
      return forEnum (type, *t);

    // Could get here if in C++ mode.
    abort ();
  }

  TypeVisitor (ASTContext &_ctx) : ctx (_ctx) {}
};

// Adds a default case -- all non-overridden cases go to forDefault().
// Additionally, all arrays go through forArray() before going to forDefault(),
// so arrays can be caught uniformly there.
//
// I'm contemplating the addition of this defaulting mechanism to
// TypeVisitor<>, but it can be error-prone (missing cases are silently
// defaulted) so I'm not sure if I should.
template <typename Result>
class TypeVisitorWithDefault : public TypeVisitor<Result>
{
protected:
  virtual Result forDefault (QualType) = 0;

  virtual Result forFunctionPointer (QualType t, const PointerType &)
  {
    return forDefault (t);
  }
  virtual Result forPointer (QualType t, const PointerType &)
  {
    return forDefault (t);
  }
  virtual Result forScalar (QualType t, const BuiltinType &)
  {
    return forDefault (t);
  }
  virtual Result forStruct (QualType t, const RecordType &)
  {
    return forDefault (t);
  }
  virtual Result forUnion (QualType t, const RecordType &)
  {
    return forDefault (t);
  }
  virtual Result forEnum (QualType t, const EnumType &)
  {
    return forDefault (t);
  }
  virtual Result forArray (QualType t, const ArrayType &)
  {
    return forDefault (t);
  }
  virtual Result forIncompleteArray (QualType t, const IncompleteArrayType &a)
  {
    return forArray (t, a);
  }
  virtual Result forConstantArray (QualType t, const ConstantArrayType &a)
  {
    return forArray (t, a);
  }
  virtual Result forVariableArray (QualType t, const VariableArrayType &a)
  {
    return forArray (t, a);
  }

  TypeVisitorWithDefault (ASTContext &ctx) : TypeVisitor<Result> (ctx) {}
};

// Checks whether a given type is a valid PENCIL type.  Call report () to get a
// series of callbacks with the error messages.
// FIXME: freeze a specific format for the error messages so that it can be
// manipulated reliably.
struct TypeWFChecker : public TypeVisitor<void> {

  // Error messages will be queued up in `violations', indexed by priority.
  // It's usually useful to only report the highest-priority violations, so
  // e.g., if we find a function pointer we don't waste our time complaining
  // that its argument arrays have no size information.  But all the
  // violation information does get kept, in case it's useful.
  enum ViolationPriority {
    IncompleteType = 1,         // A constituent type is missing information,
                                // but can be valid PENCIL with some addition.
                                // e.g. arrays without size information.
    NonPencilType = 5           // A constituent type is fundamentally
                                // incompatible with PENCIL, e.g. contains a
                                // function pointer.
  };

  std::map< ViolationPriority, std::deque<std::string> > violations;
  std::string (*formatErrorMessage) (const std::string &);

  // Skip formatting by formatErrorMessage ().
  void addFormattedViolation (ViolationPriority p, const std::string &msg)
  {
    violations[p].push_back (msg);
  }

  void addViolation (ViolationPriority p, const std::string &msg)
  {
    addFormattedViolation (p, formatErrorMessage (msg));
  }

  virtual void forFunctionPointer (QualType t, const PointerType &pt)
  {
    addViolation (NonPencilType,
                  "function pointer type '" + t.getAsString () + "'");
  }

  virtual void forPointer (QualType t, const PointerType &pt)
  {
    addViolation(NonPencilType, "pointer type '" + t.getAsString () + "'");
  }

  virtual void forScalar (QualType t, const BuiltinType &)
  {
    // Scalars are always OK.
  }

  virtual void forIncompleteArray (QualType t, const IncompleteArrayType &a)
  {
    addViolation (IncompleteType,
                  "unsized array type '" + t.getAsString ()
                  + "' - write 'T a[n]' using some variable 'n' if you"
                  + " want dynamic size");
  }

  virtual void forConstantArray (QualType t, const ConstantArrayType &a)
  {
    (*this) (a.getElementType ());
  }

  virtual void forVariableArray (QualType t, const VariableArrayType &a)
  {
    // FIXME: Restrict to rational-function expressions.  We probably need
    // polynomial for things like matrices, while division is required for
    // scaling down an input size.
    assert (a.getSizeExpr ());
    if (a.getSizeModifier () == ArrayType::Star)
      {
        addViolation (NonPencilType,
                      "invalid array type '"
                      + t.getAsString () + "', size"
                      + " must be arithmetical expression, possibly"
                      + " referencing variables");
      }
    else if (a.getSizeExpr ()->hasNonTrivialCall (ctx))
      {
        addViolation (NonPencilType,
                      "invalid array type '"
                      + t.getAsString () + "' -  size must be"
                      + " arithmetical expression, possibly referencing"
                      + " variables");
      }
    (*this) (a.getElementType ());
  }

  // structs may not contain pointer types -- this is easily checked for
  // structs whose definitions are visible at their points of use.  A tricky
  // issue is incomplete structs, which, as with the x (of type struct s) in
  // 
  //   struct s;
  //   /* (1) */
  //   void f (struct s * const restrict x)
  //   {
  //      /* (2) */
  //   }
  //   struct s {
  //     int *p;
  //   };
  //
  // may *become* a pointer-containing type after its use has occurred.  Worse
  // yet, the definition of the struct s may never be visible to pencilc.
  //
  // We believe incomplete structs are nonetheless OK, under the assumption
  // that:
  //
  //   - non-PENCIL functions called from within PENCIL do not create aliasing.
  //   
  //   - struct definitions within PENCIL functions are required to have no
  //     pointer members.
  // 
  // The only ways that the (2) part in the code above can introduce aliasing
  // are:
  //
  //    - by assigning to x
  //
  //    - by calling a non-PENCIL function that assigns pointer members of x
  //
  // For the first to be possible, the definition of the struct must be visible
  // at the assignment site, so the definition must either occur at (1) or (2).
  // A struct occuring at (1) would make the pointer member visible to the
  // function parameter type checker, so this will be caught promptly.  This is
  // also the case if (1) had a prototype for f() and a definition of struct s,
  // in that order.  A pointer-containing struct definition occuring at (2)
  // will also be checked and caught promptly.
  //
  // The second kind of aliasing is ruled out by fiat.
  // 
  // It follows that an incomplete struct can be a valid PENCIL struct.  It is
  // only explicit struct definitions with pointer members that can cause
  // problems.
  virtual void forStruct (QualType t, const RecordType &r)
  {
    RecordDecl *decl = r.getDecl ();
    assert (decl);

    for (RecordDecl::field_iterator i = decl->field_begin ();
         i != decl->field_end ();
         ++i)
      (*this) (i->getType ());
  }

  virtual void forUnion (QualType t, const RecordType &r)
  {
    addViolation (NonPencilType, "union type '" + t.getAsString () + "'");
  }

  virtual void forEnum (QualType t, const EnumType &r)
  {
    // Enum types are always OK.
  }

public:

  bool hasErrors () const
  {
    return !violations.empty ();
  }

  const std::deque<std::string> &getTopPriorityViolations () const
  {
    assert (hasErrors ());
    return violations.rbegin ()->second;
  }

  TypeWFChecker (ASTContext &ctx,
                 std::string (*_formatErrorMessage) (const std::string &))
  : TypeVisitor<void> (ctx), formatErrorMessage (_formatErrorMessage)
  {}
};

// Checks PENCIL violations in a single function parameter declaration.  The
// outermost array/pointer dimension is handled specially, so the TypeWFChecker
// should only be used to check "deep" constituent types.  This class is mostly
// a wrapper that implements this special-casing.
class FunctionParamChecker : private TypeVisitorWithDefault<void> {
  TypeWFChecker deep_checker;
  ParmVarDecl &decl;

  static std::string formatDeepError (const std::string &msg)
  {
    return "argument type contains " + msg;
  }

  virtual void forArray (QualType t, const ArrayType &a)
  {
    // Shouldn't happen.
    abort ();
  }

  virtual void forFunctionPointer (QualType t, const PointerType &p)
  {
    deep_checker.addFormattedViolation (TypeWFChecker::NonPencilType,
                                        "function pointers not allowed");
  }

  virtual void forPointer (QualType t, const PointerType &p)
  {
    QualType origType = decl.getOriginalType ();
    bool missingConst = ! origType.isConstQualified ();
    bool missingRestrict = ! origType.isConstQualified ();
    bool missingStatic = false;
    bool isArray;

    if ((isArray = origType->isArrayType ()))
      {
        const ArrayType *a = ctx.getAsArrayType (origType);

        // Missing qualifiers are coalesced into a single report and reported
        // here, including missing static.  Size problems are reported
        // separately as part of the requirement that the whole type be a valid
        // PENCIL type, i.e. in the deep checker.
        if (a->getSizeModifier () != ArrayType::Static)
          missingStatic = true;
      }

    if (missingStatic || missingConst || missingRestrict)
      {
        std::string msg = std::string (isArray ? "array" : "pass-by-pointer")
          + " argument must be qualified"
          + (isArray ? " static" : "")
          + " const restrict";
        deep_checker.addFormattedViolation (TypeWFChecker::IncompleteType, msg);
      }

    deep_checker (isArray ? origType : p.getPointeeType ());
  }

  virtual void forDefault (QualType t)
  {
    deep_checker (t);
  }

public:

  bool hasErrors () const
  {
    return deep_checker.hasErrors ();
  }

  void report (DiagnosticsFormatter &formatter) const
  {
    if (!deep_checker.hasErrors ())
      return;

    const std::deque<std::string> &violations
      = deep_checker.getTopPriorityViolations ();

    for (std::deque<std::string>::const_iterator i = violations.begin ();
         i != violations.end ();
         ++i)
      formatter (DiagnosticsEngine::Error,
                 decl.getLocStart (),
                 "PENCIL violation: " + *i);
  }

  FunctionParamChecker (CompilerInstance &_CI, ParmVarDecl &_decl)
    : TypeVisitorWithDefault<void> (_CI.getASTContext ()),
      deep_checker (_CI.getASTContext (), formatDeepError),
      decl (_decl)
  {
    (*this) (decl.getType ());
  }
};

class FunctionRetTypeChecker : private TypeVisitorWithDefault<void> {
  TypeWFChecker deep_checker;
  const SourceLocation loc;

  static std::string formatDeepError (const std::string &msg)
  {
    return "return type contains " + msg;
  }

  virtual void forArray (QualType t, const ArrayType &a)
  {
    // Clang already complains about array return types.
  }

  // There's no implicit array->pointer conversion in return types, so if we
  // get here then we really have a pointer.
  virtual void forPointer (QualType t, const PointerType &p)
  {
    deep_checker.addFormattedViolation (TypeWFChecker::NonPencilType,
                                        "return type must be scalar or struct");
  }

  virtual void forDefault (QualType t)
  {
    deep_checker (t);
  }

public:

  bool hasErrors () const
  {
    return deep_checker.hasErrors ();
  }

  void report (DiagnosticsFormatter &formatter) const
  {
    if (!deep_checker.hasErrors ())
      return;

    const std::deque<std::string> &violations
      = deep_checker.getTopPriorityViolations ();

    for (std::deque<std::string>::const_iterator i = violations.begin ();
         i != violations.end ();
         ++i)
      formatter (DiagnosticsEngine::Error,
                 loc, "PENCIL violation: " + *i);
  }

  FunctionRetTypeChecker (CompilerInstance &_CI, SourceLocation _loc,
                          QualType rettype)
    : TypeVisitorWithDefault<void> (_CI.getASTContext ()),
      deep_checker (_CI.getASTContext (), formatDeepError),
      loc (_loc)
  {
    (*this) (rettype);
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
          FunctionRetTypeChecker retcheck (CI, fun->getLocStart (),
                                           fun->getResultType ());
          if (retcheck.hasErrors ())
            retcheck.report (diagnostics);
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
