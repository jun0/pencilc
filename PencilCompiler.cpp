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
#include "clang/AST/TypeVisitor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/PriorityQueue.h"
using namespace clang;

namespace pencil {

class DiagnosticsFormatter {
  CompilerInstance &CI;
public:
  DiagnosticsFormatter (CompilerInstance &_CI) : CI (_CI) {}

  CompilerInstance &getCompilerInstance () {
    return CI;
  }

  DiagnosticBuilder operator () (DiagnosticsEngine::Level level,
                                 const SourceLocation &loc,
                                 const char *message) {
    DiagnosticsEngine &Diagnostics = CI.getDiagnostics ();

    return Diagnostics.Report (loc,
                               Diagnostics.getCustomDiagID (level, message));
  }

  DiagnosticBuilder operator () (DiagnosticsEngine::Level level,
                                 const SourceLocation &pos,
                                 const std::string &message) {
    return (*this) (level, pos, message.c_str ());
  }
};

// I can't seem to find a type visitor that includes (local) qualifiers, so
// here's a visitor that keeps them around (code copied from TypeVisitor.h).  A
// new instance must be created for every recursive call.
//
// The qualified types are passed in as QualType, not split into Qualifiers and
// Type, since QualType is easier to print.
// 
// Implementation note: TypeVisitor has to be inherited publicly since the
// dispatcher does a downcast.  VisitFooType() methods also have to be public
// because they're called explicitly after the downcast as members of the
// derived class.

#define DISPATCH(CLASS) \
  return static_cast<ImplClass*>(this)-> \
           Visit##CLASS(q, static_cast<const CLASS*>(T))

template<typename ImplClass, typename RetTy=void>
class QualTypeVisitor {
public:
  RetTy Visit(const QualType q) {
    const Type *T = q.split ().Ty;
    // Top switch stmt: dispatch to VisitFooType for each FooType.
    switch (T->getTypeClass()) {
#define ABSTRACT_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT) case Type::CLASS: DISPATCH(CLASS##Type);
#include "clang/AST/TypeNodes.def"
    }
    llvm_unreachable("Unknown type class!");
  }

  // If the implementation chooses not to implement a certain visit method, fall
  // back on superclass.
#define TYPE(CLASS, PARENT)                                     \
  RetTy Visit##CLASS##Type(QualType q, const CLASS##Type *T) {  \
  DISPATCH(PARENT);                                             \
}
#include "clang/AST/TypeNodes.def"

  // Base case, ignore it. :)
  RetTy VisitType(QualType q, const Type*) { return RetTy(); }
};
#undef DISPATCH


// A (qual)type visitor that checks if the given type conforms to PENCIL.
// Types rejected are pointer-carrying arrays and structs, function pointers,
// and other types that are invalid regardless of the context they appear in.
// Context-sensitive constraints, like static const restrict qualification on
// function parameters, must be checked separately.
// 
// Visit() returns true iff the type is admissible (i.e. no violations were
// detected).
// FIXME: freeze a specific format for the error messages so that it can be
// manipulated reliably.
class PencilTypeWFChecker : public QualTypeVisitor<PencilTypeWFChecker, bool> {
  ASTContext &ctx;

  void unknownType (QualType q) {
    llvm_unreachable ("Found unrecognizable type: " + q.getAsString ()
                      + " -- probably a bug in the compiler");
  }

#if 1
#define DUMP(x) llvm::errs() << #x " = " << x           \
                             << "  (" << __func__      \
                             << " @ " << __LINE__ << ")\n"
#else
#define DUMP(x) // empty
#endif

public:
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
  void addFormattedViolation (ViolationPriority p, const std::string &msg) {
    violations[p].push_back (msg);
  }

  void addViolation (ViolationPriority p, const std::string &msg) {
    addFormattedViolation (p, formatErrorMessage (msg));
  }

  bool VisitElaboratedType (QualType q, const ElaboratedType *t) {
    return Visit (t->getNamedType ());
  }

  bool VisitTypedefType (QualType q, const TypedefType *t) {
    return Visit (t->desugar ());
  }

  bool VisitPointerType (QualType q, const PointerType *t) {
    addViolation (NonPencilType,
                  std::string(t->isFunctionPointerType () ? "function " : "")
                  + "pointer type '" + q.getAsString () + "'");
    return false;
  }

  bool VisitFunctionType (QualType q, const FunctionType *t) {
    llvm_unreachable ("BUG: got to function type" + q.getAsString ());
    return false;
  }

  // Scalars are OK.
  bool VisitBuiltinType (QualType q, const BuiltinType *) { return true; }
  bool VisitComplexType (QualType q, const ComplexType *) { return true; }
  bool VisitAtomicType (QualType q, const AtomicType *) { return true; }
  bool VisitEnumType (QualType q, const EnumType *) { return true; }

  virtual bool VisitIncompleteArrayType (QualType t,
                                         const IncompleteArrayType *a) {
    addViolation (IncompleteType,
                  "unsized array type '" + t.getAsString ()
                  + "' - write 'T a[n]' using some variable 'n' if you"
                  + " want dynamic size");
    return false;
  }

  virtual bool VisitConstantArrayType (QualType t, const ConstantArrayType *a) {
    return Visit (a->getElementType ());
  }

  virtual bool VisitVariableArrayType (QualType t, const VariableArrayType *a) {
    // FIXME: Restrict to rational-function expressions.  We probably need
    // polynomial for things like matrices, while division is required for
    // scaling down an input size.
    bool ok = true;
    assert (a->getSizeExpr ());
    if (a->getSizeModifier () == ArrayType::Star) {
      addViolation (NonPencilType,
                    "invalid array type '"
                    + t.getAsString () + "', size"
                    + " must be an arithmetical expression, possibly"
                    + " referencing variables");
      ok = false;
    } else if (a->getSizeExpr ()->hasNonTrivialCall (ctx)) {
      addViolation (NonPencilType,
                    "invalid array type '"
                    + t.getAsString () + "' -  size must be"
                    + " an arithmetical expression, possibly referencing"
                    + " variables");
      ok = false;
    }
    return Visit (a->getElementType ()) && ok;
  }

  bool VisitStructureType (QualType q, const RecordType *t) {
    bool ok = true;
    RecordDecl *decl = t->getDecl ();
    assert (decl);

    for (RecordDecl::field_iterator i = decl->field_begin ();
         i != decl->field_end ();
         ++i)
      ok = Visit (i->getType ()) && ok;
    return ok;
  }

  bool VisitUnionType (QualType q, const RecordType *t) {
    addViolation (NonPencilType, "union type '" + q.getAsString () + "'");
    return false;
  }

  bool VisitRecordType (QualType q, const RecordType *t) {
    if (t->isStructureType ())
      return VisitStructureType (q, t);
    if (t->isUnionType ())
      return VisitUnionType (q, t);
    llvm_unreachable ("Found unrecognizable type: " + q.getAsString ()
                      + " -- probably a bug in the compiler");
  }

  bool VisitTagType (QualType q, const TagType *t) {
    abort ();
  }

  // Catch-all
  bool VisitType (QualType q, const Type *T) {
    llvm_unreachable ("Found unrecognizable type: " + q.getAsString ()
                      + " -- probably a bug in the compiler");
  }

  const std::deque<std::string> *getTopPriorityViolations () const {
    if (violations.empty ())
      return NULL;
    return &violations.rbegin ()->second;
  }

  PencilTypeWFChecker (ASTContext &ctx_,
                       std::string (*_formatErrorMessage) (const std::string &))
    : ctx (ctx_), formatErrorMessage (_formatErrorMessage) {
  }
};


// Checks PENCIL violations in a single function parameter declaration.  The
// outermost array/pointer dimension is handled specially, so the TypeWFChecker
// should only be used to check "deep" constituent types.  This class is mostly
// a wrapper that implements this special-casing.
class FunctionParamChecker
  : public QualTypeVisitor<FunctionParamChecker, bool> {
  PencilTypeWFChecker deep_checker;
  ParmVarDecl &decl;
  ASTContext &ctx;

public:
  static std::string formatDeepError (const std::string &msg) {
    return "argument type contains " + msg;
  }

  virtual bool VisitArrayType (QualType t, const ArrayType *a) {
    llvm_unreachable ("BUG: unknown array type " + t.getAsString ());
  }

  virtual bool VisitFunctionPointer (QualType t, const PointerType *p) {
    deep_checker.addFormattedViolation (PencilTypeWFChecker::NonPencilType,
                                        "function pointers not allowed");
    return false;
  }

  virtual bool VisitPointerType (QualType t, const PointerType *p) {
    if (p->isFunctionPointerType ())
      return VisitFunctionPointer (t, p);

    // A pointer type may be a genuine pointer or array coerced to pointer.
    // The original designation can be recovered from the declaration.
    QualType origType = decl.getOriginalType ();

    // For some reason, getOriginalType() omits qualifiers from the outermost
    // dimensions, so we need to grab those from t.
    bool missingConst = ! t.isConstQualified ();
    bool missingRestrict = ! t.isRestrictQualified ();
    bool missingStatic = false;
    bool isArray;

    if ((isArray = origType->isArrayType ())) {
      const ArrayType *a = ctx.getAsArrayType (origType);

      // Missing qualifiers are coalesced into a single report and reported
      // here, including missing static.  Size problems are reported
      // separately as part of the requirement that the whole type be a valid
      // PENCIL type, i.e. in the deep checker.
      if (a->getSizeModifier () != ArrayType::Static)
        missingStatic = true;
    }

    if (missingStatic || missingConst || missingRestrict) {
      std::string msg = std::string (isArray ? "array" : "pass-by-pointer")
        + " argument must be qualified"
        + (isArray ? " static" : "")
        + " const restrict (missing";
      if ((!isArray || missingStatic) && missingConst && missingRestrict)
        msg += isArray ? " all three" : " both";
      else {
        if (missingStatic) msg += " static";
        if (missingConst) msg += " const";
        if (missingRestrict) msg += " restrict";
      }
      msg += ")";
      deep_checker.addFormattedViolation (PencilTypeWFChecker::IncompleteType,
                                          msg);
    }

    return deep_checker.Visit (isArray ? origType : p->getPointeeType ());
  }

  virtual bool VisitType (QualType t, const Type *) {
    return deep_checker.Visit (t);
  }

  void report (DiagnosticsFormatter &formatter) const {
    const std::deque<std::string> *violations
      = deep_checker.getTopPriorityViolations ();
    if (! violations)
      return;

    for (std::deque<std::string>::const_iterator i = violations->begin ();
         i != violations->end ();
         ++i)
      formatter (DiagnosticsEngine::Error,
                 decl.getLocStart (),
                 "PENCIL violation: " + *i);
  }

  bool hasErrors () const {
    return deep_checker.getTopPriorityViolations () != NULL;
  }

  FunctionParamChecker (CompilerInstance &CI, ParmVarDecl &_decl)
    : deep_checker (CI.getASTContext (), formatDeepError),
      decl (_decl), ctx (CI.getASTContext ()) {
    Visit (decl.getType ());
  }
};

class FunctionRetTypeChecker
  : public QualTypeVisitor<FunctionRetTypeChecker, bool> {
  PencilTypeWFChecker deep_checker;
  const SourceLocation loc;

  static std::string formatDeepError (const std::string &msg) {
    return "return type contains " + msg;
  }

  void report (DiagnosticsFormatter &formatter) const {
    const std::deque<std::string> *violations
      = deep_checker.getTopPriorityViolations ();
    if (!violations)
      return;

    for (std::deque<std::string>::const_iterator i = violations->begin ();
         i != violations->end ();
         ++i)
      formatter (DiagnosticsEngine::Error,
                 loc, "PENCIL violation: " + *i);
  }

public:

  virtual bool VisitArrayType (QualType q, const ArrayType *a) {
    // Clang already complains about array return types.
    return true;
  }

  // There's no implicit array->pointer conversion in return types, so if we
  // get here then we really have a pointer.
  virtual bool VisitPointerType (QualType q, const PointerType *p) {
    deep_checker.addFormattedViolation (PencilTypeWFChecker::NonPencilType,
                                        "return type must be scalar or struct");
    return false;
  }

  virtual bool VisitType (QualType q, const Type *) {
    return deep_checker.Visit (q);
  }

  FunctionRetTypeChecker (DiagnosticsFormatter &d, SourceLocation _loc,
                          QualType rettype)
    : deep_checker (d.getCompilerInstance ().getASTContext (), formatDeepError),
      loc (_loc) {
    if (! Visit (rettype))
      report (d);
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
      if (FunctionDecl *fun = dyn_cast<FunctionDecl>(D)) {
        // Perform PENCIL-specific type checks only for the first declaration.
        if (fun->isFirstDeclaration ())
          {
            FunctionRetTypeChecker retcheck (diagnostics, fun->getLocStart (),
                                             fun->getResultType ());
            for (FunctionDecl::param_iterator param = fun->param_begin ();
                 param != fun->param_end ();
                 ++param) {
              FunctionParamChecker check (CI, **param);
              if (check.hasErrors ())
                check.report (diagnostics);
            }
          }

        diagnostics (DiagnosticsEngine::Note,
                     D->getLocStart (),
                     "");
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

static FrontendPluginRegistry::Add<pencil::PencilCompilerAction>
X("pencilc", "PENCIL compilation frontend");
