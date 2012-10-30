set(MODULE TRUE)

set( LLVM_LINK_COMPONENTS support mc)

add_clang_library(PencilCompiler PencilCompiler.cpp)

add_dependencies(PencilCompiler
  ClangAttrClasses
  ClangAttrList
  ClangCommentNodes
  ClangDeclNodes
  ClangDiagnosticCommon
  ClangStmtNodes
  )

target_link_libraries(PencilCompiler
  clangFrontend
  clangAST
  )

set_target_properties(PencilCompiler
  PROPERTIES
  LINKER_LANGUAGE CXX
  PREFIX "")

add_subdirectory(test)

