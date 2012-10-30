ENABLE_TESTING()
file(GLOB ILL_FORMED ill-formed/*.c)
file(GLOB WELL_FORMED well-formed/*.c)

set(PENCILC ${PROJECT_BINARY_DIR}/bin/clang -cc1
              -load $<TARGET_FILE:PencilCompiler>
              -plugin pencilc
              -I ${PROJECT_BINARY_DIR}/include)

foreach (f ${ILL_FORMED})
  add_test (NAME ${f} COMMAND ${PENCILC} ${f})
  set_property (TEST ${f} PROPERTY WILL_FAIL TRUE)
endforeach (f)

foreach (f ${WELL_FORMED})
  add_test (NAME ${f} COMMAND ${PENCILC} ${f})
endforeach (f)
