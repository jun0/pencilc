This is a prototype compiler for PENCIL.  Unfortunately, clang's plugin
documentation seems to be premature and I couldn't find accessible instructions
on how to compile a plugin.  The only way I know is to stick this into clang's
tools/ directory and build it with all the rest of the clang source tree.  I
don't know much about cmake, much less how clang uses it, so any suggestions on
how to minimize intrusiveness would be welcome.

Let the root of clang's source tree be $clang.  Then:

  - Move the directory containing this file (the file you're reading right now)
    into $clang/tools so that its path is $clang/tools/pencilc.

  - Add this line to $clang/tools/CMakeLists.txt:

      add_subdirectory(pencilc)

  - Build the entire clang source as usual.  You shouldn't have to do a `clean'
    if you have already built clang before.

Once the plugin is built, you can run it using:
--
Linux:
$ clang -cc1 -load <install-dir>/lib/PrintFunctionNames.so -plugin pencilc some-input-file.c

Mac:
$ clang -cc1 -load <install-dir>/lib/PrintFunctionNames.dylib -plugin pencilc some-input-file.c
--

Tests can be invoked by going to your build directory's tools/pencilc/test and
typing

  ctest

if you're using a make-based build.  If you're using something else, I have no
idea.  The development is partly test-driven, so don't be surprised to see a
small number of tests to fail.  If *most* of them fail, then there's something
wrong.

Test cases are collected into tools/pencilc/test/ill-formed and
tools/pencilc/test/well-formed; the former should contain programs that should
not compile and the latter should contain those which should.  Every file with
a .c extension in one of these directories is registered automatically when you
run cmake.  BUT, cmake doesn't notice this automatically, so you have to go to
build root and manually re-run the stupid command yourself.  How do I fix this
madness?




There are some known issues which will eventually be fixed but not any time
soon:

  - The command, as you can see, is annoyingly long.  Gotta have a wrapper script.

  - Clang can't find standard header files with the command given above.  You
    need to explicitly give the include directory, like so:

      clang -cc1 -I /usr/lib/clang/3.2/include -load /usr/lib/PrintFunctionNames.so -plugin -pencilc foo.c


