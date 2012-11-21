pencilc
=======

This is a prototype compiler for the PENCIL language.

How to build
------------

Unfortunately, clang's plugin documentation seems to be premature and I
couldn't find accessible instructions on how to compile a plugin.  The only way
I know is to stick this into clang's tools/ directory and build it with all the
rest of the clang source tree.  I don't know much about cmake, much less how
clang uses it, so any suggestions on how to minimize intrusiveness would be
welcome.

Let the root of clang's source tree be $clang.  Then:

  - Move the directory containing this file (the file you're reading right now)
    into $clang/tools so that its path is $clang/tools/pencilc.

  - Add this line to $clang/tools/CMakeLists.txt:

      add_subdirectory(pencilc)

  - Build the entire clang source as usual.  You shouldn't have to do a `clean'
    if you have already built clang before.


How to run
----------

Once the plugin is built, you can run it on a PENCIL-conformant C file using:

Linux:
$ clang -cc1 -load $install_dir/lib/PencilCompiler.so -plugin pencilc some-input-file.c

Mac:
$ clang -cc1 -load $install_dir/lib/PencilCompiler.dylib -plugin pencilc some-input-file.c

where $install_dir is your install path for clang.  You can replace it by the
build directory if you prefer not to install clang.

The compiler will check if the input file conforms to PENCIL coding rules, and
compiles it to an object file (FIXME: right now only type checking is performed,
not the translation).


Testing
-------

Tests can be invoked by going to your build directory's tools/pencilc/test and
typing

  ctest

if you're using a make-based build.  If you're using something else, I have no
idea.  The development is partly test-driven, so don't be surprised to
occasionally see a small number of tests to fail.  If *most* of them fail, then
there's something wrong.

Test cases are collected into tools/pencilc/test/ill-formed and
tools/pencilc/test/well-formed; the former contains programs that should not
compile and the latter contains those which should.  Every file with a .c
extension in one of these directories is registered automatically when you run
cmake.  BUT, cmake doesn't notice changes to these directories automatically,
so you have to go to build root and manually re-run the stupid command `cmake'
yourself.  If you know how to fix this madness, I'd really like to know.



Known Issues
------------

There are some known issues which will eventually be fixed but not any time
soon:

  - The command, as you can see, is annoyingly long.  Gotta have a wrapper script.

  - The checker makes no distinction between C functions and PENCIL functions, much less between C code and PENCIL regions.  All functions are assumed to be PENCIL, but this should change.

  - Clang can't find standard header files with the command given above.  You
    need to explicitly give the include directory, like so:

      clang -cc1 -I /usr/lib/clang/3.2/include -load /usr/lib/PencilCompiler.so -plugin -pencilc foo.c

  - Right now this is no compiler, it's just a checker.  I probably have to
    make my own Driver instance and make it a standalone tool to make it a real
    compiler that emits real object files.
