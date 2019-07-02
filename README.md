# pl0compiler
PL/0 compiler for IBM 701, except that a print statement has been added.

Entire program is in a single file. Should compile and build with any C compiler, maybe with a few trivial adjustments. Tested only with Visual Studio.

Assembler output is in "out.a".

You'll need an actual IBM 701 or a 701 emulator to run it. No 701s exist any longer, but you can find an emulator at
basepath.com/701.

Generated assembly-language files include four other files, all present here: stack.a, self-load.a, print.a, and print-number.a. The emulator's assembler has an INCL facility that includes those files. If your 701 assembler doesn't, just include the contents of those files literally by modifying the assembler output. Also, if you're using another 701 assembler, please email me, as I'd love to know where you got it.
