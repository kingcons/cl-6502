## `assemble.lisp`: This is only the Beginning

### References:
* [A port of Henry Baker's comfy-6502](http://josephoswald.nfshost.com/comfy/summary.html)
* [Fun with Lisp: Programming the NES](http://ahefner.livejournal.com/20528.html)

Since our disassembler can disassemble to either a string or sexp format, we'd
like our assembler to be similarly versatile. Therefore, we'll define a Generic
Function `asm` that works on lists or strings. In addition it takes an optional
environment as a hash table, and a starting address.

The assembler works by looping over its input, recording any labels, and
assembling each statement one at a time, using delayed functions for any
statements that use a label. Then finally, delayed functions are resolved, and
all the results are concatenated together. Both lists and strings are converted
into an intermediary format, a struct called instruction, which represents
a single statement.

The parser determines which possible address modes can match the given syntax,
which may be ambiguous due to labels, and matches these modes against what the
given opcode can use.

## Source Code
