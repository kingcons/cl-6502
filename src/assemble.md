## `assemble.lisp`: This is only the Beginning

### References:
* [A port of Henry Baker's comfy-6502](http://josephoswald.nfshost.com/comfy/summary.html)
* [Fun with Lisp: Programming the NES](http://ahefner.livejournal.com/20528.html)

Since our disassembler can disassemble to either a string or sexp format, we'd
like our assembler to be similarly versatile. Therefore, we'll define a Generic
Function `asm` that works on lists or strings. The assembler will be as *naive*
as possible so that we can reuse it in making higher-level tools later.

All the assembler needs to do is loop over its input assembling each statement
one at a time, then concatenating the results together. We can even cheat by
having our string assembler first convert the statements to sexps and then reuse
the other code!

Once the code is in a lispy format, its straightforward to grab the name of the
instruction, figure out its addressing mode by using regexes on the arguments,
and then lookup the opcode based on the name and addressing mode.

## Source Code
