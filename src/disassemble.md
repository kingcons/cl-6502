## `disassemble.lisp`: When one is Lost

Disassembly actually isn't very tricky...at least on the 6502. Conceptually, you
just want to specify a range of bytes to be viewed as code rather than data and
print its representation. Our storage of metadata in the opcodes array makes it
trivial. We simply loop from start to end, always incrementing by the length of
the opcode rather than 1.

Disassembling a single instruction at a given index works by reading the byte,
retrieving the matching entry in the opcodes array, and either printing it using
the Addressing Mode's writer or returning a list representing the instruction.

Thus, we can disassemble to either a lispy syntax or the standard 6502 syntax in
only a few dozen lines of code. Since we've factored out disassembling a single
opcode, its easy to add a `current-instruction` helper to disassemble whatever
the CPU is about to execute as well.

## Source Code
