## `conditions.lisp`: Just in Case

There's really not much to say about our
[conditions](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html).
The primary illegal state our emulator can encounter is trying to execute an
invalid opcode, i.e. a byte for which no opcode definition exists. For our
assembler, we have also defined a condition for invalid syntax. Finally,
there is a condition for invalid addressing modes, mostly to provide better
debugging in case a new opcode definition was fat-fingered.

## Source Code
