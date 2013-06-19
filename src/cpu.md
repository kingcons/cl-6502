## `cpu.lisp`: The 6502 VM

### References:
* [General](http://nesdev.parodius.com/6502.txt)
* [Registers](http://www.obelisk.demon.co.uk/6502/registers.html)

Loosely, the idea of `cpu.lisp` is to define a simple VM the rest of the emulator's
behavior is defined in terms of. The first thing to do is construct a model
of the CPU's state. Like all CPUs, the 6502 has a Program Counter pointing
to the next instruction to execute. The Program Counter (or PC) is 16 bits,
meaning the 6502 can address 64k of RAM. It also has Stack Pointer, Accumulator,
X, and Y registers each of which is a single byte. There is a Status Register
with information about the result of the last computation. For emulation purposes,
we'll also add a cycle counter to track CPU execution time.

Once data structures for the CPU and RAM are defined, we'll want helper functions
for the rest of the emulator to transform them. Common operations we'll want to
support include:

* Getting and setting a byte/word in RAM.
* Pushing and popping items off the stack.
* Getting and setting individual bits in the status register.
* Resetting the state of the system.
* Executing an [NMI](http://en.wikipedia.org/wiki/Non_maskable_interrupt).

We'll also add a few things to help ensure the correctness of our emulator:

* Wrappers to ensure that bytes and words don't overflow.
* Type definitions for a byte (u8) and machine word (u16).
* A macro to simplify the definition of opcodes.

Most other things we'll need to define our opcodes will be built in language
primitives like arithmetic, conditionals, etc.

## Source Code
