## `addressing.lisp`: The Addressing Mode Protocol

### References:
* [Addressing Modes](http://www.obelisk.demon.co.uk/6502/addressing.html)

Addressing Modes are perhaps *the* major difference between high-level languages
and assembly. In practically every high-level language, you define named
variables to store your data in and then access your data by name. In assembly,
you store data at a memory address and interact with the address only. To
interact with a given address, you use Addressing Modes.

Conceptually, an addressing mode is just a function that takes a number and
returns an index into memory. Each addressing mode indexes into memory
differently and comes with a unique syntax which we'll define readers and
writers for to aid with assembly and disassembly.

The 6502 has 13 addressing modes that can be roughly divided into 5 groups based
on what part of memory they access:

1. Basic: CPU registers - implied, immediate, accumulator
2. Zero-Page: The bottom 256 bytes of RAM - zero-page, zero-page-x, zero-page-y
3. Absolute: Any address in RAM - absolute, absolute-x, absolute-y
4. Indirect: The address stored at another address - indirect, indirect-x, indirect-y
5. Relative: Conditionally go forwards or backwards - relative

To make things more complicated, you can't just pass any address to any CPU
instruction. Each CPU instruction supports a limited number of addressing modes.
You'll note when we start defining opcodes that certain addressing modes take
more CPU time than others. On many older systems, a large part of writing fast
assembly code is figuring out how to layout your program's data in memory so
you can use the fastest addressing mode possible to access it.

A functioning emulator needs to know how to parse assembly, print disassebly, get data,
and set data for each addressing mode. **cl-6502** uses
[methods on unique symbols](http://cl-cookbook.sourceforge.net/clos-tutorial/#section-4.5)
for parsing and printing, and functions for getting and setting. The parser
will be a simple regular expression to extract a number, the printer is a lisp
format string that effectively inverts the process. Since the getter and setter
need to work on the same address in memory, and use the same code to compute
that address, we'll use a `defaddress` macro to factor out their shared code.

## Source Code
