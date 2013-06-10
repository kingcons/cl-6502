## `addressing.lisp`: The Addressing Mode Protocol

### References:
* [Addressing Modes](http://www.obelisk.demon.co.uk/6502/addressing.html)

Addressing Modes are perhaps *the* major difference between high-level languages
and assembly. In practically every high-level language, you define data with
variables and then access those variables directly. In assembly, you store data
at memory addresses and interact with the addresses only. You can't, however,
just pass any address to any CPU instruction.

Each CPU instruction supports a limited number of Addressing Modes. Different
Addressing Modes get data from different places in memory. Each Addressing Mode
has a different syntax which we'll want to define readers and writers for to aid
with assembly and disassembly. You'll note when we start defining opcodes that
certain addressing modes take more CPU time than others. On many older systems,
a large part of writing fast assembly is figuring out how to store your
program's data so you can use the fastest Addressing Mode possible to get it.

The 6502 has 13 Addressing Modes that can be roughly divided into 5 groups based
on what part of memory they access:

1. Basic: CPU registers - implied, immediate, accumulator
2. Zero-Page: The bottom 256 bytes of RAM - zero-page, zero-page-x, zero-page-y
3. Absolute: Any address in RAM - absolute, absolute-x, absolute-y
4. Indirect: The address stored at another address - indirect, indirect-x, indirect-y
5. Relative: Conditionally go forwards or backwards - relative

We'll implement the Addressing Modes through a protocol on unique symbols rather
than custom classes. We can define methods on specific symbols using Lisp's
fantastic [EQL Specialized Methods](http://cl-cookbook.sourceforge.net/clos-tutorial/#section-4.5).
We need to know how to parse, print, get, and set each mode. The parser will be
a simple regular expression to extract a number, the printer is a lisp format
string that effectively inverts the process. Since the getter and setter will
be working on the same address in memory, we'll use a `defaddress` macro so they
can share as much code as possible.

## Source Code
