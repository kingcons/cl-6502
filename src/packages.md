## `packages.lisp`: Exposed Functionality

Finally, we'll want to define packages for our emulator to provide a public API.
Who knows, maybe somebody out there is just dying for a drag and drop 6502
emulator in Lisp with a function to single-step instructions. :)

The `6502` package is for use by emulator writers, the test suite, etc. It
exposes all the types and interesting high-level functionality. It also shadows
Common Lisp's built-in **AND** and **BIT** symbols since they name 6502 opcodes.
If you were reading *very* closely, you probably noticed we had a `cl:` package
prefix hiding in the `defasm` macro.

The `cl-6502` package is more limited and designed for public consumption,
hiding the Addressing Modes and their protocol, the CPU slot accessors, and
other helpers.

## Source Code
