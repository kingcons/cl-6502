## `opcodes.lisp`: Enter defasm

### References:
* [Opcodes](http://www.obelisk.demon.co.uk/6502/reference.html)
* [Py65](https://github.com/mnaberez/py65/blob/master/src/py65/devices/mpu6502.py)

Now comes the core of the project, the CPU opcode definitions. The 6502 has 56
instructions, each with several addressing modes. We'll use our `defasm` macro
to define each instruction with all its variants in one place. The variants are
given as a list of lists where each variant is specified like so:

`(opcode-byte cycles bytes addressing-mode)`

The `opcode-byte` is the how the opcode is stored in memory, the `cycles` are
how long it takes the CPU to execute, and the `bytes` are how many bytes in RAM
the opcode *and* its arguments use.

You might have wondered why in `defasm` we increment the Program Counter by 1,
then *later* check `track-pc` before incrementing the rest. Imagining the opcode's
execution makes it obvious. The PC points at the opcode, then we increment the
program counter to point at the first argument. The instruction BODY can run
without worrying about argument offsets. Afterwards, if there were no arguments,
we're already pointing at the next instruction. Otherwise, we just bump the PC
past the arguments.

If you were reading carefully earlier, you noticed that we wrap the body of
`defasm` in a FLET that defines the `getter` and `setter` functions to access
memory for the opcode's addressing mode. It's advantageous for performance to
compute as much as possible at compile-time, so we have `make-getter` compute
a custom body for the GETTER in `defasm`.

`make-getter` is there only because, unlike **all** the other instructions,
shifts and rotations (i.e. ASL, LSR, ROL, and ROR), use "raw" addressing in
their accumulator mode but "normal" addressing everywhere else. Consequently,
those instructions are the only place it is used. Aren't you glad I already
hunted those bugs down?

## Source Code
