## `opcodes.lisp`: Enter defasm

### References:
* [Opcodes](http://www.obelisk.demon.co.uk/6502/reference.html)
* [Py65](https://github.com/mnaberez/py65/blob/master/src/py65/devices/mpu6502.py)

Now comes the core of the project, the CPU opcode definitions. The 6502 has 56
instructions, each with several addressing modes. We'll use our `defasm` macro
to define each instruction with all its variants in one place. When the function
is called it will be passed the symbol naming its addressing mode, so the getter
and setter calls will dispatch properly. The variants for each instruction are
given in a list of lists where each variant is specified like so:
`(opcode-byte cycles bytes addressing-mode)`

The `opcode-byte` is the how the opcode is stored in memory, the `cycles` are
how long it takes the CPU to execute, and the `bytes` are how many bytes in RAM
the opcode *and* its arguments use.

## Source Code
