## `parser.lisp`: String processing

## References:
[ca65 syntax](http://www.cc65.org/doc/ca65-4.html)

The parser converts source code in string format to intermediary instructions.
The syntax is simple, following the standard 6502 style, except without any
directives. A single label can contain any of the following: a label, an opcode,
an operand, and a comment.

Operands have different syntax depending upon the addressing mode, which include
implied, immediate, accumulator, zero-page, absolute, relative, indirect, and
indexing using either the X or Y register. These address modes are parsed
using the regular expressions from addressing.lisp.

The actual value from the operand is still handled by the parser, and can be
represented using decimal, or hexidecimal (if preceded by "$" a dollar sign),
or binary (if preceded by "%" a percent sign), or can be a label, or a simple
expression consisting of some other term plus ("+") another expression.

The parser returns a list of instructions, with each slot set to the appropriate
fields from the assembly statement. There may be more than one address-mode in
such instructions, since the syntax is ambiguous.

## Source Code
