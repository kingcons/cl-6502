# cl-6502, or The 6502...IBM 704 edition ;)

cl-6502 is a Common Lisp emulator, assembler and disassembler for the
[MOS 6502 processor](http://en.wikipedia.org/wiki/MOS_Technology_6502).
In case that sounds weird to you, the MOS 6502 is famous for its use in...

* the [Apple II](http://en.wikipedia.org/wiki/Apple_II_series),
* the [original NES](http://en.wikipedia.org/wiki/Nintendo_Entertainment_System),
* the [Commodore 64](http://en.wikipedia.org/wiki/Commodore_64),
* the [BBC Micro](http://en.wikipedia.org/wiki/BBC_Micro),
* and [Michael Steil's phenomenal talk](http://media.ccc.de/browse/congress/2010/27c3-4159-en-reverse_engineering_mos_6502.html) at 27C3.

I gave a talk on cl-6502 and related ideas which is online [here](http://vimeo.com/47364930). The slides are available separately [here](http://redlinernotes.com/docs/talks/opa.html). A few notes on why I'm writing it are [here](http://blog.redlinernotes.com/posts/On-Interactive-Retrocomputing.html) and minor notes on the design are [here](http://blog.redlinernotes.com/posts/An-Emulator-Design-Pattern.html).

## Install
You are strongly encouraged to use this library via [Quicklisp](http://quicklisp.org/). Simply start your lisp and run: ```(ql:quickload 'cl-6502)```.

## Getting Started
* Check out the docs for the [*cl-6502*](http://redlinernotes.com/docs/cl-6502.html) package or have a look on [quickdocs](http://quickdocs.org/cl-6502).
* Play around at the REPL!
* Use it to create your own wacky code artifacts.
* There is also a lower-level *6502* package if you really want to get your hands dirty. NOTE: The 6502 package shadows `BIT` and `AND` so you likely don't want to `:use` it in your own packages.

In particular, [asm](http://redlinernotes.com/docs/cl-6502.html#asm_func), [disasm](http://redlinernotes.com/docs/cl-6502.html#disasm_func), [execute](http://redlinernotes.com/docs/cl-6502.html#execute_func), [6502-step](http://redlinernotes.com/docs/cl-6502.html#6502-step_func), and [reset](http://redlinernotes.com/docs/cl-6502.html#reset_func) are likely of interest.

### A simple example:

1. Load cl-6502 and switch to the `cl-6502` package.
2. Write some 6502 code and run it through ```asm``` (e.g. ```(asm "brk")```) to get a bytevector to execute.
3. Load it into memory and run it with ```(execute *cpu* *my-bytevector*)``` OR
 1. Load it with ```(setf (get-range 0) *my-bytevector*)```
 2. Set the program counter to 0 with ```(setf (6502:cpu-pc *cpu*) 0)```
 3. Manually step through it with ```(6502-step *cpu* (get-byte (6502:immediate *cpu*)))```
4. ```(reset)``` the CPU as necessary and keep hacking! :)

### Supported Assembler Syntax
There are sexp-based and string-based assemblers, both invoked via `asm`. The string-based assembler expects statements to be separated by newlines. The sexp-based assembler expects each statement to be in its own list. Disassembling to both formats is supported via `disasm` and `disasm-to-list`. Comments are supported. ';' is treated as comment to end-of-line in the string assembler.

```
| Addressing Mode | SEXP-based format | String format  |
|-----------------|-------------------|----------------|
|   Implied       |  (:brk)           | "brk"          |
|   Immediate     |  (:lda :#$00)     | "lda #$00"     |
|   Accumulator   |  (:rol :a)        | "rol a"        |
|   Zero-page     |  (:lda :$03)      | "lda $03"      |
|   Zero-page, X  |  (:lda :$03.x)    | "lda $03, x"   |
|   Zero-page, Y  |  (:ldx :$03.y)    | "ldx $03, y"   |
|   Absolute      |  (:sbc :$0001)    | "sbc $0001"    |
|   Absolute, X   |  (:lda :$1234.x)  | "lda $1234, x" |
|   Absolute, Y   |  (:lda :$1234.y)  | "lda $1234, y" |
|   Indirect      |  (:jmp :@1234)    | "jmp ($1234)   |
|   Indirect, X   |  (:lda :@12.x)    | "lda ($12), x" |
|   Indirect, Y   |  (:lda :@34.y)    | "lda ($34), y" |
|   Relative      |  (:bne :&fd)      | "bne &fd"      |
```

## Hacking

* Using Quicklisp: For local development, git clone this repository into the ```local-projects``` subdirectory of quicklisp.

To run the tests, after you've loaded *cl-6502* just run ```(asdf:oos 'asdf:test-op 'cl-6502)```. You may need to ```(ql:quickload 'cl-6502-tests)``` to ensure that the fiveam dependency is satisfied first.

## License

The code is under a BSD license except for docs/6502.txt and tests/6502_functional_test.a65 which are only present by 'mere aggregation' and not strictly part of my sources.
