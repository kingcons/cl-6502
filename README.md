# cl-6502, or The 6502...IBM 704 edition ;)

cl-6502 is a Common Lisp emulator, disassembler and (soon) assembler for the
[MOS 6502 processor](http://en.wikipedia.org/wiki/MOS_Technology_6502).
In case that sounds weird to you, the MOS 6502 is famous for its use in...
the [Apple II](http://en.wikipedia.org/wiki/Apple_II_series),
the [original NES](http://en.wikipedia.org/wiki/Nintendo_Entertainment_System),
the [Commodore 64](http://en.wikipedia.org/wiki/Commodore_64),
and [Michael Steil's phenomenal talk](http://media.ccc.de/browse/congress/2010/27c3-4159-en-reverse_engineering_mos_6502.html) at 27C3.

A few notes on why I'm doing this are [here](http://redlinernotes.com/blog/?p=1421). Some notes on the design of cl-6502 are [here](http://redlinernotes.com/blog/?p=1428).

## Install
Just git clone this library and make sure the resulting path is on your ```asdf:*central-registry*``` list. Hopefully, this will be in Zach Beane's positively delightful [quicklisp](http://quicklisp.org/) "real soon now". Start your lisp and then:

* Using ASDF: ```(asdf:oos 'asdf:load-op 'cl-6502)```
* Using Quicklisp (coming soon): ```(ql:quickload 'cl-6502)```

To run the tests, after you've loaded *cl-6502* just run ```(asdf:oos 'asdf:test-op 'cl-6502)```. There is a dearth of tests at the moment but there will be more soon as the design has recently solidified.

## Getting Started
* Check out the docs for the [cl-6502](http://redlinernotes.com/docs/cl-6502.html) and [6502-cpu](http://redlinernotes.com/docs/6502-cpu.html) packages.
* Play around at the REPL!
* Use it to create your own wacky code artifacts. (NOTE: As the 6502 package shadows *BIT* and *AND*, you're hereby advised not to *:use* it in any other packages.)

In particular, [disasm](http://redlinernotes.com/docs/6502-cpu.html#disasm_func), [execute](http://redlinernotes.com/docs/6502.html#execute_func), [6502-step](http://redlinernotes.com/docs/6502.html#6502-step_func), and [reset](http://redlinernotes.com/docs/6502-cpu.html#reset_func) are likely of interest.
