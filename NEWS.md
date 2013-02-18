## Changes for 0.8.6 (2013-02-xx):

*
* Improved readability of generated code from defopcode/defins.
* Improve assembler error reporting.
* Fix assembler PC tracking bug when using labels.
* General refactoring and code cleanups.

## Changes for 0.8.5 (2012-07-22):

* Assembler supports forward references/jumps.
* Assembler detects addressing mode of a label based on call site, not definition.
* Fix PC tracking in assembler.
* Improve assembler unit tests.

## Changes for 0.8 (2012-07-14):

* Add assembler with comment and label/var support.
* Miscellaneous improvements to docs and unit tests.

## Changes for 0.7.5 (2012-07-11):

* Huge refactor to status bit handling, correctness improved.
* Improve correctness of ADC and SBC instructions.

## Changes for 0.7.2 (2012-06-24):

* Switched to BSD license.
* Completed addressing mode unit tests.
* Fixed load-time defopcode bug with eval-when.
* Fleshed out README.

## Changes for 0.7.1 (2012-06-17):

* Disassembler correctly formats output based on addressing mode.
* Add API docs.

## Changes for 0.7 (2012-06-16):

* Export 6502-step, execute.
* Improved cycle counting.
* Bugfixes to PC tracking (jsr, jmp, rts, relative mode).

## Changes for 0.6.5 (2012-06-11):

* Add disassembler.
* Bugfixes and cleanups.

## Changes for 0.5 (2012-06-10):

* Initial release.
