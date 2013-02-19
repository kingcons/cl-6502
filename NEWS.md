## Changes for 0.8.6 (2013-02-xx):

* Added NMI support.
* Fixed incorrect carry bit handling in CMP,CPX,CPY.
* Fixed incorrect jumps when using JSR opcode.
* Improved assembler error reporting.
* Improved readability of generated code from defopcode/defins.
* Fixed assembler PC tracking bug when using labels.
* General refactoring and code cleanups.

## Changes for 0.8.5 (2012-07-22):

* Assembler supports forward references/jumps.
* Assembler detects addressing mode of a label based on call site, not definition.
* Fixed PC tracking in assembler.
* Improved assembler unit tests.

## Changes for 0.8 (2012-07-14):

* Added assembler with comment and label/var support.
* Miscellaneous improvements to docs and unit tests.

## Changes for 0.7.5 (2012-07-11):

* Huge refactor to status bit handling, correctness improved.
* Improved correctness of ADC and SBC instructions.

## Changes for 0.7.2 (2012-06-24):

* Switched to BSD license.
* Completed addressing mode unit tests.
* Fixed load-time defopcode bug with eval-when.
* Fleshed out README.

## Changes for 0.7.1 (2012-06-17):

* Disassembler correctly formats output based on addressing mode.
* Added API docs.

## Changes for 0.7 (2012-06-16):

* Export 6502-step, execute.
* Improved cycle counting.
* Bugfixes to PC tracking (jsr, jmp, rts, relative mode).

## Changes for 0.6.5 (2012-06-11):

* Added disassembler.
* Bugfixes and cleanups.

## Changes for 0.5 (2012-06-10):

* Initial release.
