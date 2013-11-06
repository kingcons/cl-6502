## Changes for 0.9.6 (2013-11-11):

* Rewrite DEFADDRESS again and stop inlining get-byte. The new implementation
  is easier to read. It is also slightly slower but allows emulators
  to monkey patch get-byte to use a given platform's memory map.
* Reduce consing on SBCL > 1.1.8 due to modarith/type checking changes.
* Make debugging easier by using named-lambdas for the opcodes.
* Minor code cleanups and book clarifications.

## Changes for 0.9.5 (2013-07-05):

* Further performance improvements from type hints:
  Emulates the 6502 at ~50 mhz with SBCL 1.1.4.debian on an old Core 2 Duo.

## Changes for 0.9.4 (2013-06-19):

* Major performance improvements from rewriting:
  DEFADDRESS, DEFASM, and status-bit handling, again.
  Results in a 75% reduction in runtime on Klaus' testsuite!
* Two new chapters in the book: Lessons Learned and A Naive JIT.
* *Incompatible change*: Renamed 6502-STEP to STEP-CPU.
* *Incompatible change*: JIT-EXECUTE was removed.

## Changes for 0.9.3 (2013-06-10):

* Code written in a more literate style with a [readable book](http://redlinernotes.com/docs/cl-6502.pdf)!

## Changes for 0.9.2 (2013-05-14):

* Substantial performance improvements from rewriting:
  overflow handling for arithmetic ops (overflow-p),
  storage handling for %status-bit (defenum).
  Results in a 33% reduction in runtime and an over 90%
  reduction in garbage generated for Klaus' testsuite.
* Greatly simplified defasm macro replaces defopcode+defins.
* New Addressing Modes implementation.

## Changes for 0.9.1 (2013-04-01):

* Add a **very** naive JIT compiler. Don't expect big speedups. Do expect bugs.
* Improve error reporting on incorrect assembly.
* Remove comment and label support from assembler.
* Fix symbolic disassembly for certain addressing modes.

## Changes for 0.9.0 (2013-03-16):

* Perfect run on Klaus Dorfmann's test suite aside from decimal mode ADC/SBC.
* Fix bugs in new symbolic assembler. Indirect and *-x *-y addressing modes
  were previously not working. Extended test suite to cover these cases.
* Added and exported CURRENT-INSTRUCTION helper.
* Fix stack wraparound in PHP, PLA, etc.

## Changes for 0.8.9 (2013-03-03):

* Add symbolic assembly and disassembly, i.e. sexp-format support.

## Changes for 0.8.8 (2013-02-23):

* Massive package overhaul and docs update.
* Minor dead code removal (i.e. toy assembly progs).

## Changes for 0.8.7 (2013-02-21):

* Tweaked DEFOPCODE to fix addressing mode handling of ASL, LSR, ROL, and ROR.
* Rewrote ROL and ROR, fixing bugs.
* Emulate the infamous 6502 indirect JMP page wrapping bug.
* Fixed incorrect break and unused bit handling in PHP, PLP, and RTI.
* Fixed incorrect overflow bit handling in ADC.
* Fixed incorrect result in LSR due to extra fetch.
* Fixed carry bit handling in SBC, CMP, CPX, and CPY. Previous fix didn't correctly handle all cases.

## Changes for 0.8.6 (2013-02-20):

* Added NMI support.
* Improved assembler error reporting.
* Improved readability of generated code from defopcode/defins.
* Fixed CPU initial state based on values in NESdev wiki.
* Fixed incorrect carry bit handling in CMP,CPX,CPY.
* Fixed incorrect jumps when using JSR opcode.
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
