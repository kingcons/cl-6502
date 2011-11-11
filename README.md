The 6502...IBM 704 edition. ;)
Very work in progress. Probably for a while cause I'm dumb. Don't get excited.

Thoughts on testing:
1. UGH
2. GUH
3. HUG? ... no.
4. Can't prevent style-warnings loading fiveam via asdf. DO.NOT.CARE.
5. So ql:quickload library.

Recommended way to run tests:
(ql:quickload '(cl-6502))
(asdf:oos 'asdf:test-op 'cl-6502)
