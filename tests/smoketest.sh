#!/bin/sh
sbcl --eval "(asdf:oos 'asdf:test-op 'cl-6502)" \
     --eval "(sb-ext:quit)"
