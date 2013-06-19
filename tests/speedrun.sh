#!/bin/sh
sbcl --eval "(ql:quickload 'cl-6502-tests)" \
     --eval "(in-package :6502-tests)" \
     --eval "(progn (speedrun) (sb-ext:quit))"
