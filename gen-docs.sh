#!/bin/sh
sbcl --eval "(ql:quickload '(cl-6502 sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :cl-6502 \"docs/cl-6502.html\")" \
     --eval "(cl-api:api-gen :6502-cpu \"docs/6502-cpu.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
