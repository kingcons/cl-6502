#!/bin/sh
sbcl --eval "(ql:quickload '(cl-6502 sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :cl-6502 \"docs/cl-6502.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
