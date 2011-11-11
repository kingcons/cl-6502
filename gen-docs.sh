#!/bin/sh
sbcl --eval "(ql:quickload '(cl-6502 sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :6502 \"docs/index.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
