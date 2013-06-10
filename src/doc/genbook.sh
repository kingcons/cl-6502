#!/bin/bash

# This shell scripts generates the top-level Markdown structure of the
# cl-6502 book.
#
# The authors list is automatically generated from Git history,
# ordered from most to least commits.
# TODO: Add jit.lisp later?

cat <<EOF
% cl-6502
% $(git log --pretty=format:%an | \
        grep -v -e '^root$' | \
        sort | uniq -c | sort -nr | sed 's/^[0-9 ]*//' | \
        awk 'NR > 1 { printf("; ") } { printf("%s", $0) } END { print("") }')

# Introduction
$(cat ../intro.md)

# CPU
$(cat ../cpu.md)
$(cat obj/cpu.lisp.md)

# Addressing Modes
$(cat ../addressing.md)
$(cat obj/addressing.lisp.md)

# Opcode Emulation
$(cat ../opcodes.md)
$(cat obj/opcodes.lisp.md)

# Exceptional Conditions
$(cat ../conditions.md)
$(cat obj/conditions.lisp.md)

# Stepping and Execution
$(cat ../utils.md)
$(cat obj/utils.lisp.md)

# Taking Apart Old Code
$(cat ../disassemble.md)
$(cat obj/disassemble.lisp.md)

# Creating New Code
$(cat ../assemble.md)
$(cat obj/assemble.lisp.md)

# Wrapping things up...
$(cat ../packages.md)
$(cat obj/packages.lisp.md)

# Conclusion
$(cat ../outro.md)

EOF
