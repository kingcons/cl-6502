#!/bin/bash

# This shell scripts generates the top-level Markdown structure of the
# cl-6502 book.
#
# The authors list is automatically generated from Git history,
# ordered from most to least commits.

cat <<EOF
% cl-6502
% $(git log --pretty=format:%an | \
        grep -v -e '^root$' | \
        sort | uniq -c | sort -nr | sed 's/^[0-9 ]*//' | \
        awk 'NR > 1 { printf("; ") } { printf("%s", $0) } END { print("") }')

# CPU
$(cat ../cpu.md)
## \`cpu.lisp\`: The 6502 VM
$(cat obj/cpu.lisp.md)

# Addressing Modes
$(cat ../addressing.md)
## \`addressing.lisp\`: The Addressing Mode Protocol
$(cat obj/addressing.lisp.md)

# Opcode Emulation
$(cat ../opcodes.md)
## \`opcodes.lisp\`: Enter Defopcode
$(cat obj/opcodes.lisp.md)

# Exceptional Conditions
## \`conditions.lisp\`: Just in Case
$(cat obj/conditions.lisp.md)

# Stepping and Execution
## \`utils.lisp\`: Bringing it all Together
$(cat obj/utils.lisp.md)

# Taking Apart Old Code
## \`disassemble.lisp\`: When one is Lost
$(cat obj/disassemble.lisp.md)

# Creating New Code
## \`assemble.lisp\`: Only the Beginning
$(cat obj/assemble.lisp.md)

EOF
