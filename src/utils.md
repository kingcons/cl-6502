## `utils.lisp`: Bringing it all Together

Now we need to actually connect the dots and get our CPU emulating! An `EXECUTE`
function that emulates code in a loop until a halt instruction and a `STEP-CPU`
function to execute a single opcode suit nicely. Here we see that our opcodes
array finally gets put to good use as `STEP-CPU` grabs the appropriate function
and its metadata from the array and calls it, raising an error if needed.

## Source Code
