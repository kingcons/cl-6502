## `utils.lisp`: Bringing it all Together

Now we need to actually connect the dots and get our CPU emulating! An `EXECUTE`
function that emulates code in a loop until a halt instruction and a `STEP-CPU`
function to execute a single opcode suit nicely. Here we see that our opcodes
array finally gets put to good use as `STEP-CPU` just grabs the appropriate
lambda and calls it, leaving `EXECUTE` to handle errors if necessary.

## Source Code
