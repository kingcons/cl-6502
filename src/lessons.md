# Lessons Learned - CPU Emulation

## What must be fast

3 things absolutely must be fast: opcode dispatch, memory read/write, and status bit handling. CPUs only do a few things and most of them your language already knows how to make fast; arithmetic and assignment, in particular. Practically every CPU instruction deals with memory somehow, updates the status register, or both. Think carefully about how to map these operations onto your language. They should happen a few million times a second. :)

## High-level emulation is viable.

High-level emulation *can* work and the result is great. On the other hand, it entails _extra_ work mapping the low-level hardware to your high-level language back to reasonably fast code. This is a time-consuming process.

## Profile with intuition.

Your language profiler can't show you architectural issues, only hotspots. I used SBCL's statistical profiler a good bit on this project and regard it highly. It tracks both CPU usage and memory allocation/consing very well. However, most of the really big gains came from some mixture of intuition, experimentation, and good old hard thinking.

## Get the API right first.

Consequently, getting the API right is the hard part and the most important. If you've defined your interfaces correctly, you can rewrite the underlying data representations or execution strategies to achieve good performance.

# Lessons Learned - Common Lisp

## Structures can be preferable to classes.

Structures are much more static than classes. They also enforce their slot types. When you have a solid idea of the layout of your data and really need speed, they're ideal.

## CLOS is fast enough.

CLOS, for single-dispatch at least, is really quite fast. When I redesigned the emulator to avoid a method call for every memory read/write, my benchmark only ran ~10% faster. I eventually chose to stick with the new scheme for several reasons, performance was only a minor factor.

## Eval-when is about *data* more than code.

That is, the times I found myself using it always involved computing data at compile-time that would be stored or accessed at load-time or later. E.g. I used it to ensure that the status-bit enum was created for use by `set-flags-if`, the mode-clauses variable was bound in time for `defaddress`, and the opcodes arrays were populated before execution.

## Apply is more expensive than you might think.

My second big speedup came from storing function objects (i.e. lambdas) instead of symbols to pass to apply. We avoid the indirection of looking up a function in the current package and the indirection of packing and unpacking the function arguments in a list.
