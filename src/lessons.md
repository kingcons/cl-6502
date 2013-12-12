# Lessons Learned - Emulation

## High-level emulation is viable

High-level emulation *can* work and the result is great. On the other hand, it
entails _extra_ work mapping the low-level hardware to your high-level language
back to reasonably fast code. This is a time-consuming process.

## What must be fast

3 things absolutely must be fast: opcode dispatch, memory reading/writing, and
status bit handling. CPUs only do a few things and most of them your language
already knows how to make fast; arithmetic and assignment, in particular.
Practically every CPU instruction deals with memory somehow, updates the status
register, or both. Think carefully about how to map these operations onto your
language. They should happen a few million times a second. :)

## Profile with intuition

Your language profiler can't show you architectural issues, only hotspots. I
used SBCL's statistical profiler a good bit on this project and regard it highly.
It tracks both CPU usage and memory allocation/consing very well. However, most
of the really big gains came from some mixture of intuition, experimentation,
and good old hard thinking.

## Get the API right first

Consequently, getting the API right is the hard part and the most important. If
you've defined your interfaces correctly, you can rewrite the underlying data
representations or execution strategies to achieve good performance.

# Lessons Learned - Common Lisp

## Structures can be preferable to classes

Structures are much more static than classes. They also enforce their slot types.
When you have a solid idea of the layout of your data and really need speed,
they're ideal.

## CLOS is fast enough

CLOS, for single-dispatch at least, is really quite fast. When I redesigned the
emulator to avoid a method call for every memory read/write, my benchmark only
ran ~10% faster. I eventually chose to stick with the new scheme for several
reasons, performance was only a minor factor.

## Destructuring is more expensive than you think

My second big speedup came, indirectly, from changing the arguments to the
opcode lambdas. By having the opcode only take a single argument, the CPU, I
avoided the need to destructure the opcode metadata in `step-cpu`. You **don't**
want to destructure a list in your inner loop, no matter how readable it is!

## Eval-when is about *data* more than code

That is, the times I found myself using it always involved computing data at
compile-time that would be stored or accessed at load-time or later. E.g. I used
it to ensure that the status-bit enum was created for use by later macros like
`set-flags-if`. Regardless, [try to go without it](http://fare.livejournal.com/146698.html) if possible.

## Use DECLAIM (and DECLARE) wisely

*DECLAIM* is for global declarations and *DECLARE* is for local ones. Once you've
eked out as many algorithmic gains as possible and figured out your hotspots with
the profiler, recompile your code with `(declaim (optimize speed))` to see what
notes the compiler gives you. Letting the compiler know the *FTYPE* of your most
called functions and inlining a few things can make a big difference.
