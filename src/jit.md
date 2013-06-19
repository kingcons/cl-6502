## `jit.lisp`: "JIT" is a bit generous, isn't it?

Proper JITs involve much careful thinking and engineering to get good results. This "jit" was more of a quick, fun Sunday evening hack. The whole point of a compiler is to rewrite code into a faster form that preserves the original semantics. That entails doing analysis to understand the original code as well as transformations to improve the parts the compiler understands. As [Daniel Ehrenberg puts it](http://useless-factor.blogspot.com/2009/10/bitfields-in-factor-structs-and-special.html), every compiler has a "special style" that it knows how to optimize. Thus, speeding up a language really corresponds to increasing the parts of the language the compiler understands, growing the special style.

This JIT doesn't understand anything. All it does is eliminate the opcode dispatch between branches. It also never invalidates the cached code which means that if a program modifies itself while it runs, as many old assembly programs do, it will still execute the original version of the code, producing incorrect results. As [Mike Pall has written](http://lambda-the-ultimate.org/node/3851#comment-57646), a fast interpreter can outrun a naive compiler. For these and other reasons, **cl-6502** sticks with interpretation. I just think the JIT is too cute to remove.

## Source Code
