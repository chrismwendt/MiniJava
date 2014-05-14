# MiniJava

A MiniJava compiler written in Haskell, modeled after Gregor Richard's implementation for Purdue University's compilers class Spring 2014.

## Lexing and parsing

Parsec is a parsing library for Haskell (not a custom language which gets compiled into Haskell, unlike JavaCC for Java) which made the parser for MiniJava incredibly concise and straightforward.

## Type checking

I strayed from the canonical implementation and performed type checking on the AST instead of first compiling to SSA form in order to avoid some redundant checks and look-ups.

## SSA generation

Instead of compiling the SSA statements into a list and reconstructing the control flow graph in the register allocator, I stored the SSA in a control flow graph directly and linearize it when needed. Using fgl (Functional Graph Library for Haskell) and its wealth of functions was critical to making the control flow graph work smoothly.

## Register allocation

By far the most amount of time was spent on register allocation, partly because I only got about half of the examples to work in my Java version that I submitted for class, and partly because I found a problem with the canonical algorithm which could cause it to get stuck and I came up with a different register allocation algorithm to solve it. Here is an example which could cause a problem, with a register limit of 2:

```java
int a;
int b;
int c;
System.out.println(a);
System.out.println(b);
System.out.println(c);
```

It would not work to spill `c` since a register must be available to compute the value, but determining which of `a`, `b`, or `c` to spill cannot be done using information in the interference graph alone.

## Code generation

Code generation mostly involved a 1:1 translation from Java to Haskell.

## Notes
- I encountered significantly fewer frustrating bugs in Haskell than in Java, probably because of the strong static typing and functional purity
- I spent more time thinking, designing, and understanding my solution in Haskell and less time throwing code around compared to Java
- The Haskell code is a factor of 2 or 3 shorter than Java
- Translating this stateful compiler into Haskell took longer than I had expected (no surprise)
