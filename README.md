# PCF
Haskell implementation of a Programming Computable Functions (PCF) interpreter.

This is a statically typed lambda calculus with Booleans, natural numbers, and a fix operator for general recursion.

See example.cf for an example program. To run it, type "stack build && stack exec PCF-exe example.cf" in the root project directory.

Some basic logical and arithmetic functions are defined in standard.cf. They can be imported by using 'import standard'. The import feature is very basic -- it simply inlines the imported file in the place of the import command. Imported files can't import other files, so if they depend on other files you must be sure to import them manually and in the correct order.

A program is a sequence of commands, each of which is either a bind or eval command (not counting imports).
* bind - assign a term to a name in the global context, so that the name may appear free in the terms of subsequent commands, and its corresponding term will be substituted in.
* eval - reduce a term to a value (guaranteed by the type system).

The result of the last command is displayed by the interpreter. It can be either bind or eval -- the result of a bind is a (Id, Term) pair; the result of an eval is the value resulting from its term.
