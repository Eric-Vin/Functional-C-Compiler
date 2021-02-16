# C-Compiler

A compiler written in Haskell, with the eventual goal of being able to compile C18 into x86. This compiler is built from the ground up with minimal use of non Base libraries. In general, the goal of this compiler is to successfully compile all ISO standard C, in addition to programs that, while not ISO standard C, can still be compiled with some slight inference as to the intended meaning (I.e. the compiler seeks to compile both strictly conforming and conforming C18 programs).

## Preprocessor ##
The Preprocessor supports the following directives:

* "#include"