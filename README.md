**Lamb** is a toy dynamically typed functional programming language

It's not meant for real-world use but for educational purposes on writing an interpreter with pattern matching in Haskell.

**Features**

* No re-assignment (i.e., once you bind a value, you can't re-assign it)
* Pattern matching
    f([]) -> "nothing".
    f([a]) -> one thing".
* Imperative I/O
* Higher-order functions
* Partial evaluation on curried functions

** Hello World! **

`examples/helloworld.lamb`:

    hello(object) -> object + "!".
    hello() -> "hello, " + hello("world").
    
    putstrln(hello()).

** License **

Licensed under the terms of the zlib license. See `LICENSE` for details.