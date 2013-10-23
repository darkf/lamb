**Lamb** is a toy dynamically typed functional programming language

It's not meant for real-world use but for educational purposes on writing an interpreter with pattern matching in Haskell.

**Features**

* No re-assignment (i.e., once you bind a value, you can't re-assign it)
* Pattern matching: `f([]) -> "nothing". f([a]) -> one thing".`
* Imperative I/O
* Higher-order functions
* Lexical scope


**Hello World!**

From `examples/helloworld.lamb`:

    hello() -> "hello, " + hello("world").
    hello(object) -> object + "!".
    
    putstrln(hello()).

See other examples in the `examples` directory.

**License**

Licensed under the terms of the zlib license. See `LICENSE` for details.