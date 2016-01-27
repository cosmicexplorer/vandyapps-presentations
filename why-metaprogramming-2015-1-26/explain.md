why metaprogramming
===================

- super overloaded term
    - compile-time computation
    - code generation
    - higher order functions
    - reflection / modifying types at runtime
    - bullshit buzzword
- compile-time computation: prime checker
    - [pattern matching](./prime-compile.cpp)
    - [performance](./prime-constexpr.cpp)
- code generation: create language features
    - [resource protection](./ping-url.lisp)
    - [async](https://github.com/cosmicexplorer/markdown-mode/blob/feat/add-async/markdown-mode.el)
- higher order functions: establish context for functions passed in
    - [getFilesFromPackageJsonMacro](~/projects/active/cpm/src/lib.coffee)
- modification of types at runtime: prototypal inheritance
    - [add array function](./prototypal.js)
- more types
    - eval-based code generation
        - super difficult
        - only used by terrorists
- next time: c++ template metaprogramming in depth!
