# The Erumpo Programming Language

## Introduction

Erumpo is a lisp dialect which intends to be both expressive and performant. Currently, the following features are implemented:

 - A monadic parser and REPL in Haskell, as the reference implementation of the language
 - S-exp syntax, call-by-value semantics
 - Primitive types and operators
 - First-class functions (closures)
 - Algebraic Data Types and Pattern Matching
 - `eval` expressions on strings, mechanism for run-time metaprogramming
 - A module system with `import` declarations
 
The following features are still missing and will be implemented in this year:

 - More syntactic sugar, like multi-parameter functions/auto-currying
 - Proper error propagation, providing useful compile-time/run-time error information
 - Type system
 - First-class continuations/Tail-call optimization
 - Type-safe compile-time metaprogramming via macros
 - A bytecode virtual machine, possibly with LLVM backend
 - Foreign Function Interface, multithreading facilities, etc

## Quick Start

Use `ghc` to compile the `Main` module, or invoke the `main` IO action in the `Main` module under `ghci`. You will see the following message:

    Welcome to Erumpo REPL!
    λ >>>

Under the Erumpo REPL,  you can repeatedly type declarations/expressions and check the output.

Erumpo is a purely functional language. Evaluating an expression does not change the top-level environment. In order to introduce top-level bindings, you can use a `define` declaration.

Currently the REPL is the only way to run Erumpo programs. You may put the declarations of a long Erumpo program into a file with `.er` extension, and put it in the current directory. Then you can use an `import` declaration to import the declarations and reuse the program.

## Declarations

In Erumpo, declarations are separated from expressions. Declarations are executed, produces some side-effects (like modifying the top-level environment), and do not return a value. Also, in an Erumpo program, there can only be declarations; naked top-level expressions are invalid.

### `define` Declaration

A `define` declaration introduces top-level bindings. It has the following syntax:

    (define pat exp)

Where `pat` is a pattern and `exp` is an expression. It means pattern matching `pat` to `exp` and if succeeds, create the appropriate top-level bindings.

Examples:

    (define k 2)
    (define (Tuple x y) (Tuple 3 5))

When a `define` declaration is executed, `exp` is not immediately evaluated. Instead, it is saved and only evaluated when later evaluating an expression under REPL. When `pat` conflicts with an earlier identical `pat`, the earlier one will be overwritten.

### `import` Declaration

An `import` declaration imports declarations from an Erumpo program. It has the following syntax:

    (import module_name)

For example, `(import prelude)` means importing `prelude.er`, `(import prelude.list)` means importing `prelude/list.er`.

You might want to `(import prelude)`, which loads the standard library.

Nested imports are allowed; a valid Erumpo program can contain a list of `import` declarations following a list of `define` declarations. Later imports can shadow earlier imports, as in `define`'s case.

Currently cyclic imports are not detected and may result in stack overflow.

## Expressions

### Overview

In Erumpo, an expression is evaluated with call-by-value semantics and yields a value. Values are distinct from expressions; they can be regarded as normal forms of expressions which cannot be further reduced.

An expression is evaluated in an environment. A environment is a set of bindings, which maps variable names to values.

### Primitive Types & Constant Expressions

 - A value of `Unit` type: `nil`
 - A value of `Bool` type : `true`/`false`
 - A value of `Int` type: `42`
 - A value of `Float` type: `0.1`
 - A value of `Char` type: `'c'`

Note that the `Int` type supports arbitrarily large integers.

We also have a string type, which is internally a list of `Char`. A string literal can be written as `"233"`. The list type is explained below.

### ADT Expressions & Pattern Matching

We also have Algebraic Data Types. An ADT has one or more constructors; each constructor has a constructor name and takes a list of values. For example, the list type may be either `nil` or `(Cons car cdr)`, where `car` is the head element and `cdr` is another list. The constructor name must begin with an uppercase letter.

An ADT expression has the following form:

    (Cons_name exp0 exp1 ..)

When evaluated, `exp0`, `exp1` ... are evaluated in order, and the resulting values are used to created an ADT value with the `Cons_name` constructor.

There is a syntactic sugar for writing lists:

    [exp0 exp1 ..]

The notation is equivalent to `(Cons exp0 (Cons exp1 ..))`.

Pattern matching is provided for handy manipulation of ADT. Pattern matching may occur under several circumstances; its semantics is to match a pattern with a value, and if succeeds, return an environment. There are several kinds of patterns:

 - Empty Pattern: `_`, matches any value, create no binding
 - Constant Pattern: `val`, matches any value equal to `val`, create no binding
 - Variable Pattern: `x`, matches any value `val`, create the binding from `x` to `val`
 - ADT Pattern: `(Cons_name pat0 pat1 ..)`, recursively matches an ADT value with constructor `Cons_name`, create bindings combined from results of matching `pat0`, `pat1`, etc. If any sub-pattern fails to match, the entire match fails.

Currently there lacks a type checker, so it is up to the programmer to enforce the typing conventions, for example, every constructor must take a fixed number of values; violating the contract is non-standard behavior.

### Variable Expressions

A variable expression is simply the variable's name, like `x` or `f`. The variable's name must begin with a lowercase letter to distinguish it from a constructor. Evaluating a variable expression under an environment simply fetches the value if the binding exists, otherwise the evaluation fails.

### `lambda` Expressions & Application Expression

A `lambda` expression denotes an anonymous function. Its syntax is as follows:

    (lambda pat exp)

When evaluated, `pat`, `exp` and the current environment are all encapsulated into a closure value.

An application expression applies a function to its parameter. Its syntax is as follows:

    (f_exp x_exp)

When evaluated, `f_exp` is evaluated to get a closure value; then `x_exp` is evaluated to get a parameter value `x_val`; then `x_val` is matched against `pat`, and if succeeds, the resulting environment is combined with the closure environment, resulting in an environment to evaluate `exp`.

Note that a function defined by `lambda` only takes 1 argument; functions with multiple arguments can be written as curried functions or one function taking a `Tuple` ADT value as argument.

### `letrec` Expressions

A `letrec` expression creates mutally recursive bindings for evaluating an expression. Its syntax is as follows :

    (letrec ((pat0 exp0) (pat1 exp1) ..) exp)

Its semantics is similar to evaluate `exp` in a new environment; the new environment is created by evaluating `exp0`, `exp1`, .. in the new environment itself, then matching the resulting values with `pat0`, `pat1`, .. , creating new bindings which contribute to the new environment itself.

`letrec` makes recursion possible in Erumpo. The method for implementing recursion is not fanciful combinators, but rather `letrec` which gives function names so that they can refer to their own names (and other functions' names) in their own definition.

`letrec` is also related to the top-level "environment". After some `import`/`define` declarations are executed, we have a set of pattern/expression pairs. The evaluation of expression and pattern matching are not immediately carried out; instead, when later evaluating an expression under REPL, the pattern/expression pairs are wrapped into a `letrec` expression.

### `if` Expressions

An `if` expression has the following syntax:

    (if cond_exp then_exp else_exp)

When evaluated, first `cond_exp` is evaluated. If the resulting value is not `Bool`, evaluation fails, otherwise if it is `true`, then `then_exp` is evaluated, otherwise `else_exp` is evaluated.

### `case` Expressions

An `case` expression has the following syntax:

    (case exp ((pat0 exp0) (pat1 exp1) ..))

When evaluated, first `exp` is evaluated, then it is matched with `pat0`; if succeeds, the resulting new bindings are added to the current environment to evaluate `exp0`; if fails, then it is matched with `pat1`, and so on. If none of the pattern matched, then the evaluation fails.

### Unary Operator Expressions

An unary operator expression has the following syntax:

    (op exp)

When evaluated, first `exp` is evaluated, the the operator `op` is applied to the value. Currently `op` can be:

 - `!`, the "not" operator for the `Bool` type
 - `print`, prints the value and returns `nil`
 - `eval`, takes the string value, parses it to an Erumpo expression and evaluates it under the current environment.

### Binary Operator Expressions

A binary operator expression has the following syntax:

    (op x_exp y_exp)

When evaluated, first `x_exp` and `y_exp` are evaluated, then the operator `op` is applied to the two values. Currently `op` can be:

 - `&&`/`||`: the "and"/"or" operator for the `Bool` type
 - `==`/`!=`/`<=`/`>=`/`<`/`>`: comparison operators for primive types and ADT types
 - `+`/`-`/`*`/`/`: arithmetic operators for `Int`/`Float` type

For the precise semantics of comparison/arithmetic, refer the `Interpreter` module of the implementation. Structured comparison is enabled, for example, the comparison result of two strings (lists of `Char`s) is decided by lexicographical comparison. However, comparing two ADT values with different constructors will cause a type error. (for convenience, `nil` is less than any value)

## License

3-clause BSD license. Check `LICENSE` in this repository.