This is a simple implementation of Chapter3 & Chapter4 of the book *Types and Programming Languages*

Part of the code refers to the implementation from the website http://www.cis.upenn.edu/~bcpierce/tapl.

## Requirements

* `ocaml == 4.08.1`
* `ocamllex == 4.08.1`
* `ocamlyacc == 4.08.1`

## Usage

* Build: run `make`
* Read from a file: run `./f test.f`.
* Use the REPL mode: run `./f -i` and use `;;` like OCaml to seperate commands.

For example:

```
❯ ./f -i
> iszero 0;;
true
> succ succ 0;;
2
> if iszero succ 0 then succ 0 else 0;;
0
```

* Clean up the directory: run `make clean`;

## Main Features

* Introduce the precedence of symbols such that it supports complex expressions without parentheses, like
  * `succ succ if true then 0 else succ 0;`
  * `if if iszero succ 0 then true else false then succ 0 else pred succ 0;`
* Support the REPL mode.