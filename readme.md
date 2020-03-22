This is a simple implementation  of the book *Types and Programming Languages*.

The code is based on the implementation from the website http://www.cis.upenn.edu/~bcpierce/tapl.

## Current Support

* `arith` : chapter 3, 4
* `untyped` : chapter 5, 6, 7
* `simplytyped` : chapter 8, 9, 10



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

