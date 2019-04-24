# tjr_simple_earley: Simple Earley Parsing

## Description

This is an implementation of an Earley-like algorithm, designed for simplicity.

## Quick links

* Online **ocamldoc** can be found [here](https://tomjridge.github.io/tjr_simple_earley/index.html).
* The main executable (test code and examples) is [bin/earley_main.ml](./bin/earley_main.ml). 
  * After compiling, you should be able to run some examples by typing eg `make run_examples`
  * If you have installed via opam, there should be a public executable `earley_main.exe`.

## Install

To install **from source**, checkout this repository and type: `make all`

To install **via opam**: `opam pin add ...`; for more details see the relevant file in the [.dockerfile](./.dockerfile) directory. 


## Executables

If you have built from source, you should be able to run the examples by typing: `make run_examples`. 

There are various examples, which test variations of the following:

* data-structure implementations
* grammar, input length
* debug output

Please see `bin/` for more information.

## Parsing specification

A generic specification of parsing can be found in file `src/earley_spec.ml`. See `bin/test_spec.ml` for an example of how it can be used to produce a trace of all items that we expect to encounter during a parse.
