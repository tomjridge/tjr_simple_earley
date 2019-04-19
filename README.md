# tjr_simple_earley: Simple Earley Parsing

## Description

This is an implementation of an Earley-like algorithm, designed for simplicity.

## Quick links

* Online **ocamldoc** can be found [here](https://tomjridge.github.io/tjr_simple_earley/index.html).
* Examples can be found in [bin/](./bin/)

## Install

To install **from source**, checkout this repository and type: `make all`

To install **via opam**: `opam pin add ...`; for more details see the relevant file in the [.dockerfile](./.dockerfile) directory. Note that this will not install the examples; for those you need to install from source.


## Examples

If you have built from source, you should be able to run the examples by typing: `make run_examples`. 

There are various examples, which test variations of the following:

* datastructure implementations
* grammars, input length
* debug output

Please see `bin/` for more information.

## Parsing specification

A generic specification of parsing can be found in file `src/earley_spec.ml`. See `bin/test_spec.ml` for an example of how it can be used to produce a trace of all items that we expect to encounter during a parse.