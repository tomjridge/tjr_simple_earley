# tjr_simple_earley: Simple Earley Parsing

## Description

This is an implementation of an Earley-like algorithm, designed for simplicity.

## Quick links

* Online **ocamldoc** can be found [here](https://tomjridge.github.io/tjr_simple_earley/index.html).

## Install

To install **from source**, checkout this repository and type: `make all`

To install **via opam**: FIXME todo


## Examples

If you have built from source, you should be able to run the examples by typing: `make run_examples`

There are three executables: `simple_test.exe`, `test.exe` and `test2.exe`. Each runs the Earley parser with the grammar `E -> E E E | "1" | eps`. The input is a string of "1"s. The length of the input is given as a command line argument to the executable. For example, to run `simple_test.exe` with an input of length 400, you would type: `dune exec bin/simple_test.exe 400`

The different examples correspond to different datastructure implementations.

The output of `make run_examples` should resemble the following:

~~~
time dune exec bin/simple_test.exe 400
400

real	0m4.485s
user	0m4.436s
sys	0m0.182s
time dune exec bin/test.exe 400
Right hand side suffix count: 8
400

real	0m1.960s
user	0m1.844s
sys	0m0.109s
time dune exec bin/test2.exe 400
400
Number of items processed: 242602

real	0m1.927s
user	0m1.849s
sys	0m0.076s
~~~





