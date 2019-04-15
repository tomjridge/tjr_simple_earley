# tjr_simple_earley: Simple Earley Parsing

## Description

This is an implementation of an Earley-like algorithm, designed for simplicity.

## Quick links

* Online **ocamldoc** can be found [here](https://tomjridge.github.io/tjr_simple_earley/index.html).

## Install

To install **from source**, checkout this repository and type: `make all`

To install **via opam**: FIXME todo


## Examples

If you have built from source, there are two executables,
`test.native` and `simple_test.native` (slower). To run the Earley
parser with the grammar `E -> E E E | "1" | eps`, with an input length
of 400 say, in the bin directory type:

    time ./test.native 400

The output should look like:

    $ time ./test.native 400
    Right hand side suffix count: 8
    400
    
    real	0m2.491s
    user	0m2.484s
    sys	0m0.004s




