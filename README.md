# tjr_simple_earley: Simple Earley Parsing

## Description

This is an implementation of an Earley-like algorithm, designed for simplicity.

## Quick links

* Online **ocamldoc** can be found [here](https://tomjridge.github.io/ocamldocs/p_tjr_simple_earley/index.html).
* The main executable (test code and examples) is [bin/earley_main.ml](./bin/earley_main.ml). 
  * After compiling, you should be able to run some examples by typing eg `make run_examples`
  * If you have installed via opam, there should be a public executable `earley_main.exe`.

## Install

To install **from source**, checkout this repository and type: `make all`

To install **via opam**: `opam pin add ...`; for more details see the Dockerfile. 


## Executables

If you have built from source, you should be able to run the examples by typing: `make run_examples`. 

There are various examples, which test variations of the following:

* data-structure implementations
* grammar, input length
* debug output

Please see `src-examples/` for more information.

## Branches

Branches are: master, dev. Master should compile always. Dev is used for changes that might break compilation temporarily.

## Parsing specification

A generic specification of parsing can be found in file `src/earley_spec.ml`. See `src-test/test_spec.ml` for an example of how it can be used to produce a trace of all items that we expect to encounter during a parse.

## Animated GIF

See [here](https://drive.google.com/file/d/1yttQ85buceu1ZCGi4Vzy5OBydwifoxUb/view?usp=sharing)

Sample output:

~~~
time $run_main spec EEE :1x100
begin -------------------------------------------------
parser=spec; grammar=EEE; input_length=100 (bin/earley_main.ml)
15759 nt_items produced (src-test/test_spec.ml)
end ---------------------------------------------------

real	0m4.800s
user	0m4.301s
sys	0m0.394s

time $run_main unstaged EEE :1x100
begin -------------------------------------------------
parser=unstaged; grammar=EEE; input_length=100 (bin/earley_main.ml)
15757 nt_items produced (src-test/test_unstaged.ml)
end ---------------------------------------------------

real	0m0.234s
user	0m0.205s
sys	0m0.015s

time $run_main unstaged EEE :1x200
begin -------------------------------------------------
parser=unstaged; grammar=EEE; input_length=200 (bin/earley_main.ml)
61507 nt_items produced (src-test/test_unstaged.ml)
end ---------------------------------------------------

real	0m1.171s
user	0m1.125s
sys	0m0.040s

time $run_main simple EEE :1x100
begin -------------------------------------------------
parser=simple; grammar=EEE; input_length=100 (bin/earley_main.ml)
15757 nt_items produced (src-test/simple_test.ml)
end ---------------------------------------------------

real	0m0.193s
user	0m0.154s
sys	0m0.025s

time $run_main simple EEE :1x200
begin -------------------------------------------------
parser=simple; grammar=EEE; input_length=200 (bin/earley_main.ml)
61507 nt_items produced (src-test/simple_test.ml)
end ---------------------------------------------------

real	0m0.692s
user	0m0.661s
sys	0m0.019s
~~~

Also [here](https://gist.github.com/tomjridge/27b3c804397b758b8102d6c51e749400)

Version of 2021q1, timings [here](https://gist.github.com/tomjridge/36ec17d16035768ee0212377b66e42e5)
