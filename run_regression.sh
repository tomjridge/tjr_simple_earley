#!/bin/bash

export ROOT=--root=.
export MAIN=bin/earley_main.exe

(g=EEE; ch=1; export g ch; for len in 200 400; do \
time dune exec $ROOT $MAIN unstaged $g `printf '%*s' $len | tr ' ' $ch`; done)

echo -----------------------------------------------------------------

(g=aho_s; ch=x; export g ch; for len in 250 500; do \
time dune exec $ROOT $MAIN unstaged $g `printf '%*s' $len | tr ' ' $ch`; done)

echo -----------------------------------------------------------------

(g=aho_sml; ch=x; export g ch; for len in 250 500; do \
time dune exec $ROOT $MAIN unstaged $g `printf '%*s' $len | tr ' ' $ch`; done)
