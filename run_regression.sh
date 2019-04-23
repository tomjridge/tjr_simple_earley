#!/bin/bash

export ROOT=--root=.
export MAIN=bin/earley_main.exe

# FIXME support more algorithms... particularly simple

(g=EEE; ch=1; export g ch; for len in 200 400; do \
time dune exec $ROOT $MAIN unstaged $g :${ch}x${len}; done)

(g=aho_s; ch=x; export g ch; for len in 250 500; do \
time dune exec $ROOT $MAIN unstaged $g :${ch}x${len}; done)

(g=aho_sml; ch=x; export g ch; for len in 250 500; do \
time dune exec $ROOT $MAIN unstaged $g :${ch}x${len}; done)

