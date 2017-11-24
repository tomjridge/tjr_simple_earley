\title(An Earley-like parser built for performance and correctness)
\author(Tom Ridge)
\date(2017-11-24)

// FIXME \toc // FIXME or pass through to raw latex?

\section(Introduction) // --------------------------------------------------

This is a version of Earley's algorithm which was essentially derived
from scratch, paying attention to correctness and performance issues.

This code is the among the fastest implementations of Earley in a
high-level language that I am aware of. For example, for the grammar

`E -> E E E | "1" | eps`

with an input "111..." of length 400, the parse takes about 3s on a
single core on current commodity Intel hardware. By way of comparison,
Haskell's Happy parser takes two minutes to process an input of size
60, and cannot handle much longer lengths at all (and I doubt many
other "general" parser implementations can either). 

The problem with these other implementations is that they seem to be
buggy as far as performance goes, and these bugs tend to manifest when
using "horrible" grammars like the above, and long inputs. 

This code is meant to be a reference implementation for Earley-like
parsing, thereby providing a base line against which to measure
performance of other algorithms. It is also intended to guide
implementations of Earley in other languages: just copy the code
below.



\subsection(About this document) // ----------------------------------------

The documentation is in the form of an asciidoc file, which is turned
into an html file.

We include source code in the documentation, and these are produced by
extracting code between "special comments" in the source code
file. The special comments are of the form :ab: i.e., two letters
between two colons. The code snippet is then placed in a file
snips/xxx/ab-bc (where :bc: is the label following the label :ab:, and
xxx is the original filename). These snippets are then included in
this documentation file.

// this is a comment include(snips/tjr_earley.ml/ma-mm)



\section(Quick introduction to this variant of Earley's algorithm) // ------

Earley's algorithm is a general parsing algorithm for all context free
grammars. We assume that the reader understands the words
"nonterminal", "terminal" and "symbol". A grammar is a list of rules,
where each rule is of the form $X -> \alpha$, where
$X$ is a nonterminal and $\alpha$ is a list of
symbols.

Traditional Earley works with "items" of the form $X -> \alpha
. \beta,j$. The algorithm works in stages. At stage $k$ the algorithm
is looking at the input string at position $k$. The existence of an
item $X -> \alpha . \beta,j$ at stage $k$ means that, starting from
input position $j$, using rule $X -> \alpha \beta$, it was possible to
parse the symbols $\alpha$ to match the input between position
$j$ and $k$.

From now on we will use verbatim rather than LaTeX math.


Our version of the algorithm works with items of the form `X ->
i,as,k,bs`. This means the same as above: starting from position `i`
it was possible to parse the list of symbols `as` by consuming the
input upto position `k`. Compared to the traditional presentation, we
have placed `i` before `as`, and made `k` explicit and placed it after
`as`. This emphasizes that `as` matched the input between `i` and `k`.

We assume the reader is familiar with the functioning of Earley's
original algorithm. 

This version is similar. Compared to Earley, we have paid close
attention to the datastructures and representations. We have also made
a few optimizations that perhaps have not been noticed before. We
operate in stages, with the current stage denoted `k`.


\subsection(Notational conventions) // -------------------------------------

We make use of notational conventions. Usually the following is true:

* `i,j,k` are ints; `i` is the start position for an item; `k` is the
  current stage

* `X,Y` are nonterminals

* `T` is a terminal

* `S` is a symbol


\subsection(Earley items) // -----------------------------------------------

Traditional Earley works with a single notion of item. We extend this
to the following:

* `X -> i,as,k,bs` - the "traditional" item; we refer to this as a
  "nonterminal" item, or just "item"

* `i,X,k` - a complete (nonterminal) item; arises from nonterminal
  items of the form `X -> i,as,k,[]`, by omitting the irrelevant `as`
  and `[]` components. The meaning is: the nonterminal `X` matches the
  input between `i` and `k`.

* `k,T,j` - a complete (terminal) item; arises from matching a
  terminal against the input from position `k` to `j`.

* `X -> i,as,k,S bs` - a "traditional" item, but where the final
  component is not empty and starts with symbol `S`; we refer to this
  as a "blocked" item, because progress depends on parsing the symbol
  `S` against the input starting from position `k` (we sometimes say
  the item is blocked on `k,S`); if the parse of `S` is successful, we get a
  complete item `k,S,j` which we can cut against the original item to
  get a new item of the form `X -> i,as S,j,bs`.


\subsection(The essential parsing step) // ---------------------------------

At the heart of all general parsing algorithms is the following step.

Suppose we have an item `X -> i,as,k,S bs` (here, S is a symbol). This
means that the sequence `as` matched the input from position `i` to
`k`. Suppose we also have an item `S -> k,cs,j,[]` (i.e., the symbol
`S` matched the input from position `k` to `j`). Then we are justified
in concluding that `X -> i,as S,j,bs` (i.e., the sequence `as S`
matches the input from position `i` to `j`). This rule can be
expressed as follows:


// FIXME
// \verbatim(
// X -> i,as,k,S bs     S -> k,...,j,[]
// ------------------------------------ Cut
// X -> i,as S,j,bs
// )

This should be read as: given the two things above the line, we can
conclude with the thing below the line, and the reasoning step is
called "Cut".

In the terms of the last section, we take a "blocked" item `X ->
i,as,k,S bs` and a complete item `k,S,j` and cut them to give an item
`X -> i,as S,j,bs`.


\subsection(From the "essential step" to Earley's algorithm) ------------

Roughly the idea is as follows: from an initial nonterminal `X`, keep
adding more and more items until you end up with a complete item
`(0,X,l)`, where `l` is the length of the input. This is a
straightforward "stupid" algorithm. With appropriate datastructures,
this is $O(n^3)$.

Earley adds a single optimization: process the input character by
character. At stage `k`, process all items of the form `X ->
i,as,k,bs` (and similar) before moving on to stage `k+1`. It seems
plausible that in practice this will run faster than the "stupid"
algorithm above.

To arrive at the code below, I took these ideas and then examined the
steps that were needed in minute detail, taking care to retain good
performance and (hopefully) correctness.


\section(Code) // ----------------------------------------------------------

\subsection(Preliminaries) // ----------------------------------------------

We define some basic library types and functions. Note that for sets
and maps we use records rather than (stdlib) functors. This is to
allow us to choose the implementation at runtime: if the input size is
small we can use arrays; otherwise we can fall back to hashmaps or
maps.


\ocaml_include(snips/tjr_earley.ml/ma-mm)


Note that the type `('e,'t) set_ops` involves a type of elements `'e`,
and a type `'t` for the set itself. Similarly, the type `('k,'v,'t)
map_ops` is a map from keys `'k` to values `'v`, implemented by type
`'t`.


\subsection(Input signature) // --------------------------------------------

The code is parameterized over the signature `S_`, which defines all
the types and operations we assume. We use a signature so that we can
instantiate the generic code with various different
implementations. For example, the test code uses integers to represent
items.


\ocaml_include(snips/tjr_earley.ml/mm-mn)


First we declare the types for nonterminals, terminals and symbols.


\ocaml_include(snips/tjr_earley.ml/mn-mo)


Next we give the type for (nonterminal) items, together with
associated operations. An item is like a record, of the form
`{nt;i;as;k;bs}`, indicating that the sequence of symbols `as` could
be parsed between `i` and `k`; this corresponds to an attempt to parse
the production `nt -> as bs` starting at position `i`. Note that we
keep the type abstract, and values of type `nt_item_ops` provide the usual record
projection functions.


\ocaml_include(snips/tjr_earley.ml/mo-mp)


Next we have various types for 

* sets of items

* triples `(i,X,k)` of a nonterminal `X` and two integers `i,k`, which
  corresponds to a parsed item of the form `X -> ...` between `i` and
  `k` in the input (a "complete" nonterminal item); note that because `k` is the "current stage", it suffices to record only `(i,X)` rather than `(i,X,k)`.

* maps from nonterminals, integers, and terminals


\ocaml_include(snips/tjr_earley.ml/mp-mq)


We need to record the blocked items less than `k` (where `k` is the
"current" position in the input). This is a map from an integer `i<k`,
to a map from a nonterminal to a set of items, type `bitms_lt_k`.


\ocaml_include(snips/tjr_earley.ml/mq-mr)



We also need to record the items that we need to process at stage
`k'>k`. This is a map from `k'>k` to a set of items.

\ocaml_include(snips/tjr_earley.ml/mr-ms)


Finally we have the operation `cut`, which takes an item
`{nt;i;as;k;bs}` where `bs` is of the form `X::bs'`, and an int `j`
which indicates that symbol `X` could be parsed between `k` and `j`,
and produces the new item `{nt;i;as';j;bs'}`, where `as'` is `as` with
the parsed symbol `X` appended.


\ocaml_include(snips/tjr_earley.ml/ms-nm)




\subsection(Functor declaration) // ----------------------------------------

The code is parameterized by the signature `S_` and defined within a functor.

\ocaml_include(snips/tjr_earley.ml/nm-np)




\subsection(State type) // -------------------------------------------------

The algorithm executes in stages. At each stage `k`, the state of the
algorithm is captured as a record.


\ocaml_include(snips/tjr_earley.ml/np-nq)


Maps from int are typically indexed by `k`. 

NOTE Typically eg `todo_gt_k` would be sparse and, after the first few
k, have no entries (since this gets filled when a terminal completes).







\subsection(Auxiliary functions: blocked items) // -------------------------

The `bitms` function retrieves the blocked items corresponding to an
index `k` and a nonterminal `X`. The `add_bitm_at_k` function adds an item
at the current stage.

\ocaml_include(snips/tjr_earley.ml/nq-nr)


\subsection(Auxiliary functions: todo items) // ----------------------------

The `pop_todo` function pops an item off the list of items that are "todo" at this stage. 
//
The `add_todo` function adds an item either at the current stage, or at a later stage, depending on the item.

\ocaml_include(snips/tjr_earley.ml/nr-ns)



\subsection(Auxiliary functions: \texttt{(i,X,k)} items) // -----------------------

Similarly, we have `add` and `mem` functions for `(i,X,k)` items.

\ocaml_include(snips/tjr_earley.ml/ns-nt)


\subsection(Auxiliary functions: \texttt{find\_ktjs}) // ---------------------------

Finally we have a function to find the set of `j` s corresponding to a
parsed terminal item `(k,T,j)`. At stage `k`, we maintain a map from
`T` to a list of int (the `j` s). We use an option to distinguish the
case where we have not attempted to parse `T` at position `k`, from
the case where we have tried to parse, but there were no results.

\ocaml_include(snips/tjr_earley.ml/nt-nu)



\subsection(Main code, \texttt{run\_earley} and \texttt{step\_k}) // ------------

The main code is parameterized by various set and map operations,
`cut`, `new_items` (which provides new items according to the
grammar), `input` (the input itself), `parse_tm` (which details how to
parse terminals), `input_length` (the length of the input; the input
is not necessarily a string, but can be arbitrary), `init_nt` (the
initial nonterminal to start the parse).

\ocaml_include(snips/tjr_earley.ml/nu-oc)


We then have some trivial code:


\ocaml_include(snips/tjr_earley.ml/oc-od)


Finally, we can start the algorithm proper.

We start by popping `nitm` off the todo items at the current stage
`k`. We check whether the item is complete (i.e., `bs = []`).

\ocaml_include(snips/tjr_earley.ml/od-oe)


\subsection(Complete items) // ---------------------------------------------

In the complete case, we may or may not have seen the complete item
`(i,X,k)` before. We check whether it has already been done or not.


\ocaml_include(snips/tjr_earley.ml/oe-of)


If it has already been done, we do nothing:

\ocaml_include(snips/tjr_earley.ml/of-og)


Otherwise we have to record `(i,X,k)` in the set of done items for the
current stage. Additionally, we have to process (cut!) all blocked
items `(Y -> i',as,i,X bs)` against this complete item to produce new
items of the form `(Y -> i',as X,k,bs)` which we add to the set of
todo items at the current stage.


\ocaml_include(snips/tjr_earley.ml/og-og)


This concludes the handling of complete items.


\subsection(Incomplete items) // -------------------------------------------

If the item is incomplete, then we have a new blocked item of the form
`X -> i,as,k,(S bs')` at stage `k`. The symbol `S` may be a terminal
or nonterminal. We case split on which it is.

\ocaml_include(snips/tjr_earley.ml/og-oh)



\subsubsection(Incomplete nonterminal items) // ----------------------------


If the symbol is a nonterminal `Y`, then we have an item blocked on
`k,Y`. We lookup all blocked items `bitms` at `k,Y`. If we have not
processed this item, then `bitms` will be empty. If we have processed
any item blocked on `k,Y`, then `bitms` will be nonempty, and we do
not need to expand `Y`.

\ocaml_include(snips/tjr_earley.ml/oh-oi)


If `bitms` is not empty, then we have already expanded `Y` at stage
`k`. However, we may have new complete items `(k,Y,j)` (where, in
fact, `j=k` because we are still at stage k). If we have a complete
item `(k,Y,k)` we cut it against the blocked item `X -> i,as,k,(Y
bs')` to get a new item `X -> i,as Y,k,bs'`.


\ocaml_include(snips/tjr_earley.ml/oi-oj)



If `bitms` is empty, we need to expand the symbol `Y` to get new items.

\ocaml_include(snips/tjr_earley.ml/oj-ok)


This completes the handling of incomplete nonterminal items.


\subsubsection(Incomplete terminal items) // -------------------------------

If the symbol is a terminal `T`, we check whether we have any complete
items `(k,T,j)`. If we have not yet processed `T` at stage `k`, then
`ktjs` will be `None`:


\ocaml_include(snips/tjr_earley.ml/ok-ol)


In which case, we process `k,T` by calling the auxiliary `parse_tm`
with the terminal, the input, the input length, and the current
position `k`. We record the `js` corresponding to successful parses
`(k,T,j)`.


\ocaml_include(snips/tjr_earley.ml/ol-om)


If we have already processed `k,T`, then we just retrieve the `js`
from previously. In either case, we have the list `js` of `j` s that we
need to process against items blocked on `k,T`. 

Recall that we are processing item `X -> i,as,k,(T bs')`. Certainly we
need to cut this against all the `(k,T,j)` items we have just
found. Do we need to deal with any other items blocked on `k,T`?
Suppose there was such an item; then it would have been processed at
some earlier stage, and at that point it would have been cut against
the items `(k,T,j)`. So in fact, we only need to worry about the
current blocked item, which we cut against each of the `j` s to get
new todo items.  FIXME this could be checked with an assert


\ocaml_include(snips/tjr_earley.ml/om-or)


This concludes the exposition of the `step_k` function.




\subsection(Main code: \texttt{loop\_k}) // ----------------------------------------

At stage `k`, if there are todo items we process them, otherwise we stop.


\ocaml_include(snips/tjr_earley.ml/or-pm)


\subsection(Main code: \texttt{loop}) // ------------------------------------------

The main loop repeatedly processes items at `k` before moving on to `k+1`.


\ocaml_include(snips/tjr_earley.ml/pm-ps)



\subsection(Main code: \texttt{result}) // ---------------------------------------

Finally, we construct the result by calling `loop`, starting from `k=0`.

\ocaml_include(snips/tjr_earley.ml/ps-pu)


And that is the end of the code.

\ocaml_include(snips/tjr_earley.ml/pu-py)



\section(Using the library) // ---------------------------------------------

There is an example of how to use the library in the file
`test.ml`. Note that this includes an optimization (represent items by
ints) that introduces some complexity. 

A more straightforward implementation is in file
`simple_test.ml`. This version takes about twice as long as the
"optimized" version. However, this is the one to look at if you want
to understand how to use this library for parsing.



\section(Informal derivation) // -------------------------------------------

From the cut rule, it is clearly important to keep track of items that
are blocked on `i,X`. Let's call these `B(i,X)`; `B(i,X)` is a map
from `(i,X)` to a set of nonterminal items. In the implementation we
distinguish whether the items are blocked at the current stage
`B(k,X)` or some previous stage `B(k'<k,X)`.

We also need to keep track of complete items, which are either
`C(i,X,k)` or `C(k,T,j)`. _In the implementation `C(k,T,j)` is a map
from `T` to an optional list of `j`, so that we can check whether we
have already attempted to parse `T` at stage k.

At each stage `k` we have a list of `todo(k)` items which we need to
process. Processing these items can result in further items being put
on the `todo(k)` list (or on the lists for `k'>k`). _In the
implementation we keep track of all items that are either `todo` or
`done`, in order to avoid adding an item more than once to `todo`.
FIXME why track done?

Then, for each item in the `todo(k)` list of the form `X ->
i,as,k,bs`:

* If item of the form `X -> i,as,k,[]` then process complete item
  `(i,X,k)`, by cutting it against all items blocked on `(i,X)`. _In
  the implementation we keep track of the set of complete items
  `(i,X,k)` and only process each once.

* If item of the form `X -> i,as,k,Y bs` then process `Y` at stage
  `k`, by expanding it using the grammar rules. We also have to add
  the item to the blocked items `B(k,Y)`. ADDITIONALLY check whether a
  complete item `(k,Y,k)` has been found, and if so cut it with the
  item `X -> ...` to get a new todo `X -> i,as Y,k,bs`. _In the
  implementation we only expand `Y` once at stage `k`.

* If item of the form `X -> i,as,k,T bs` then process `T` by parsing
  the input at position `k` to get a set of terminal items of the form
  `(k,T,j)`. We then add further todo items `X -> i,as T,j,bs`. NOTE
  we do not record items blocked on terminals - we process these
  immediately then they are encountered. _In the implementation, we
  implement `C(k,T,j)` as a map from `T` to an OPTIONAL list of `j` so
  that we only parse `T` once.

It is fairly easy (!) to convince yourself that this indeed captures
all the possible cases that can arise.
