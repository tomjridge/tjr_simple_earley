# Simple implementation of an Earley-like parsing algorithm


## Meta

This document explains the OCaml code in `simple_earley.ml`. An html
version of the code is in `simple_earley.ml.html` or possibly
[here](simple_earley.ml.html).

The code is a minimal implementation of an Earley-like parsing
algorithm.

The source for this file is `simple_earley.md`. This markdown file can
be processed with pandoc. An html version of this doc is in
`simple_earley.html`.

References to the code are of the form `l:bc` corresponding to a code
comment `(* l:bc *)` (i.e. a label `bc`).

We assume familiarity with Earley's algorithm.


## An Earley-like algorithm

Earley's algorithm works with items of the form `X -> as.bs,i,k` where:

* `X` is a nonterminal (nt)
* `as` and `bs` are sequences of symbols
* `i` and `k` are indexes into the input; $i \le k$

Notation: `i,j,k` are typically integer indexes where $i \le k \le j$;
`as` and `bs` are typically sequences of symbols; `X,Y,Z,nt` are
nonterminals; `S,sym` are symbols, `T,tm` are terminals. `X -> i as k
bs` is an alternative notation to the usual `X ->
as.bs,i,k`. Similarly $X \rightarrow _i{}\alpha_k{}\beta$. Sometimes
we use spaces instead of commas in items.

At (l:bc) the type of `nt_item` (nonterminal item) is given, which is
as expected. N.B. the `as` are stored in "reverse" order, that is, an
item `X -> A B C. U V W,i,k` is stored as
`(X,[C,B,A],[U,V,W],i,k)`. At (l:ba) the type of `tm_item` is
given. This is not standard. A terminal item is of the form $_k{}T_?$
and corresponds to an attempt to start parsing the terminal $T$ from
index $k$ (the second subscript is "to be found"). In traditional
Earley, the input is a sequence of characters, and each terminal
corresponds to a character. Here, we wish to generalize this; a
terminal may correspond to a range of the input. This enables us to
use existing parsers as terminal parsers, which is essential for
composing parsers.

At (l:cd) we introduce the notion of a complete item of the form
$_k{}S_j$. These items arise from items of the form `X -> as.,k,j`
which gives a complete item $_k{}X_j$ and parsed terminal items of the
form $_k{}T_j$.

Notation: `k S j` is $_k{}S_j$

At (l:de) we make the string type abstract. A substring is of the form
$(s,i,j)$ representing the part of the string $s$ between $i$ and
$j$ where $i \le j$.

At (l:ef) we encode a grammar as two functions. The first provides the
nt items for a given nt, with input string and index into the
string. The second encodes terminal parsers: given a tm, and a
substring $(s,i,j)$, return the indexes $k$ such that $(s,i,k)$ could
be parsed as tm.

At (l:fg) we define key types for the blocked and complete maps (more
later). Both consist of an index and a symbol.

At (l:gh) we define the map types that we need. From a set of complete
items, we need to identify those for a given start index and
symbol. This is the role of the complete map type `cm_t`. The codomain
of the map is a set of integers $j$. If $j \in cm(k,S)$ then there is
a complete item $_k{}S_j$.

The blocked map allows us to identify, from a set of nt items, those
that are currently blocked at position $k$ waiting for a parse of
symbol $S$ to complete. $nitm \in bm(k,S)$ if $nitm$ is of the form $X
\rightarrow i,\alpha,k,(S \beta)$.

The state of the Earley algorithm is represented as a record with the
following fields:

* `todo_done` is the set of all items that are pending, or have
  already been processed
* `todo` is the list of all items that are pending
* `blocked` is the blocked map
* `complete` is the complete map

At (l:hi) we add an item to the state by adding it to `todo` and
`todo_done`. If the item is already in `todo_done` we leave the state
unchanged.

At (l:ij) we implement the core Earley step. This takes a blocked item
`X -> i,as,k,(S bs)` and a complete item $_k{}S_j$ and forms a new item
of the form `X -> i,(as S),j,bs`. Note that the `as` field is stored in
"reverse" order (to make this operation more efficient).

Notation: `(S bs)` is the list `bs` with `S` cons'ed on. `(as S)` is the
list `as` with `S` joined on the end.

~~~
X -> i,as,k,(S bs)   k S j
------------------------- cut
X -> i,(as S),j,bs
~~~

We then give definitions for adding a complete item to the complete
map, and a blocked item to the blocked map.

At (l:ja) we pull out some common code to process a list of complete
items, all of which have a given key. Processing involves adding each
`citm` to the complete map, and cutting each item against the relevant
blocked items.

At (l:jk) we reach the core `step` part of Earley's algorithm. The
full algorithm repeatedly applies `step` to an initial state until
there are no further `todo` items.

At (l:jp) we at processing an nt item. This item may be complete. If
so, via `process_citms` we record it in the complete map, and process
it against any blocked items with the same key.

At (l:kl) the nt item is not complete. So we record it in the blocked
map. We then try to progress the item by cutting it with all the
current complete items with the same key. It may be that we have yet
to process all or any of the relevant complete items. So we also have
to look at the symbol the nt item is blocked on, and manufacture more
items. This is (l:lm).

At (l:mn) we are processing a terminal item. We use `p_of_tm` to
determine which substrings of the input can be parsed as the terminal
$T$. This gives us complete items of the form $_k{}T_j$. For each
`citm` we then update the complete map and process against blocked
items, using `process_citms`

That concludes the explanation of the core of the algorithm.

At (l:no) we repeatedly apply the step function in a loop until there
are no more items to do.

At (l:op) we apply the loop function to an initial state.

At (l:pq) we give an example for the grammar $E \rightarrow E E E |
'1' | \epsilon$.


## Complexity

We assume that there is a global bound on the number of results
produced by each invocation of `p_of_tm`. This is the case when
terminal parsers recognize a single character or epsilon (which is the
standard use of terminal parsers): `p_of_tm` produces either 0 or 1
results.

As implemented, the algorithm is `O(n^3 log n)` because the sets and
maps use OCaml's default sets and maps, which are implemented as
binary trees. However, clearly given an input and a grammar, there are
only a finite number of items that can be in any of the sets or
maps. Thus, we can enumerate these items, and use the enumeration to
implement e.g. a set as an array. This would give the `O(n^3)` desired
complexity.

