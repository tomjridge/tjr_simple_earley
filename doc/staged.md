# Analysis of a staged version of Earley's algorithm

The file `e_cd.ml` contains a version of Earley's algorithm that is
unstaged, i.e., items are considered in any order.

Real Earley processes items in stages.

Which items are present at each stage?

Suppose we are at stage `k`. Then the following shows the progression
of items.

~~~

("X -> i as k bs (l:bc)"
 "nt items at k; i <= k"

 (cases
  ("case bs=[]; item X -> i as k"

   ("i X k"
    "item COMPLETE at i k; i <=k  (*)"

    ("X' -> h (as' X) k bs'"
     "CUT with X' -> h as' i X bs'"
     "key: i X ; NEED items BLOCKED on X at i <= k  (*)"
     "as (l:bc)")))
  
  ("case bs = (S bs'); item X -> i as k (S bs')"
   "key: k S; item BLOCKED on S at k  (*)"

   ("k S"
    "N.B. don't consider such items blocked"
    
    (cases
     ("case S=T; item = k T"
      ("k T j"
       "key = k T; item COMPLETE at k j  (*)"
       
       "k <= j"
       (cases
        ("case j = k; item = k T k"
         "NEW item at k"
         "key k T; item COMPLETE at k k  (*)"

         ("X' -> i' (as' T) k bs'"
          "CUT with X' -> i' as' k (T bs')"
          "key: k T ; NEED items BLOCKED on k T  (*)"
          "NEW item at k"
          "as l:bc"))

        ("case k < j"
         "X' -> i' (as' T) j bs'"
         "NEW item at j > k"))) ) ; S=T

     ("case S=Y; item = k Y"
      ("Y -> k k bs'"
       "NEW item at k"
       "as l:bc")))))))

~~~

Is it better to maintain a single set S = S1 Un S2? or two sets S1 S2?
Clearly two is faster, but how much? Potentially much quicker!

So the implementation should maintain two blocked maps.

At stage k, we get new 
