# Analysis of a staged version of Earley's algorithm

The file `e_cd.ml` contains a version of Earley's algorithm that is
unstaged, i.e., items are considered in any order.

Real Earley processes items in stages.

Which items are present at each stage?

Suppose we are at stage `k`. Then the following shows the progression
of items.

~~~

((X -> i as k bs) (l:bc)
  nt items at k; i <= k
  (cases
    (case bs=[]
      = (X -> i as k)
      (i X k ; possible NEW COMPLETE (i X k) (*) ; if i=k, must store in CMAP
        (CUT WITH X' -> h as' i X bs' ; NEED BLOCKED i X  (*)
          NEW ITEM X' -> h (as' X) k bs' ; as (l:bc))))
    (case bs = (S bs')
      = (X -> i as k (S bs'))
      NEW BLOCKED (X -> i as k (S bs')) on k S  (*)

      N.B. record whether k S is a new key before adding new blocked      
      
      (cases BLOCKED CUT/FOCUS
        (CUT WITH COMPLETE (k S j) ; NEED COMPLETE k S  (*)

          N.B. if S is a nonterminal, j must be k (if anything); thus we can record complete items k
          Y k separately from k T j

          NEW ITEM (X -> i (as S) j bs')
          (cases
            (case j=k, =(X -> i (as S) k bs'), as l:bc)
            (case k<j, NEW ITEM at j>k)))
        (FOCUS S; NEW ITEM k S

          N.B. can avoid (k S) items by looking at BLOCKED at k S (moreover, we only need BLOCKED k
          Y, since k T is handled immediately below) if non-empty, we have already processed (k S);
          N.B. don't consider such items blocked
          
          (cases S=T | S=Y
            (case S=T; = (k T)

              N.B. can avoid (k T) items by looking in complete at k T; if None, expand T else we
              have already processed k T; alternatively look at blocked; if there is an item blocked
              on k T, we have already expanded T; in fact, since we EXPAND T as soon as we encounter
              it, the only item that is BLOCKED k T is the one we are currently working with: (X ->
              i as k (T bs'))
              
              (EXPAND T ; NEW COMPLETE k T j  (*)

                N.B. in the following CUT, we only have to cut with the current item (X -> ...),
                since this is the first time we meet kT
                
                (CUT WITH X -> i as k (T bs') ; NEW ITEM (X -> i' (as T) j bs')
                  (cases
                    (k = j; NEW ITEM (X -> i (as T) k bs')  as (l bc))
                    (k < j; NEW ITEM (X -> i (as T) j bs' at stage j>k))))))
            (case S=Y
              = (k Y)
              EXPAND Y
              (Y -> k k bs'
                NEW ITEM as (l bc) ))))))))


| BLOCKED | COMPLETE  | N.B.                                                      |
|         | k X k     | i X k for i <= k, but don't need to record i < k          |
|         |           | because these are processed against BLOCKED i immediately |
| i X     |           | need BLOCKED for i < k                                    |
| k X     |           | BLOCKED for k T is handled by looking at COMPLETE         |
|         | k T j     | there are no k X j for j > k                              |
|---------+-----------+-----------------------------------------------------------|
| k X     | kTj,  kXk | these suffice to cover all possibilities at k             |

N.B. suffices for BLOCKED to index i X for i < k, since i=k is handled
by k S


BLOCKED<k map from (i,X) to nt_item set

BLOCKED=k map from (k,S) to nt_item set; k constant, so map from S

COMPLETE map from key (k,S) to int set option; k is constant, so map
from S to int set option

We also need to record the nt_items that are pending at j > k

At stage k, we start with those items blocked at k; we get new blocked
items; at the end of k, we take all the blocked items and update the
global blocked map at k


(todo management
  (initially we start with blocked k)

  N.B. todo is a list of nt_item; todo_done is a set of nt_item + i X k
  
  (which items do we need to check whether we have already processed? i.e. DONE, since we already
  have TODO
    
    (k T - no, because we can look in COMPLETE)
    (k Y - no, can check blocked k Y for non-emptiness)
    (X -> i as k bs
      
      this may be in todo, so we don't want to add it again; could it be that this item arises, but
      we have already processed it?
      
      (case bs= Y bs', item should be in blocked on k Y, or in TODO; if TODO is a list, we may want
      such items in TODO_DONE)
      
      (case bs= T bs', we have to maintain a set of such DONE items X -> i as k (T bs') or even X ->
      i as k T; if TODO is a list, place in TODO_DONE)

      (case bs=[], we may already have processed iXk; we need to record this item as DONE))
    (i X k
      we need to record this as TODO_DONE, since we certainly want to avoid repeated processing)
    ))

    

~~~

Is it better to maintain a single set S = S1 Un S2? or two sets S1 S2?
Clearly two is faster, but how much? Potentially much quicker! So the
implementation should maintain two blocked maps.

