(
  (meta
    (title Analysis of a staged version of Earley's algorithm)
    (author Tom Ridge)
    (date 2016-02-20))

  (body
    (section (Introduction)

      (The file `e_cd.ml` contains a version of Earley's algorithm that is unstaged, i.e., items are
        considered in any order. Real Earley processes items in stages. Which items are present at
        each stage? The following is an analysis corresponding to the code in `e_fg.ml`.))

    (section (Analysis)

      (Suppose we are at stage `k`. Then we have an initial set of items of the form `X -> i as k
        bs`.)

      (itm=(X -> i as k bs) (l:bc)
        (nt items at k; i <= k)
        (cases
          (case bs=[]; itm=(X -> i as k); possible NEW COMPLETE item (iXk)

            (N.B. if i=k, we must store (kXk) in cm
              
              First, we check whether we have already processed the item (iXk). If we have, we don't
              do anything. If we haven't, we need to cut the item as follows.)  


            (CUT with (X' -> h as' i X bs'); NEED BLOCKED i X; NEW (X' -> h (as' X) k bs') (ref:bc))) 


          (case bs=(S bs'); itm=(X -> i as k (S bs')); NEW BLOCKED (X -> i as k (S bs'))=bitm on k S
            
            (cases S
              (case Y; bitm=(X -> i as k (Y bs'))

                (We may already have processed (k Y). If so, there should be items blocked on (k
                Y). This assumes the following invariant holds:

                  INVARIANT: processing an item at (k Y) results in at least 1 such item blocked on (k Y).

                  So we first check whether there are any blocked items, and to ensure the invariant
                  holds, we immediately add bitm to blocked.)

                (cases bitms_empty
                  (case true; nothing further to do)
                  (case false; we need to process bitm

                    (In order to process bitm we have to perform two steps: cut bitm against the
                      matching citms; and expand Y using the grammar rules. Matching citms must be
                      of the form (kYk), because:

                      INVARIANT: at stage k, citms involving a nt Y must be of the form (kYk)

                      We could look in `ixk_done` at this point, but observe that there are no items
                      blocked on (k Y). In order to get an item (Y -> k as k bs) (and hence to (Y ->
                      k cs k) and (kYk)) we need to have processed an item blocked on (k Y). This is
                      true providing:

                      INVARIANT: when processing an item (Y -> k as j bs), there is at least one
                      item blocked on (k Y).

                      This invariant obviously holds, except for the initial state. The simplest way
                      to enforce the invariant is to construct an initial state consisting of the
                      item (X -> X) (where X is the start nt).

                      Thus, we only need to expand Y at this point))))

              (case T; bitm=(X -> i as k (T bs'))

                (We make have to parse (k T) to get items of the form (kTj). First we check whether
                we have already processed such an item by looking in the `ktjs` map, for terminal
                T. Either we have, and we take the js from there, or we haven't, in which case we
                attempt to parse T from k, and store the results in the ktjs map. Either way, we
                have a set of js corresponding to items (kTj), and we cut these against bitm.)))))

        ) (# end of tree proof)



      ) (# analysis)

    (section (Datastructures)
                  
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

(* state at k *)
type state_t = {
  k: int;
  todo: nt_item list;
  todo_done: Nt_item_set.t;
  todo_gt_k: Nt_item_set.t Int_map.t;
  ixk_done: Ixk_set.t;  (* i X k *)
  ktjs: int list option Map_tm.t;  (* k T j *)
  bitms_lt_k: Nt_item_set.t Blocked_map.t;
  bitms_at_k: Nt_item_set.t Map_nt.t;  (* bitms blocked at k,X *)
  all_done: Nt_item_set.t;
}

Is it better to maintain a single set S = S1 Un S2? or two sets S1 S2?
Clearly two is faster, but how much? Potentially much quicker! So the
implementation should maintain two blocked maps.

      ) (# datastructures)

    )
  )
