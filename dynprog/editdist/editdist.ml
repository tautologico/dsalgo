(*

  editdist.ml
  Edit distance calculation using Dynamic Programming
  (as suggested by an exercise in Chapter 15 of CLRS, 3rd ed.)

  Andrei de A. Formiga, 2010-09-11

*)

type action = Left | Right | Replace | Insert | Delete

let rec editdist_rec x i y j = 
  if i >= String.length x && j >= String.length y then 0
  else if i >= String.length x then 3 + (editdist_rec x i y (j+1))  (* insert  *)
  else if j >= String.length y then 2 + (editdist_rec x (i+1) y j)  (* delete  *)
  else if x.[i] = y.[j] then editdist_rec x (i+1) y (j+1)           (* noop    *)
  else min (min (3 + (editdist_rec x i y (j+1)))                    (* insert  *)
                (2 + (editdist_rec x (i+1) y j)))                   (* delete  *)
           (4 + (editdist_rec x (i+1) y (j+1)))                     (* replace *)

