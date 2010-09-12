(*

  editdist.ml
  Edit distance calculation using Dynamic Programming
  (as suggested by an exercise in Chapter 15 of CLRS, 3rd ed.)

  Andrei de A. Formiga, 2010-09-11

*)

type action = Left | Right | Replace | Insert | Delete


(** Utility function *)
let min3 a b c = min (min a b) c

(* TODO: Describe solution idea and recursive equation *)


(* Recursive solution *)
let rec editdist_rec x i y j = 
  if i >= String.length x && j >= String.length y then 0
  else if i >= String.length x then 3 + (editdist_rec x i y (j+1))  (* insert  *)
  else if j >= String.length y then 2 + (editdist_rec x (i+1) y j)  (* delete  *)
  else if x.[i] = y.[j] then editdist_rec x (i+1) y (j+1)           (* noop    *)
  else min3 (3 + (editdist_rec x i y (j+1)))                        (* insert  *)
            (2 + (editdist_rec x (i+1) y j))                        (* delete  *)
            (4 + (editdist_rec x (i+1) y (j+1)))                    (* replace *)


(*

Table-based bottom-up solution. 


*)
let init_matrix m n = 
  let d = Array.make_matrix (m+1) (n+1) 0 in
  d.(m).(n) <- 0;
  for i = m-1 downto 0 do
    d.(i).(n) <- d.(i+1).(n) + 2
  done;
  for j = n-1 downto 0 do
    d.(m).(j) <- d.(m).(j+1) + 3
  done;
  d

let editdist_mat x y = 
  let m = String.length x in
  let n = String.length y in
  let d = init_matrix m n in
  for i = m-1 downto 0 do
    for j = n-1 downto 0 do
      if x.[i] == y.[j] then d.(i).(j) <- d.(i+1).(j+1)
      else d.(i).(j) <- min3 (d.(i+1).(j)+2) (d.(i).(j+1)+3) (d.(i+1).(j+1)+4)
    done;
  done;
  d

let editdist x y =
  let d = editdist_mat x y in
  d.(0).(0)


(* Tests *)
let x1 = "C is a relatively \"low level\" language."
let y1 = "Java is an object-oriented language."
let sol1 = 73


      

