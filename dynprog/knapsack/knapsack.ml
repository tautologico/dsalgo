(*

  knapsack.ml

  Solve the 0-1 knapsack problem using dynamic programming
  (based on exercise 16.2-2 from the book "Introduction to Algorithms", 3rd ed.)

  Andrei de A. Formiga, 2010-09-21

*)


let rec knapsack tw v w i = 
  if i >= Array.length v then
    0
  else if w.(i) > tw then
    knapsack tw v w (i+1)
  else
    max (v.(i) + (knapsack (tw - w.(i)) v w (i+1)))
        (knapsack tw v w (i+1))


let rec knapsack_l tw is = match is with
    [] -> 0
  | (v, w) :: is' -> 
    if w > tw then knapsack_l tw is'
    else max (v + knapsack_l (tw-w) is')
             (knapsack_l tw is')

(*

  The iterative bottom-up table-based solution must be indexed by the amount tw of 
  space left on the knapsack and the item i under consideration. 

  A wasteful solution would be to create a W x n table for every possible weight 
  w between 0 and W. 

  In this case, memoization seems to be easier than building a table bottom-up. 

*)

(* Memoized knapsack *)
let knapsack_mem tw vs ws =
  let n = Array.length vs in
  let memo = Hashtbl.create (5 * n) in   (* Guesses at 5 * number of items *)
  let rec loop i w = 
    if Hashtbl.mem memo (i, w) then Hashtbl.find memo (i, w)
    else if i >= n then 0
    else 
      let res = 
        if ws.(i) > w then loop (i+1) w 
        else max (vs.(i) + loop (i+1) (w - ws.(i)))
                 (loop (i+1) w) in
         Hashtbl.add memo (i,w) res;
         res in
  loop 0 tw


(* Some test data and solutions *)
let is1 = [(60, 10); (100, 20); (120, 30)]
let v1 = [| 60; 100; 120 |]
let w1 = [| 10; 20; 30 |]
let sol1 = 220


