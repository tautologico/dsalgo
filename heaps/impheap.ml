(***
 * 
 * impheap.ml
 * Imperative heap implemented using arrays
 * Based on the priority queue implementation in 
 * "The Algorithm Design Manual", 2nd edition, by Steve Skiena
 * 
 * Andrei de A. Formiga, 2010-08-07
 *
 ********************************************************************)

(* 

This implementation works only for element types that can be compared using 
the function min. A module+functor approach would be necessary to make it 
really generic. 

*)



type 'a heap = { queue: 'a array; mutable n: int }

exception Empty_queue


let create_heap size = 
  { queue = Array.make (size+1) 0; n = 0 }

let parent n =
  if n = 1 then -1 else n / 2           (* implicitly floor n/2 *)

let young_child n = 
  n * 2

let swap hp ix1 ix2 = 
  let tmp = hp.queue.(ix1) in
  hp.queue.(ix1) <- hp.queue.(ix2);
  hp.queue.(ix2) <- tmp

let rec bubble_up hp ix = 
  let p = parent ix in
  if p = -1 then 
    ()
  else if hp.queue.(p) > hp.queue.(ix) then
  (
    swap hp ix p;
    bubble_up hp p
  )
  else
    ()

(* TODO: error checking for queue size *)
let insert hp x = 
  hp.n <- hp.n + 1;
  hp.queue.(hp.n) <- x;
  bubble_up hp hp.n

let make_heap s n = 
  let hp = create_heap n in
  Array.iter (insert hp) s;
  hp

let min_ix hp ix1 ix2 = 
  if hp.queue.(ix1) <= hp.queue.(ix2) then ix1 else ix2

(** Determines the index of the element with minimum value between 
    the root at index ix and its two children *)
let local_min_ix hp ix = 
  let ch = young_child ix in 
  if ch + 1 <= hp.n then
    min_ix hp ix (min_ix hp ch (ch+1))
  else if ch <= hp.n then
    min_ix hp ix ch
  else
    ix

let rec bubble_down hp ix = 
  let min_index = local_min_ix hp ix in
  if min_index != ix then 
  (
    swap hp ix min_index;
    bubble_down hp min_index
  )
  else 
    ()

let extract_min hp = 
  if hp.n <= 0 then raise Empty_queue
  else 
    let min = hp.queue.(1) in
    (
      hp.queue.(1) <- hp.queue.(hp.n);
      hp.n <- hp.n - 1;
      bubble_down hp 1;
      min
    )

(* imperative, in-place heapsort *)
let heap_sort s = 
  let n = Array.length s in
  let hp = make_heap s n in
  Array.iteri (fun i x -> s.(i) <- extract_min hp) s

(* alternative, faster make_heap *)
let make_heap2 s n = 
  let hp = create_heap n in
  hp.n <- Array.length s;
  for i = 0 to (hp.n-1) do
    hp.queue.(i+1) <- s.(i)
  done;
  for i = hp.n downto 1 do
    bubble_down hp i
  done;
  hp
