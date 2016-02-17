(* 
This is an attempt to write a functional version of traditional heaps. It's not an
algorithm that lends itself well to functional programming. 
*)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let print_tree t ffun = 
  let rec print_loop t ident = 
    match t with
      Nil -> ()
    | Node(v, tl, tr) -> 
       Printf.printf "%s+ %a\n" (String.make ident ' ') ffun v;
       print_loop tl (ident+4);
       print_loop tr (ident+4)
  in
  print_loop t 0

let print_int_node ch v = 
  Printf.fprintf ch "%d" v

let print_int_tree t = 
  print_tree t print_int_node

type dominator = Top | Left | Right

let node_value t = 
  match t with
    Nil -> failwith "Attempted to get value from nil tree"
  | Node (v, _, _) -> v

let max_val_dom_tree v t = 
  match t with
    Nil -> true
  | Node (v', _, _) -> v > v'

let max_tree_dom_tree t1 t2 = 
  match (t1, t2) with
    Nil, Nil -> failwith "these trees are not comparable"
  | Nil, _ -> false
  | _, Nil -> true
  | Node (v1, _, _), Node (v2, _, _) -> if v1 > v2 then true else false

let max_domination v tl tr = 
  match (max_val_dom_tree v tl, max_val_dom_tree v tr) with 
    true, true -> Top
  | false, true -> Left
  | true, false -> Right
  | false, false -> if max_tree_dom_tree tl tr then Left else Right

let rec max_heapify v tl tr = 
  match max_domination v tl tr with
    Top -> Node (v, tl, tr)
  | Left -> 
     let Node(vl, tll, tlr) = tl in
     Node (vl, max_heapify v tll tlr, tr)
  | Right ->
     let Node(vr, trl, trr) = tr in
     Node (vr, tl, max_heapify v trl trr)

let leaf v = Node (v, Nil, Nil)

let t1 = Node (14, leaf 2, leaf 8)
let t2 = Node (7, leaf 1, Nil)

let test_equal act exp ~name = 
  if act = exp then
    Printf.printf "Test %s successful\n" name 
  else
    Printf.printf "Test %s FAILED\n" name

let test_max_heapify () = 
  test_equal (max_heapify 5 Nil Nil) (leaf 5) "empty trees";
  test_equal (max_heapify 4 t1 t2) 
             (Node(14, Node(8, leaf 2, leaf 4), Node(7, leaf 1, Nil))) 
             "clrs example"
  
(* The idea here is to follow the imperative version and assume a tree layout on the 
 array a, and build the heap by successive application of max_heapify. We can begin by 
 creating leafs of all nodes with index >= N / 2 *)           
let build_max_heap a = 
  let len = Array.length a in
  let rec build_loop i =
    if i >= len then Nil
    else 
      let left = build_loop (2*i) in
      let right = build_loop (2*i+1) in
      max_heapify a.(i) left right
  in
  build_loop 1

let rec max_heapify_sent v tl tr sent = 
  match tl, tr with
  | Nil, Nil -> if v = sent then Nil else Node(v, Nil, Nil)
  | Node(vl, tll, tlr), Nil -> if v > vl then Node(v, tl, Nil) else Node(vl, max_heapify_sent v tll tlr sent, Nil)
  | Nil, Node(vr, trl, trr) -> if v > vr then Node(v, Nil, tr) else Node(vr, Nil, max_heapify_sent v trl trr sent)
  | Node(vl, tll, tlr), Node(vr, trl, trr) ->
     if v > vl then
       if v > vr then Node(v, tl, tr) else Node(vr, tl, max_heapify_sent v trl trr sent)
     else 
       if v > vr then Node(vl, max_heapify_sent v tll tlr sent, tr) 
       else if vl > vr then Node(vl, max_heapify_sent v tll tlr sent, tr) else Node(vr, tl, max_heapify_sent v trl trr sent)

  match max_domination v tl tr with
    Top -> Node (v, tl, tr)
  | Left -> 
     let Node(vl, tll, tlr) = tl in
     Node (vl, max_heapify v tll tlr, tr)
  | Right ->
     let Node(vr, trl, trr) = tr in
     Node (vr, tl, max_heapify v trl trr)


