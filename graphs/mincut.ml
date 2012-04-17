(*
 * mincut.ml
 * Determining the minimum cut in an undirected graph using 
 * Karger's random contraction algorithm. 
 * 
 * Andrei de A. Formiga, 2012-04-09
 * 
 *)

type supernode = { ns: int list }

type node = Normal of int | Super of supernode
    
type graph = {
  mutable nvertices: int;
  nodes: node array;
  mutable edges: (int * int) list;
}

(* utility functions *)
    
(** Split a string into substrings using characters in string seps as separators. 
    Default separator is a space character. *)
let split ?(seps=" ") s = 
  let l = String.length s in
  let rec sep i = 
    if i >= l then [] else 
      if String.contains seps s.[i] then sep (i+1) else nonsep i 1
  and nonsep i j = 
    if (i+j) = l then [(i,j)] else 
      if String.contains seps s.[i+j] then (i,j) :: sep (i+j) else nonsep i (j+1) in
  List.map (fun (i, l) -> String.sub s i l) (sep 0)

(** Select a random element from a list. *)
let random_elt lst = 
  let l = List.length lst in
  let i = Random.int l in 
  List.nth lst i

let flip_pair (f, s) = (s, f)
  
(* main functions *)
let rec append_unique e1 e2 =
  match e1 with
      [] -> e2
    | e :: e1r ->
      if List.exists (fun ex -> e = ex || e = (flip_pair ex)) e2 then
        append_unique e1r e2
      else
        append_unique e1r (e :: e2)
          
let read_graph file_name =
  let read_line line =
    match split ~seps:" \t\r\n" line with
        [] -> failwith "read_graph: Empty line"
      | vs :: rest ->
        let v = int_of_string vs in
        (Normal v, List.map (fun v2 -> (v, int_of_string v2)) rest) in
  let rec read_loop f vertices edges =
    try
      let line = input_line f in
      let v, es = read_line line in
      read_loop f (v :: vertices) (append_unique es edges)
    with End_of_file -> (vertices, edges) in
  let f = open_in file_name in
  let vs, es = read_loop f [] [] in
  close_in f;
  { nvertices = List.length vs;
    nodes = Array.of_list (List.rev vs);
    edges = es }

let node_size n =
  match n with
      Normal _ -> 1
    | Super s -> List.length s.ns
      
(* merge vertices with numbers vn1 and vn2, 
   assuming vertices are stored in order (vertex 1 in index 0, and so on) *)
let merge_vertices g (vn1, vn2) =
  let merged_vertex v1 v2 =
    match v1, v2 with
        (Normal n1, Normal n2) -> Super {ns = [n1; n2]}
      | (Super s1, Normal n2) -> Super {ns = (n2 :: s1.ns)}
      | (Normal n1, Super s2) -> Super {ns = (n1 :: s2.ns)}

      | (Super s1, Super s2) -> Super {ns = (s1.ns @ s2.ns)} in
  let change_merged_vertices v =
    match v with
        Normal _ -> failwith "merge_vertices: unexpected normal node"
      | Super s -> List.iter (fun n -> g.nodes.(n-1) <- v) s.ns in
  let v1, v2 = g.nodes.(vn1-1), g.nodes.(vn2-1) in
  let v = merged_vertex v1 v2 in
  change_merged_vertices v;
  g.nvertices <- g.nvertices - 1
    
let remove_self_loops g =
  let not_self_loop (vn1, vn2) =
    let v1, v2 = g.nodes.(vn1-1), g.nodes.(vn2-1) in
    v1 != v2 in
  g.edges <- List.filter not_self_loop g.edges

  
let rec random_contraction g =
  if g.nvertices == 2 then g
  else
    let e = random_elt g.edges in
    merge_vertices g e;
    remove_self_loops g;
    random_contraction g

let mincut_value g =
  List.length g.edges

let next_seed s =
  (s * s + 9931) mod 104173
    
let mincut g trials =
  let _ = Random.init 104173 in
  (* let _ = Random.init 64693 in *)
  let rec loop c mc =
    if c >= trials then mc
    else
      let g' = { nvertices=g.nvertices; nodes=Array.copy g.nodes; edges=g.edges} in
      let _ = random_contraction g' in
      let m = mincut_value g' in
      if m < mc then loop (c+1) m
      else loop (c+1) mc in
  loop 1 (g.nvertices-1)
    
(* small test graphs *)
let g1 = {
  nvertices = 4;
  nodes = [|Normal 1; Normal 2; Normal 3; Normal 4|];
  edges = [(1, 2); (2, 3); (3, 4); (4, 2)]}

let g2 = {
  nvertices = 4;
  nodes = [|Normal 1; Normal 2; Normal 3; Normal 4|];
  edges = [(1, 2); (1, 4); (2, 3); (3, 4); (4, 2)]}

let g3 = {
  nvertices = 5;
  nodes = [|Normal 1; Normal 2; Normal 3; Normal 4; Normal 5|];
  edges = [(1, 2);
           (2, 3); (2, 4); (2, 5);
           (3, 4); (3, 5);
           (4, 5)]
}  

let main () =
  let trials = 199000 in
  let g = read_graph "kargerAdj.txt" in
  let mc = mincut g trials in
  Printf.printf "Mincut value for graph: %d\n" mc
    
let _ = main ()
  
