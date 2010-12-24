(*

  imp_graph_al.ml

  Simple imperative graph implementation based on adjacency lists.
  Based on "The Algorithm Design Manual", 2nd edition, by Steven Skiena, Chap. 5

  Andrei de A. Formiga, 2010-10-15

*)

(* 
   type for vertices: 
   vertex v has a list of pairs (vo, w) such that 
   there is an edge from v to vo with weight w
*)
type vertex = (int * int) list

(* graph holding integers *)
type graph = 
    { mutable nvertices: int;
      mutable nedges: int;
      directed: bool;
      vertices: (vertex * int) array; (* each vertex is a vertex list and out-degree, 
                                        to avoid O(n) calculation of out-degree from list *)
    }

(** Maximum number of vertices *)
let maxV = 1000


(* the undirected graph shown in Figure 5.4, page 151 
   (note that here vertex indexing start at 0) *)
let g1 = { nvertices = 5; nedges = 7; directed = false;
           vertices = [| ([(1, 1); (4, 1)], 2);                   (* v1 *)
                         ([(0, 1); (4, 1); (2, 1); (3, 1)], 4);   (* v2 *)
                         ([(1, 1); (3, 1)], 2);                   (* v3 *)
                         ([(1, 1); (4, 1); (2, 1)], 3);           (* v4 *)
                         ([(3, 1); (0, 1); (1, 1)], 3) |]         (* v5 *)
         }


(* functions *)
let initialize_graph nvert directed = 
  { nvertices = nvert; nedges = 0; directed = directed; vertices = Array.make nvert ([], 0) }

(* insert unweighted vertex into graph *)
let rec insert_edge g x y directed = 
  let insert_at_vertex v vo =
    let edgs, deg = g.vertices.(v) in 
    let nedgs = (vo, 1) :: edgs in
    g.vertices.(v) <- (nedgs, deg + 1) in
  ( insert_at_vertex x y; if not directed then insert_edge g y x true else g.nedges <- g.nedges + 1 )

let read_graph nvert nedges directed =     (* Some problems with Scanf.scanf *)
  let g = initialize_graph nvert directed in
  for i = 1 to nedges do
    (
      print_string "edge: "; 
      flush stdout;
      Scanf.scanf "%d %d" (fun x y -> insert_edge g x y directed)
    )
  done

let print_graph g =  
  let print_adj_list el = List.iter (fun (vo, _) -> Printf.printf "%d " vo) el in
  let print_edge i el = ( Printf.printf "%d: " i; print_adj_list el; print_string "\n" ) in
  Array.iteri (fun i (el, _) -> print_edge i el) g.vertices


(* Graph search *)
type search_state = 
    { processed: bool array;
      discovered: bool array;
      parent: int array        (* probably better to use int option array *)
    }

let initialize_search g = 
  { processed = Array.make g.nvertices false; 
    discovered = Array.make g.nvertices false; 
    parent = Array.make g.nvertices (-1) }

(* breadth-first search *)
let bfs g state start proc_early proc_late proc_edge = 
  let process_edge v1 v2 = 
    if not state.processed.(v2) || g.directed then
      ignore (proc_edge v1 v2)
    else
      () in
  let discover_vertex v q p = 
    if not state.discovered.(v) then
      (
        Queue.add v q;
        state.discovered.(v) <- true;
        state.parent.(v) <- p
      )
    else
      () in 
  let q = Queue.create () in
  (
    Queue.add start q;
    state.discovered.(start) <- true;
    while not (Queue.is_empty q) do
      let v = Queue.take q in
      let el, _ = g.vertices.(v) in
      proc_early v;
      state.processed.(v) <- true;
      List.iter (fun (vo, _) -> ( process_edge v vo; discover_vertex vo q v )) el;
      proc_late v
    done
  )

(* Do a breadth-first search for printing vertices and edges *)      
let bfs_print g start = 
  let state = initialize_search g in
  bfs g state start 
    (fun v -> Printf.printf "Processed vertex %d\n" v) 
    (fun _ -> ())
    (fun x y -> Printf.printf "Processed edge %d - %d\n" x y)

(* Discover the connected components *)
let connected_components g = 
  let print_vertex v = Printf.printf " %d" v in
  let state = initialize_search g in
  let c = ref 0 in 
  for i = 0 to (g.nvertices - 1) do
    if not state.discovered.(i) then 
      (
        c := !c + 1;
        Printf.printf "Component %d:" !c;
        bfs g state i print_vertex (fun _ -> ()) (fun _ _ -> ());
        print_string "\n"
      )
    else ()
  done
