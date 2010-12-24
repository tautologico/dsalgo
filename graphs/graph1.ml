(*

  graph1.ml

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
      mutable vertices: (vertex * int) array; (* each vertex is a vertex list and out-degree, 
                                              to avoid O(n) calculation of out-degree from list *)
    }

(** Maximum number of vertices *)
let maxV = 1000


(* the undirected graph shown in Figure 5.4, page 151 *)
let g1 = { nvertices = 5; nedges = 7; directed = false;
           vertices = [| ([(2, 1); (5, 1)], 2);                   (* v1 *)
                         ([(1, 1); (5, 1); (3, 1); (4, 1)], 4);   (* v2 *)
                         ([(2, 1); (4, 1)], 2);                   (* v3 *)
                         ([(2, 1); (5, 1); (3, 1)], 3);           (* v4 *)
                         ([(4, 1); (1, 1); (2, 1)], 3) |]          (* v5 *)
         }


(* functions *)
let initialize_graph directed = 
  { nvertices = 0; nedges = 0; directed = directed; vertices = Array.make maxV ([], 0) }

let read_graph directed = 
  { nvertices = 0; nedges = 0; directed = directed; vertices = [] } (* TODO *)

(* insert unweighted vertex into graph *)
let rec insert_edge g x y directed = 
  let insert_at_vertex v vo =
    let edgs, deg = g.vertices.(v) in 
    let nedgs = (vo, 1) :: edgs in
    g.vertices.(v) <- (nedgs, deg + 1) in
  ( insert_at_vertex x y; if directed then insert_edge g y x false else g.nedges <- g.nedges + 1 )



