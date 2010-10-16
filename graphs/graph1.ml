(*

  graph1.ml

  Simple graph implementation based on adjacency lists.
  Based on "The Algorithm Design Manual", 2nd edition, by Steven Skiena

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
    { nvertices: int;
      nedges: int;
      directed: bool;
      vertices: (vertex * int) list;   (* each vertex is a vertex list and out-degree, 
                                          to avoid O(n) calculation of out-degree from list *)
    }


(* the undirected graph shown in Figure 5.4, page 151 *)
let g1 = { nvertices = 5; nedges = 7; directed = false;
           vertices = [ ([(2, 1); (5, 1)], 2);                   (* v1 *)
                        ([(1, 1); (5, 1); (3, 1); (4, 1)], 4);   (* v2 *)
                        ([(2, 1); (4, 1)], 2);                   (* v3 *)
                        ([(2, 1); (5, 1); (3, 1)], 3);           (* v4 *)
                        ([(4, 1); (1, 1); (2, 1)], 3) ]          (* v5 *)
         }


(* functions *)
let initialize_graph directed = 
  { nvertices = 0; nedges = 0; directed = directed; vertices = [] }

let read_graph directed = 
  { nvertices = 0; nedges = 0; directed = directed; vertices = [] } (* TODO *)

let insert_edge g x y directed =                                    (* TODO *)
  ()



