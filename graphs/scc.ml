(* 
 * scc.ml
 * Determining strongly-connected components for a directed graph
 * 
 * Andrei de A. Formiga, 2012-04-16
 * 
 *)

(* number of vertices *)
let n_vertices = 875714

type node = {
  n: int;
  mutable visited: bool; 
  mutable outl: int list;
  mutable inl: int list;
  mutable ftime: int;
  mutable leader: int;
}
    
type graph = {
  size: int;
  nodes: node array;
  mutable forder: int array;
}

type direction = Normal | Reverse

(* create n_vertices+1 nodes (for easier indexing of nodes 1..n_vertices) *)
let init_graph () =
  let templnode = {n=0; outl=[]; inl=[]; visited=false; ftime=0; leader=0} in
  { size = n_vertices;
    forder = Array.make n_vertices 0;
    nodes = Array.init n_vertices (fun i -> { templnode with n=i }) }

let add_out_edge n target =
  n.outl <- (target :: n.outl)

let add_in_edge n source =
  n.inl <- (source :: n.inl)

let add_edge g i j =
  add_out_edge g.nodes.(i) j;
  add_in_edge g.nodes.(j) i
    
let read_graph fname =
  let add_edge_from_file g i j =
    add_edge g (i-1) (j-1) in
  let read_loop g f =
    try
      while true do
        Scanf.fscanf f " %d %d " (add_edge_from_file g)
      done
    with End_of_file -> () in
  let g = init_graph () in 
  let f = open_in fname in
  read_loop g f;
  g

let mark_unexplored g =
  for i = 0 to (Array.length g.nodes)-1 do
    g.nodes.(i).visited <- false
  done

let dfs g s ft dir =
  let ftr = ref ft in
  let rec loop i = 
    let n = g.nodes.(i) in 
    let l = match dir with Normal -> n.outl | Reverse -> n.inl in
    n.visited <- true;
    n.leader <- if dir = Normal then g.forder.(s) else s;
    List.iter (fun x -> if g.nodes.(x).visited then () else loop x) l;
    if dir = Reverse then
      ( n.ftime <- !ftr; g.forder.(!ftr) <- i; ftr := !ftr + 1 )
    else
      () in
  loop s;
  !ftr

let dfs_loop g dir =
  let rec loop i ft =
    if i < 0 then () else
      let j = if dir = Normal then g.forder.(i) else i in
      if not g.nodes.(j).visited then
        let nft = dfs g j ft dir in
        loop (i-1) nft
      else
        loop (i-1) ft in
  loop (g.size-1) 0

let scc g =
  dfs_loop g Reverse;
  mark_unexplored g;
  dfs_loop g Normal

let get_with_default h key def =
  if Hashtbl.mem h key then Hashtbl.find h key else def
    
let count_sccs g =
  let h = Hashtbl.create (g.size / 5) in
  let c = ref 0 in
  for i = 0 to (g.size-1) do
    c := get_with_default h g.nodes.(i).leader 0;
    Hashtbl.replace h g.nodes.(i).leader (!c+1)
  done;
  h

let print_sccs h =
  Hashtbl.iter (Printf.printf "SCC with leader %d and %d vertices\n") h
    
let main () =
  Printf.printf "Reading graph from file...\n";
  let g = read_graph "SCC.txt" in
  Printf.printf "Completed reading.\n";
  scc g;
  print_sccs (count_sccs g)

(* capture output then sort by biggest SCCs: 
./scc > scc.out
sort -n -r --key=6 scc.out | less
*)

(* some test graphs *)
let g1 = {
  size = 4;
  forder = Array.make 4 0;
  nodes = [| { n=0; outl=[1; 2]; inl=[]; visited=false; ftime=0; leader=0 };
             { n=1; outl=[3]; inl=[0]; visited=false; ftime=0; leader=0 };
             { n=2; outl=[3]; inl=[0]; visited=false; ftime=0; leader=0 };
             { n=3; outl=[]; inl=[1;2]; visited=false; ftime=0; leader=0 }  |]
}

let g2 = {
  size = 9;
  forder = Array.make 9 0;
  nodes = [| { n=0; outl=[3]; inl=[6]; visited=false; ftime=0; leader=0 };
             { n=1; outl=[7]; inl=[4]; visited=false; ftime=0; leader=0 };
             { n=2; outl=[5]; inl=[8]; visited=false; ftime=0; leader=0 };
             { n=3; outl=[6]; inl=[0]; visited=false; ftime=0; leader=0 };
             { n=4; outl=[1]; inl=[7]; visited=false; ftime=0; leader=0 };
             { n=5; outl=[8]; inl=[2;7]; visited=false; ftime=0; leader=0 };
             { n=6; outl=[0]; inl=[3;8]; visited=false; ftime=0; leader=0 };
             { n=7; outl=[4;5]; inl=[1]; visited=false; ftime=0; leader=0 };
             { n=8; outl=[2;6]; inl=[5]; visited=false; ftime=0; leader=0 }  |]
}

let () = main ()
  
