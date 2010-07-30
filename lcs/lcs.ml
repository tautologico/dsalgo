(**
 *
 * lcs.ml
 * 
 * Longest Common Subsequence implementation(s)
 * 
 * Andrei de A. Formiga, 2010-07-29
 *
 ********************************************************************)

(* utilitarian function *)
let rec list_to_string lc = 
  match lc with
      [] -> ""
    | c :: rlc -> (String.make 1 c) ^ (list_to_string rlc)


(* Recursive, exponential solution (calculates only the length of the LCS) *)
let lcs_cost_rec s1 s2 = 
  lcs_cost_rec_span s1 s2 ((String.length s1) - 1) ((String.length s2) - 1)

let rec lcs_cost_rec_span s1 s2 n m = 
  if n < 0 || m < 0 then
    0
  else if s1.[n] == s2.[m] then 
    1 + (lcs_cost_rec_span s1 s2 (n - 1) (m - 1))
  else
    max (lcs_cost_rec_span s1 s2 (n - 1) m) (lcs_cost_rec_span s1 s2 n (m - 1))
    


(*** Bottom-up imperative implementation **************************************)
let build_lcs_matrix s1 s2 l1 l2 = 
  let m = Array.make_matrix (l1+1) (l2+1) 0 in
  for i = 1 to l1 do
    for j = 1 to l2 do
      if s1.[i-1] == s2.[j-1] then 
        m.(i).(j) <- 1 + m.(i-1).(j-1)
      else
        m.(i).(j) <- max m.(i).(j-1) m.(i-1).(j)
    done;
  done;
  m

let lcs_length s1 s2 = 
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let m = build_lcs_matrix s1 s2 l1 l2 in
  m.(l1).(l2)
  
let lcs s1 s2 = 
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let m = build_lcs_matrix s1 s2 l1 l2 in
  let rec search i j = 
    if m.(i).(j) == 0 then
      []
    else if s1.[i-1] == s2.[j-1] then
      s1.[i-1] :: search (i-1) (j-1)
    else if m.(i-1).(j) == m.(i).(j) then
      search (i-1) j
    else 
      search i (j-1) in
  list_to_string (List.rev (search l1 l2))

(* No need to create list -> transform to string *)
let lcs2 s1 s2 = 
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let m = build_lcs_matrix s1 s2 l1 l2 in
  let rec search i j res = 
    if m.(i).(j) == 0 then
      res
    else if s1.[i-1] == s2.[j-1] then
      search (i-1) (j-1) ((String.make 1 s1.[i-1]) ^ res)   (* still, O(n) concatenations *)
    else if m.(i-1).(j) == m.(i).(j) then
      search (i-1) j res
    else 
      search i (j-1) res in
  search l1 l2 "" 


