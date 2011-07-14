open Lgraph
open Utils
open BatStd
open BatList
module S = BatString

let reenumgraph g = 
  let q = g.graph in
  let i = ref 1 in
  let h = H.create 0 in
  let iteration v = 
    let id = vids v in
    H.add h id !i;
    i:=!i+1 in
  let mapv v =
    let id = vids v in
    trid (H.find h id) v in
  G.iter_vertex iteration q;
  G.map_vertex mapv q

let edge_count gg =
  let g = gg.graph in
  let c = ref 0 in
  G.iter_edges_e (fun _ -> c:=!c+1) g; !c;;

let vertex_count gg = 
  let g = gg.graph in
  let c = ref 0 in
  G.iter_vertex (fun _ -> c:=!c+1) g; !c;;

let metisadj g =
  let l = ref [] in
  let itrlw w i = if i=[] then [] else interleave ~last:w w i in
  let succ v =
    let s = G.succ g v in
    let ids = L.map vids s in
    let wg = if L.length s > 1 then 1000 else 1 in
    itrlw wg ids
  in
  let pred v = 
    let p = G.pred g v in
    let ids = L.map vids p in
    let wg = if (L.length <| (L.concat <| L.map (G.succ g) p)) > 3 then 1000 else 1 in
    (* let wg = 1 in *)
    itrlw wg ids
  in
  let iteration v = 
    l := (vids v, L.concat [succ v; pred v]) :: !l in
  G.iter_vertex iteration g;
  sort !l

let out_metis vc ec adj outf =
  let head  = (soi vc) ^ " " ^ (soi ec) ^ " 1\n" in
  let body1 = L.map (fun (_,l)-> interleave " " (L.map soi l)) adj in
  let body2 = L.concat (interleave ["\n"] body1) in
  let body  = String.concat "" body2 in
  let o = head ^ body in
  output_file outf o

let output gg met dot =
  let vc  = vertex_count gg in
  let ec  = edge_count gg in
  let rg  = reenumgraph gg in
  let adj = metisadj rg in
  out_metis vc ec adj met;
  output_graph rg dot;
  rg

let metis_parts inf =
  let ios = int_of_string in
  let partlist =
    let fd = open_in inf in
    BatStd.input_list fd in
  let ordlist = 
    init (L.length partlist) (fun x -> x+1) in
  L.map2 (fun x y -> (ios y),x)
    ordlist partlist;;

let format_parts parts = 
  let ordparts = group (fun (x,_) (y,_) -> Pervasives.compare x y) parts in
  let ordstrpt = L.map (fun x -> interleave " " <| L.map (snd |- soi) x) ordparts in
  L.map (String.concat "") ordstrpt

let out_parts frmpts outf = 
  let o = String.concat "" <| interleave ~last:"\n" "\n" frmpts in
  output_file outf o

(* let in_ () = metis_parts "metis.txt.part.4" *)
(* let out() = output gg "metis.txt" "xxx.dot" *)
