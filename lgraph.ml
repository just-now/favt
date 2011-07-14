(* #load "utils.cmo" *)
(* #load "syntax.cmo" *)

open Utils
open Syntax
open Asttree
open Graph
open Path

(* --- Types --- *)
exception PGraphException of string
type binop = BiPlus | BiMinus | BiTimes
type unop  = UnOut | UnAbs

type lid = int
and llabel =
    PInt   of int    * lid
  | PVar   of string * lid
  | PFree  of lid
  | PBinOp of binop * lid
  | PUnOp  of unop  * lid
  | PNone

let bo2s = function
    BiPlus  -> "+"
  | BiMinus -> "-"
  | BiTimes -> "*"
let uo2s = function
    UnOut -> "=>"
  | UnAbs -> "|.|"

(* Produce vertex labels *)
let rec vlbls = function
    PInt (i,id)   -> "(" ^ (soi id) ^ ")i=" ^ (soi i)
  | PVar (s,id)   -> "(" ^ (soi id) ^ ")v=" ^ s
  | PBinOp (o,id) -> "(" ^ (soi id) ^ ")(" ^ (bo2s o) ^ ")"
  | PUnOp (o,id) -> "(" ^ (soi id) ^ ")(" ^ (uo2s o) ^ ")"
  | PFree  id     -> soi id
  | PNone  -> raise (PGraphException "None is added to labels!")

let rec trid id v = match v with
    PInt (i,_)   -> PInt (i,id)
  | PVar (s,_)   -> PVar (s,id)
  | PBinOp (o,_) -> PBinOp (o,id)
  | PUnOp (o,_)  -> PUnOp (o,id)
  | PFree  _     -> PFree id
  | PNone  -> raise (PGraphException "None is added to labels!")

let rec vids = function
    PInt (_,id) | PVar (_,id) -> id
  | PBinOp (_,id) -> id | PUnOp (_,id) -> id | PFree id -> id
  | PNone  -> raise (PGraphException "None is added to labels!")
(* --- /Types --- *)

module L = List
module H = Hashtbl
(* ------------------- modules ---------------------------- *)
module V = struct
  type t = llabel
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end
module E = struct
  type t = int
  let compare = Pervasives.compare
  let default = 0
end
module G = Imperative.Digraph.ConcreteLabeled(V)(E)
module W = struct
  type label = int
  type t = int
  let weight x = x
  let compare = Pervasives.compare
  let add = (+)
  let zero = 0
end
module Dij = Path.Dijkstra(G)(W)
module O = Oper.I(G)
module N = Oper.Neighbourhood(G)
(* ------------------- /modules ---------------------------- *)
(* ------------------- output ---------------------------- *)
module Display = struct
  include G
  let vertex_name v = "\"" ^ String.escaped (vlbls (V.label v)) ^ "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end
module DotOutput = Graphviz.Dot(Display)
let output_graph g f =
  let oc = open_out f in
  DotOutput.output_graph oc g;
  close_out oc
(* ------------------- /output ---------------------------- *)
type lgraph = {
    graph : G.t;
    id2v : (lid, G.V.t) H.t;
    id2l : (lid, llabel) H.t;
  } 

let mklgraph () = {
  graph=G.create();
  id2v = H.create 0;
  id2l = H.create 0;
}

let outp g f = output_graph g.graph f

let addv g lbl =
  let v = G.V.create lbl in
  H.add g.id2v (vids lbl) v;
  H.add g.id2l (vids lbl) lbl;
  G.add_vertex g.graph v

let adde g v1 v2 =
  G.add_edge_e g.graph
    (G.E.create (H.find g.id2v v1) 1 (H.find g.id2v v2))

let succv g v =
  G.succ g.graph (H.find g.id2v v)

let rme g e =
  G.remove_edge_e g.graph e

let rmv g v = 
  G.remove_vertex g.graph (H.find g.id2v v)

let gsp g v1 v2 = Dij.shortest_path g.graph
    (H.find g.id2v v1)
    (H.find g.id2v v2)

(* create graph of grid *)
let mkGrid g w h =
  let adde' g v1 v2 = adde g (vids v1) (vids v2) in
  let cs i j = PFree (j+10*i) in
  let w' = w - 1 in
  let w''= w'- 1 in
  let h' = h - 1 in
  let h''= h'- 1 in
(* vertexes *)
  for i=0 to h' do
    print_string "{rank=same;";
    for j=0 to w' do
      print_string (vlbls(cs j i) ^ "; ");
      addv g (cs j i)
    done;
    print_string "}\n"
  done;
(* vertical edges *)
  for i=0 to h' do
    for j=0 to w'' do
      adde' g (cs i j) (cs i (j+1));
      adde' g (cs i (j+1)) (cs i j)
    done
  done;
(* horisontal edges *)
  for i=0 to h'' do
    for j=0 to w' do
      adde' g (cs i j) (cs (i+1) j);
      adde' g (cs (i+1) j) (cs i j)
    done
  done

(* --- main graph producer --- *)
let rec makeGraph g id e =
  let proc_term l = addv g l; id+1 in
  let proc_2ix1o a b l =
    let adde' = match l with PNone -> (fun x y -> ()) | _ -> adde g in
    let addv' = match l with PNone -> (fun x   -> ()) | _ -> addv g in
    addv' l;
    let id'  = makeGraph g (id+1) a in
    let id'' = makeGraph g (id')  b in
    adde' (id+1) id; adde' id' id; id'' in
  let oproc_2ix1o a b l = proc_2ix1o a b l in
  match e with
    Nil         -> id+1
  | Int a       -> proc_term (PInt (a,id))
  | Var a       -> proc_term (PVar (a,id))
  | Cons (a,b)  -> oproc_2ix1o a b PNone
  | Plus (a,b)  -> oproc_2ix1o a b (PBinOp (BiPlus ,id))
  | Minus (a,b) -> oproc_2ix1o a b (PBinOp (BiMinus,id))
  | Times (a,b) -> oproc_2ix1o a b (PBinOp (BiTimes,id))
  | Out (a)     ->
      addv g (PUnOp (UnOut,id)); let id' = makeGraph g (id+1) a in adde g (id+1) id; id'
  | _ -> raise (PGraphException "_")

let pvars g = L.filter (fun (x,y) -> match y with
  PVar (_,_) -> true | _ -> false) (h2l g.id2l)

let pvgroups g = BatList.group (fun x y -> match (x,y) with
  ((_,PVar(a,_)), (_,PVar(b,_))) -> Pervasives.compare a b
| _ -> raise (PGraphException "_")) (pvars g)

let pvgroupids g = L.map (fun l -> L.map fst l) (pvgroups g)
let pvgids2cozy g = L.map (fun x -> (L.hd x, L.tl x)) (pvgroupids g)
let edgesof g vid = G.succ_e g.graph (H.find g.id2v vid)
let pvid_lofe g = L.map (fun (x,y)-> (x,L.concat (L.map (edgesof g) y))) (pvgids2cozy g)
let pvid_lofe_ff g = L.filter (fun (_,y) -> y!=[]) (pvid_lofe g)

let edges2add g = 
  L.map (fun (x,y) -> x,
    L.map (fun (p,q,r) ->
      match p with
	PVar(a,b) -> (PVar(a,x),q,r)
      | _ -> raise (PGraphException "!")) y) (pvid_lofe_ff g)

let rmdupvarv g = 
  let rmdup id =
    (* H.remove g.id2v id; *)
    (* H.remove g.id2l id; *)
    rmv g id
  in
  L.map (fun (x,y) -> L.map rmdup y) (pvgids2cozy g)

(* --- main graph fixer: remove duplicate variables --- *)
let fixgraphv g =
  let es = edges2add g in
  let _ = rmdupvarv g in
  L.map (fun (x,y) -> L.map (G.add_edge_e g.graph) y) es

(* --- Test data --- *)
let test_exp =
  Cons (Out (Plus (Plus (Var "d1", Times (Var "W02", Var "d3")),
       Times (Var "W04", Plus (Var "d2", Times (Var "W02", Var "d4"))))),
   Cons (Out (Plus (Minus (Var "d1", Times (Var "W02", Var "d3")),
        Times (Var "W14", Minus (Var "d2", Times (Var "W02", Var "d4"))))),
    Cons (Out (Minus (Plus (Var "d1", Times (Var "W02", Var "d3")),
         Times (Var "W04", Plus (Var "d2", Times (Var "W02", Var "d4"))))),
     Cons (Out (Minus (Minus (Var "d1", Times (Var "W02", Var "d3")),
          Times (Var "W14", Minus (Var "d2", Times (Var "W02", Var "d4"))))), Nil))))

(* --------------------- *)
exception IsNotBinOp;;
(* get binary operation from tree *)
let gbop = function 
    Plus  (e1,e2) -> (Plus (Nil,Nil), e1, e2)
  | Minus (e1,e2) -> (Minus(Nil,Nil), e1, e2)
  | Times (e1,e2) -> (Minus(Nil,Nil), e1, e2)
  | _ -> raise IsNotBinOp;;

let idfy_4x4in_4out e = match e with
    Cons(Out(e1), Cons(Out(e2), Cons(Out(e3), Cons(Out(e4), Nil)))) ->
      let (op1,a1,b1), (op2,a2,b2), (op3,a3,b3), (op4,a4,b4) = 
	gbop e1, gbop e2, gbop e3, gbop e4 in
	Identified([Out op1; Out op2; Out op3; Out op4],
		   [[a1;b1]; [a2;b2]; [a3;b3]; [a4;b4]])
  | Cons(e1, Cons(e2, Cons(e3, Cons(e4, Nil)))) ->
      let (op1,a1,b1), (op2,a2,b2), (op3,a3,b3), (op4,a4,b4) = 
	gbop e1, gbop e2, gbop e3, gbop e4 in
	Identified([op1; op2; op3; op4], [[a1;b1]; [a2;b2]; [a3;b3]; [a4;b4]])
  | _-> e;;

let l2id = function
    PInt   (_,i)  | PVar   (_,i)  | PBinOp (_,i)  | PUnOp  (_,i) -> i
  | PFree  i -> i
  | PNone -> raise (PGraphException "l2id!")
