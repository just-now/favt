(* #load "parser.cmo" *)
(* #load "lexer.cmo" *)
(* #load "utils.cmo" *)
(* #load "syntax.cmo" *)
(* #load "asttree.cmo" *)
(* #load "lgraph.cmo" *)
(* #load "partition.cmo" *)

open Utils
open Syntax
open Asttree
open Lgraph
open Partition
module LG = Lgraph
module ES = BatString
module BL = BatList.Exceptionless
exception Toplevel
(* ------------ Utils ------------------------------ *)

let isADef    = function Syntax.Def (_,_)  -> true | _ -> false
let isAnExp   = function Syntax.Expr (_)   -> true | _ -> false
let isACDef   = function Syntax.CDef (_,_) -> true | _ -> false
let isAFunDef = function (_,Fun(_,_,_,_))  -> true | _ -> false;;

let iscomment s =
  try
    let cl = 2 and
	po = ES.find s "(*" and
	pl = ES.find s "*)" in
    let start = String.sub s po cl and
	stop = String.sub s pl cl in
    match (start, stop) with ("(*","*)") -> true | _ -> false
  with Invalid_argument _ | Not_found -> false
(* ------------ /Utils ------------------------------ *)
(* knormalizer *)
let rec knorm v vn e = 
  let rec knorm' e = 
    match e with
      Var n             -> if n = v then Var vn else Var n
    | Int _ as x        -> x
    | Bool _ as x       -> x
    | Args le           -> Args(List.map knorm' le)
    | Times (e1,e2)     -> Times (knorm' e1, knorm' e2)
    | Plus (e1,e2)      -> Plus (knorm' e1, knorm' e2)
    | Minus (e1,e2)     -> Minus (knorm' e1, knorm' e2)
    | Equal (e1,e2)     -> Equal (knorm' e1, knorm' e2)
    | Less (e1,e2)      -> Less (knorm' e1, knorm' e2)
    | More (e1,e2)      -> More (knorm' e1, knorm' e2)
    | Or (e1,e2)        -> Or (knorm' e1, knorm' e2)
    | And (e1,e2)       -> And (knorm' e1, knorm' e2)
    | If (p,e1,e2)      -> If (knorm' p, knorm' e1, knorm' e2)
    | Fun (n,a,t,e)     -> Fun (n,a,t, knorm' e)
    | Apply (e1,e2)     -> Apply (knorm' e1, knorm' e2)
    | Cons (e1,e2)      -> Cons (knorm' e1, knorm' e2)
    | Nil               -> Nil       
    | Fst e             -> Fst (knorm' e)
    | Snd e             -> Snd (knorm' e)
    | Out e             -> Out (knorm' e)
    | Let (n,e1,e2)     -> Let (n, knorm' e1, knorm' e2)
    | Identified (_, _) -> raise Toplevel
  in
  knorm' e

let ldefs = ref []
let unq' = ref 0

(* main evaluator *)
let rec eval1 d expression = match expression with
    Identified (_,_) -> raise Unimplemented
  | (Int _  | Bool _ | Var _ | Args _) as exp -> exp
  | Minus (a, Int 0) -> a (* optimisation *)
  | Plus (a, Int 0) -> a  (* optimisation *)
  | Plus (Int 0, b) -> b  (* optimisation *)
  | Times (Int 0, b) -> b  (* optimisation *)
  | Times (a, Int 0) -> a (* optimisation *)
  | Times (Int a, Int b) ->  Int (a*b) 
  | Plus  (Int a, Int b) ->  Int (a+b)
  | Minus (Int a, Int b) ->  Int (a-b)
  | Times (a,b) -> Times (eval1 d a, eval1 d b)
  | Plus (a,b) -> Plus (eval1 d a, eval1 d b)
  | Minus (a,b) -> Minus (eval1 d a, eval1 d b)
  | Equal ((Int a), (Int b)) -> Bool (a=b)
  | Equal (Nil, Nil) -> Bool true
  | Equal (Var _, Nil) -> Bool false
  | Equal (Nil, Var _) -> Bool false
  | Equal (Cons(_,_), Nil) -> Bool false
  | Equal (Nil, Cons(_,_)) -> Bool false
  | Equal (a,b) -> Equal(eval1 d a, eval1 d b)
  | Less ((Int a), (Int b)) -> Bool (a < b)
  | Less (a,b)  -> Less (eval1 d a, eval1 d b)
  | More ((Int a), (Int b)) -> Bool (a > b)
  | More (a,b)  -> More (eval1 d a, eval1 d b)
(* guards: *)
  | And (Args a, b) -> let _ = List.tl a in And((List.hd a), b)
  | And (a, Args b) -> let _ = List.tl b in And(a, (List.hd b))
  | Or (Args a, b) -> let _ = List.tl a in Or((List.hd a), b)
  | Or (a, Args b) -> let _ = List.tl b in Or(a, (List.hd b))
(* /guards: *)
  | And ((Bool a), (Bool b)) -> Bool (a && b)
  | And (a,b)  -> And (eval1 d a, eval1 d b)
  | Or ((Bool a), (Bool b)) -> Bool (a || b)
  | Or (a,b)  -> Or (eval1 d a, eval1 d b)
  | If (Bool true, t, e) -> eval1 d t
  | If (Bool false, t, e) -> eval1 d e
  | If (p,t,e) -> If (eval1 d p, t, e)
  | Fun (n,a,t,e) -> Fun (n,a,t, eval1 [] e)
  | Apply (Fun(n,an,_,e),Args(av)) ->
      let d1 = zipWith2 (fun x y -> ((fst x), y)) an av in
      let d' = d1 @ d in
      (* let e' = eval1 [] e in *)
      subst d' e
  | Apply (b,a) as exp -> exp
  | Cons (Args a, b) -> let _ = List.tl a in Cons((List.hd a), b)
  | Cons (a, Args b) -> let _ = List.tl b in Cons(a, (List.hd b))
  | Cons (a,b) -> Cons(eval1 d a, eval1 d b)
  | Nil -> Nil
  | Fst Nil -> Nil
  | Fst (Args (a)) -> let _ = List.tl a in Fst(List.hd a)
  | Snd (Args (a)) -> let _ = List.tl a in Snd(List.hd a)
  | Fst (Cons (a, b)) -> a
  | Snd (Cons (a, b)) -> b
  | Fst (e) -> Fst (eval1 d e)
  | Snd (e) -> Snd (eval1 d e)
  | Out (Cons (a,b)) -> Cons (Out(eval1 d a), Out(eval1 d b))
  | Out Nil -> Nil
  | Out a -> Out (eval1 d a)
  | Let (v,ev,e) -> 
      let unq = unq' := !unq' + 1; !unq' in
      let cev = compileAST ev d in
      match BL.find (fun x -> (snd x) = cev) !ldefs with
	Some (vn, vv) -> let e' = knorm v vn e in eval1 d e'
      |	None ->
	  let vn = v^(soi unq) in
	  ldefs := !ldefs @ [(vn,cev)];
	  let e' = knorm v vn e in eval1 d e'

(* simple until len iterator *)
and compileAST e def =  let e' = iterate (eval1 def) 10 e
			in if e' = e then e else compileAST e' def

(* create subgraphs from [(var,expr)] *)
let rec makeSubGraphs gg lts i = 
  if lts = [] then [] else
  let lts' = List.tl lts in
  let e = List.hd lts in
  let i' = LG.makeGraph gg i (snd e) in
  (fst e,i) :: makeSubGraphs gg lts' i'

(* create subgraphs of substituted variables and duplicates *)
let makeSubGraphsAll gg ldefs ii = 
  let livars = makeSubGraphs gg !ldefs ii in
  let livs = ref [] in
  let _ =
    G.iter_vertex (fun x -> match x with
      PVar (a,b) -> livs:=(a,b)::!livs
    | _ -> ()) gg.graph in
  let vstoconn =
    List.map (fun (vn,vvx) -> vvx,
      List.map snd (List.find_all 
		      (fun (n,_) -> n = vn) !livs)) livars in
  let conngrv g lcon =
    let connvs' x = 
      let v1 = fst x in
      let vl = snd x in
      let reconn' v = LG.adde g v1 v in
      List.map reconn' vl in
    List.map connvs' lcon in
  let cleangr1 gg = 
    G.iter_vertex (fun x -> match x with
      PVar (a,b) ->
	if G.succ gg.graph x = [] then G.remove_vertex gg.graph x
    | _ -> ()) gg.graph in
  (* let _ = LG.fixgraphv gg in *)
  let _ = let _ = conngrv gg vstoconn in cleangr1 gg in
  G.iter_vertex (fun v -> 
    match v with PVar(_,_) -> 
      let s = G.succ gg.graph v in
      let p = G.pred gg.graph v in
      if List.length p == 1 then
	let p1 = List.hd p in
	let _ = List.map (fun ss -> G.add_edge gg.graph p1 ss) s in
	G.remove_vertex gg.graph v
    | _ -> ()) gg.graph

(* replace duplicates in expression tree *)
let rec replaceDuplicates ex dup = 
  let unq = unq' := !unq' + 1; !unq' in
  if dup = [] then ex
  else 
    let varn = "dup"^soi(unq) in
    let dupl = List.hd dup in
    let ex' = replace_sub ex dupl (Var varn) in
    ldefs := !ldefs @ [(varn,dupl)]; replaceDuplicates ex' (List.tl dup)

(* ------------ Main and stuff ------------------------------ *)
let main =
let outfile = ref "" in
let files = ref [] in Arg.parse [("-o", Arg.Set_string outfile, "output file")]
				(fun f -> files := f :: !files)
				"Usage: ./test [-o file] [file]"; ldefs := [];
  let prog_name = List.hd !files in
  let prog_list = let fd = open_in prog_name in BatStd.input_list fd in
  let prog = List.fold_left (^) "" (List.filter (not % iscomment) prog_list) in
  let cmds = Parser.toplevel Lexer.token (Lexing.from_string prog) in
  (* let defs = List.filter isADef cmds in *)
  (* let defs = List.map (fun x -> match x with Syntax.Def (a,b) -> (a,b) | _ -> raise Toplevel) defs in *)
  let cdefs = List.filter isACDef cmds in
  let cdefs = List.map (fun x -> match x with Syntax.CDef (a,b) -> (a,b) | _ -> raise Toplevel) cdefs in
  let exps = List.filter isAnExp cmds in
  let exps = List.map (fun x -> match x with Syntax.Expr (e) -> e | _ -> raise Toplevel) exps in
  let out = List.map (Syntax.subst cdefs) exps in
  let e1 = eval1 cdefs (List.hd out) in
  let ex = compileAST e1 cdefs in
  let gg = LG.mklgraph() in
  let ex' = replaceDuplicates ex (findAstDuplicates ex)  in
  let ii = LG.makeGraph gg 1 ex' in
  let _ = makeSubGraphsAll gg ldefs ii in
  LG.outp gg !outfile

(* ------------------------------------------------------------- *)

(* let prog_name = "dft.favt" *)
(* let prog_list = let fd = open_in prog_name in Std.input_list fd *)
(* let prog = List.fold_left (^) "" (List.filter (not % iscomment) prog_list) *)
(* let cmds = Parser.toplevel Lexer.token (Lexing.from_string prog) *)
(* (\* let defs = List.filter isADef cmds in *\) *)
(* (\* let defs = List.map (fun x -> match x with Syntax.Def (a,b) -> (a,b) | _ -> raise Toplevel) defs in *\) *)
(* let cdefs = List.filter isACDef cmds *)
(* let cdefs = List.map (fun x -> match x with Syntax.CDef (a,b) -> (a,b) | _ -> raise Toplevel) cdefs *)
(* let exps = List.filter isAnExp cmds *)
(* let exps = List.map (fun x -> match x with Syntax.Expr (e) -> e | _ -> raise Toplevel) exps *)
(* let out = List.map (Syntax.subst cdefs) exps *)
(* let e1 = eval1 cdefs (List.hd out) *)
(* let ex = compileAST e1 cdefs *)
(* let gg = LG.mklgraph() *)
(* let ex' = replaceDuplicates ex (findAstDuplicates ex) *)
(* let ii = LG.makeGraph gg 1 ex' *)
(* let jj = makeSubGraphsAll gg ldefs ii *)
(* let kk = LG.outp gg "xxx.dot" *)
(* module P = Partition;; *)
(* let out() = output gg "metis.txt" "xxx.dot" *)
(* let in_ () = metis_parts "metis.txt.part.4" *)
(* let newgg = out();; *)
(* let rc = Sys.command ("./tools/metis/kmetis " ^ "metis.txt " ^ (soi 4));; *)
(* let parts = in_();; *)
(* let _ = out_parts (format_parts parts) "metis.groups";; *)

