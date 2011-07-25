#load "utils.cmo"
#load "lgraph.cmo"

open BatStd
open Lgraph
open Utils

module L = List
module A = Array
module BL = BatList

exception PlaceException of string
let raise_ s = raise (PlaceException s)

type cell_fan = { u:llabel; d:llabel; l:llabel; r:llabel }
type cell_type = PPArith | PPInOut
type cell_prop = { mutable t:cell_type; mutable id:string; mutable x : int; mutable y : int }
type cell = {
    mutable i    : cell_fan;
    mutable o    : cell_fan;
    mutable op   : llabel;
    mutable prop : cell_prop
  }

type field = cell array array

type rcu = {
    mutable f : field;
    mutable g : lgraph;
  }

let fan_create() = { u = PFree 0; d = PFree 11; l = PFree 10; r = PFree 1}
let cell_create() = { i=fan_create(); o=fan_create(); op = PFree 42; prop = {t=PPArith; id="00"; x=0;y=0} }
let field_create dx' dy' =
  let dx = dx' - 1 and dy = dy' - 1 in
  let f = A.create_matrix dx' dy' (cell_create()) in
  for y = 0 to dy do
    for x = 0 to dx do
      f.(y).(x) <- cell_create();
      if x = 0 || y = 0 || x = dx || y = dy then
	f.(y).(x).prop.t <- PPInOut;
	f.(y).(x).prop.id <- (soi y) ^ (soi x);
	f.(y).(x).prop.x <- x;
	f.(y).(x).prop.y <- y
    done
  done;
  f

let rcu_create dx dy = {
  f = field_create dx dy;
  g = let g' = mklgraph() in mkGrid g' dx dy; g'
}

let valid f (x,y) =
  let dy = A.length f and dx = A.length f.(0) in
  if x < 0 || y < 0 || x >= dx || y >= dy then false else true

let neighbours f (x,y) =
  (* octogonal *)
  let nb = [(x,y-1); (x-1,y); (x+1,y); (x,y+1);
	     (x-1,y-1); (x+1,y-1);
	     (x-1,y+1); (x+1,y+1)] in

  (* 4-i/o cells *)
  (* let nb = [ (x,y-1); (x-1,y); (x+1,y); (x,y+1)] in   *)
  L.filter (valid f) nb

let neighbours2 f (x,y) = 
  let nb = neighbours f (x,y) in
  BL.unique -| L.concat <| L.map (neighbours f) nb

let test_graph = 
  let g = load_struct "dft8-graph.dat" in
  (g : Lgraph.G.t)
let test_graph_parts = 
  let g = load_struct "dft8-parts.dat" in
  (g : (int * Lgraph.lid) list)
  
let rcu8x8 = rcu_create 8 8

let is_cell_free c = match c.op with PFree _ -> true | _ -> false
let cellxy rcu (x,y) = rcu.f.(y).(x)

(* @rcu, @g - praph to place, @p - parts to place *)
let place rcu' g p group_id =
  (* local utils *)
  let rf = rcu'.f in
  let rg = rcu'.g in
  let succ = G.succ g in
  let pred = G.pred g in
  let nbrs v = L.concat [succ v; pred v] in
  let lle = L.length in
  let cxy c = c.prop.x, c.prop.y in
  let deg v = lle (succ v) + lle (pred v) in
  let cell_from_xy xy cs =
    L.find (fun c -> cxy c = xy) cs in

  (* @v2p - vertexes to place *)
  (* @c2p - list of cells *)
  let v2p = L.map (id2v' g -| snd) (L.filter (fun (g,_) -> g = group_id) p) in
  let c2p = let ll = ref [] in A.iter (fun cs -> A.iter (fun c -> ll:=!ll@[c]) cs) rcu'.f; !ll in

  (* find most connected vertex in given vs *)
  let most_connected vs = 
    let vvs = L.map (fun v -> deg v, v) vs in
    let maxdeg = BL.max <| L.map fst vvs in
    L.filter (fun (d,_) -> d = maxdeg) vvs in

  (* v, (suc_level_count, pred_lev_count) *)
  let vert_radius v =
    (* s_o_p -- succ prediacte or pred -- predicate *)
    let rec all_conn_lev s_o_p lev v =
      let s = s_o_p v in 
      if s = [] then lev else BL.max <| L.map (all_conn_lev s_o_p (lev+1)) s in
    (all_conn_lev succ 0 v, all_conn_lev pred 0 v),v in

  (* find best vertex to start with and its initial coords *)
  let best_vxy = 
    let succ_pred2v  = L.map (vert_radius -| snd) (most_connected v2p) in
    let v2succ_pred = lxy2yx succ_pred2v in
    let range_ballace2v' = L.map (fun ((x,y),v) -> (x+y, abs (x-y)),v) succ_pred2v in
    let max_ballance = BL.max <| L.map (snd -| fst) range_ballace2v' in
    let range_ballace2v = L.map (fun ((r,b),o) -> ((r,max_ballance - b),o)) range_ballace2v' in
    let max_ballance = BL.max <| L.map (snd -| fst) range_ballace2v in
    let max_range    = BL.max <| L.map (fst -| fst) range_ballace2v in
    let rv  = L.assoc (max_range,max_ballance) range_ballace2v in
    let rxy = L.assoc rv v2succ_pred in
    rv,rxy
  in

  (* returns available cell neighbours for current configuration *)
  let xisin tran x xs = L.fold_left (||) false
      <| L.map (fun x' -> tran x' = x) xs in
  let xisntin tran x xs = not (xisin tran x xs) in
  let free_c_nbrs f xy cs =
    let nb' = neighbours f xy in
    L.filter (fun n -> xisin cxy n cs) nb'
  in

  (* sort closeset io by distance *)
  let iscio c = match c.prop.t with PPInOut -> true | _ -> false in
  let isvio v = match v with PVar (_,_) -> true | PUnOp (UnOut,_) -> true | _ -> false in
  let lg_xy2v (x,y) = x+10*y in 
  let closest_ios c gsp' cs =
    let dist xy1 xy2 = 
      match gsp' rg (lg_xy2v xy1) (lg_xy2v xy2) with ([],_) -> raise_ "ASSERT: no available IO" | (l,w) -> w in
    let io = L.filter iscio cs in
    let d2io = L.map (fun i -> dist (cxy c) (cxy i), i) io in
    let d2io' = L.sort (fun (d1,_) (d2,_) -> compare d1 d2) d2io in
    L.map snd d2io'
  in

  (* route and place io if needed, return (v_to,xy_to) if ok or exception *)
  (* if (v_to,xy_to) is io -- then place appropriate io *)
  let route vs cs (v_from,xy_from) (v_to,xy_to) = 
    let c_from = cell_from_xy xy_from c2p in (* c2p or cs ???? *)
    let c_to   = cell_from_xy xy_to   c2p in
    let dir =
      if L.mem v_from (pred v_to) then true  else
      if L.mem v_from (succ v_to) then false else
      raise_ "ASSERT: unbound vertex is along the vertexes" in
    let gsp' g _from _to = if dir then gsp g _from _to else gsp g _to _from in
    if isvio v_from then []
    else
      if isvio v_to then
	let closest = L.hd <| closest_ios c_from gsp' cs in
	let xy_to' = cxy closest in
	let path = fst <| gsp' rg (lg_xy2v xy_from) (lg_xy2v xy_to') in
	if path = []  then raise_ "ASSERT: unable to route!";
	L.map (rme rg) path; (* remove path from cells graph *)
	[(v_from,xy_from),(v_to,xy_to')]
      else
	let path = fst <| gsp' rg (lg_xy2v xy_from) (lg_xy2v xy_to) in
	if path = []  then raise_ "ASSERT: unable to route!";
	L.map (rme rg) path; (* remove path from cells graph *)
	[(v_from,xy_from),(v_to,xy_to)]
  in

  (* vertex_place vertex @v with @xy with available vertexes @vs and in cells @cs *)
  let rec vertex_place (v,xy) vs cs = 
    let c = cell_from_xy xy cs in
    let vs' = BL.remove vs v in
    let cs' = BL.remove cs c in
    let nbs = nbrs v in
    let c_nbs' = free_c_nbrs rf xy cs in
    let c_nbs  = BL.take (lle nbs) c_nbs' in
    let placed = BL.combine nbs c_nbs in
    let res = L.concat <| L.map (route vs' cs' (v,xy)) placed in
    let vsr = L.map (fst -| snd) res in
    let csr = L.map ((flip cell_from_xy) cs') <| L.map (snd -| snd) res in
    (* let cs'' = L.filter ((flip (xisntin identity)) csr) cs' in *)
    (* let vs'' = L.filter ((flip (xisntin identity)) vsr) vs' in *)
    res,(vs',cs')
  in
  let rec step_iter (r,(v,c)) acc =
    if r = [] then acc
    else 
      let vxy = snd <| L.hd r in
      let r' = L.tl r in
      let (r'',(v',c')) = vertex_place vxy v c in
      step_iter (r',(v',c')) (acc@r'')
  in
  let (r,(v,c)) = vertex_place best_vxy v2p c2p in
  step_iter (r,(v,c)) []
	       

(* --- *)
let va = place rcu8x8 test_graph test_graph_parts 0
