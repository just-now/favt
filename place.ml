(* let save_struct variants dx dy = *)
(*   let v = field_variant1 variants dx dy in *)
(*   let o = BatPervasives.open_out "m.txt" in *)
(*   BatPervasives.output_value o v; *)
(*   BatPervasives.close_out o *)

(* let load_variants () = *)
(*   let i = BatPervasives.open_in "m.txt" in *)
(*   let v = BatPervasives.input_value i in *)
(*   BatPervasives.close_in i; *)
(*   v *)

#load "utils.cmo"
#load "lgraph.cmo"

open BatStd
open Lgraph
open Utils
module L = List
module A = Array
module BL = BatList

type cell_fan = { u:llabel; d:llabel; l:llabel; r:llabel }
type cell_prop = PPArith | PPInOut
type cell = {
    mutable i    : cell_fan;
    mutable o    : cell_fan;
    mutable op   : llabel;
    mutable prop : cell_prop
  }

type field = cell array array

type rcu = {
    mutable f : field;
    mutable g : lgraph
  }

let fan_create() = { u = PNone; d = PNone; l = PNone; r = PNone }
let cell_create() = { i=fan_create(); o=fan_create(); op = PNone; prop = PPArith }
let field_create dx' dy' =
  let dx = dx' - 1 and dy = dy' - 1 in
  let f = A.create_matrix dx' dy' (cell_create()) in
  for y = 0 to dy do
    for x = 0 to dx do
      f.(y).(x) <- cell_create();
      if x = 0 || y = 0 || x = dx || y = dy then
	f.(y).(x).prop <- PPInOut
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
  let nb = [ (x-1,y-1); (x,y-1); (x+1,y-1);
	     (x-1,y); (x+1,y);
	     (x-1,y+1); (x,y+1); (x+1,y+1)] in
  L.filter (valid f) nb

let neighbours2 f (x,y) = 
  let nb = neighbours f (x,y) in
  L.map (neighbours f) nb |> L.concat

(* variants of placement of 1 cell in row *)
(* c2p  - cells to place     *)
(* dimx - row elements count *)
let row_variants1 c2p dimx =
  let free = 0 in
  (* let is_free c = c = free in *)
  let variants = ref [] in
  for i = 0 to (dimx-1) do
    for j = 0 to ((L.length c2p)-1) do
      let row = A.init dimx (fun x -> free) in
      let var = L.nth c2p j in
      row.(i) <- var;
      variants :=  !variants @ [var,(A.to_list row)]
    done
  done;
  !variants

type 'a vartree = VVar of 'a | VSet of 'a vartree list
let rec field_variant1 c2p dimx dimy =
  let vs = row_variants1 c2p dimx in
  VSet(L.map (fun v ->
    let c2p' = BL.remove c2p (fst v) in
    let fvr' = field_variant1 c2p' dimx dimy in
    VSet [VVar v; fvr'] ) vs)

(* lev   - level of tree *)
(* f     - applied function *)
(* t     - k-ary tree *)
(* facc  - accumulator passed to f *)
let rec trv f lev t =
  match t with
     VSet l -> VSet (L.map (trv f (lev+1)) l)
  |  VVar a -> VVar (f lev a)

let pack_minmax c2p dx dy =
  let var = field_variant1 c2p dx dy in
  let l = ref [] in
  let iter lev v = l := lev :: !l in
  let _ = trv iter 0 var in
  BL.min(!l), BL.max(!l), var

let pack1() =
  let minl,maxl,var = pack_minmax [1;2;3;4] 4 4 in
  let pack   = ref [] in
  let packs  = ref [] in
  let prev   = ref minl in
  let iter lev v =
    (* if lev < !prev then pack := List.tl !pack; *)
    (* if lev = minl then pack := []; *)
    let poptimes = (!prev - lev) / 2 in
    if poptimes > 0 then pack := iterate (L.tl) (poptimes-1) (!pack);
    (* if !prev - lev = 2 then pack := List.tl !pack; *)
    (* if !prev - lev = 4 then pack := List.tl(List.tl !pack); *)
    if !pack = [] then
      pack := (lev,v) :: !pack
    else
      if lev = maxl then (
	pack := (lev,v) :: !pack;
	packs := !pack :: !packs;
	pack := L.tl !pack;
	) else
	let packed_lev = fst (L.hd !pack) in
	  if lev = packed_lev then
	    pack := L.tl !pack;
	  
	pack := (lev,v) :: !pack;
	prev := lev
  in
  let _ = trv iter 0 var in
  L.rev !packs

let output_blocks() = 
  let format_block b =
    let tostr1 = L.map ( (L.map soi) |- BL.interleave " ") b in
    let tostr2 = BL.interleave ~last:["\n-------------\n"] ["\n"] tostr1 in
    String.concat "" (L.concat tostr2) in
  let cells = L.map (fun x -> L.map (snd |- snd) x) (L.map L.rev (pack1())) in
  let lscells = L.map format_block cells in
  let s = String.concat "" lscells in
  output_file "xxx.test" s
  
