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

let valid f (x,y) =
  let dy = A.length f and dx = A.length f.(0) in
  if x < 0 || y < 0 || x >= dx || y >= dy then false else true

let neighbours f (x,y) =
  let nb = [ (x-1,y-1); (x,y-1); (x+1,y-1);
	     (x-1,y); (x+1,y);
	     (x-1,y+1); (x,y+1); (x+1,y+1)] in
  L.filter (valid f) nb

(* let place f g = *)
(*   let (g',f') = block_place f g in *)
