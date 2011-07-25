(* open Lgraph *)
(* open Utils *)
(* open Place *)
(* module A = Array *)

let draw rcu' outf =
  let dy = A.length rcu'.f and dx = A.length rcu'.f.(0) in
  let cell_dim = 100 in
  let width  = cell_dim*dx in
  let height = cell_dim*dy in
  let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
  let ctx = Cairo.create surface in
  let draw_cell c x y =
    let cdf = float_of_int cell_dim in
    let offx = cdf *. (float_of_int x) in
    let offy = cdf *. (float_of_int y) in
    let arr_up x' y' s =
      let x = float_of_int x' in
      let y = float_of_int y' in
      Cairo.move_to ctx   (offx +. x)	     (offy +. y) ;
      Cairo.line_to ctx   (offx +. x +. 2.5) (offy +. y +. 8.) ;
      Cairo.line_to ctx   (offx +. x -. 2.5) (offy +. y +. 8.) ;
      Cairo.line_to ctx   (offx +. x)	     (offy +. y) ;
      Cairo.set_source_rgb ctx 1. 0. 0.;
      Cairo.show_text ctx s;
      Cairo.set_source_rgb ctx 0. 0. 0.;
    in
    let arr_dn x' y' s =
      let x = float_of_int x' in
      let y = float_of_int y' in
      Cairo.move_to ctx   (offx +. x)	    (offy +. y) ;
      Cairo.line_to ctx   (offx +. x +. 2.5) (offy +. y -. 8.) ;
      Cairo.line_to ctx   (offx +. x -. 2.5) (offy +. y -. 8.) ;
      Cairo.line_to ctx   (offx +. x)	    (offy +. y) ;
      Cairo.set_source_rgb ctx 1. 0. 0.;
      Cairo.show_text ctx s;
      Cairo.set_source_rgb ctx 0. 0. 0.;
    in
    let arr_lt x' y' s =
      let x = float_of_int x' in
      let y = float_of_int y' in
      Cairo.move_to ctx   (offx +. x)	    (offy +. y) ;
      Cairo.line_to ctx   (offx +. x -. 8.) (offy +. y -. 2.5) ;
      Cairo.line_to ctx   (offx +. x -. 8.) (offy +. y +. 2.5) ;
      Cairo.line_to ctx   (offx +. x)	    (offy +. y) ;
      Cairo.set_source_rgb ctx 1. 0. 0.;
      Cairo.show_text ctx s;
      Cairo.set_source_rgb ctx 0. 0. 0.;
    in
    let arr_rt x' y' s =
      let x = float_of_int x' in
      let y = float_of_int y' in
      Cairo.move_to ctx   (offx +. x)	    (offy +. y) ;
      Cairo.line_to ctx   (offx +. x +. 8.) (offy +. y -. 2.5) ;
      Cairo.line_to ctx   (offx +. x +. 8.) (offy +. y +. 2.5) ;
      Cairo.line_to ctx   (offx +. x)	    (offy +. y) ;
      Cairo.set_source_rgb ctx 1. 0. 0.;
      Cairo.show_text ctx s;
      Cairo.set_source_rgb ctx 0. 0. 0.;
    in
    (* bounds *)
    Cairo.move_to ctx   (offx +. 20.)	(offy +. 20.) ;
    Cairo.line_to ctx   (offx +. 20.)	(offy +. 80.) ;
    Cairo.line_to ctx   (offx +. 80.)	(offy +. 80.) ;
    Cairo.line_to ctx   (offx +. 80.)	(offy +. 20.) ;
    Cairo.line_to ctx   (offx +. 20.)	(offy +. 20.) ;
    Cairo.set_source_rgb ctx 0. 0. 1.;
    Cairo.show_text ctx c.prop.id;
    Cairo.set_source_rgb ctx 0. 0. 0.;
    (* inner bounds *)
    Cairo.move_to ctx   (offx +. 40.)	(offy +. 40.) ;
    Cairo.line_to ctx   (offx +. 40.)	(offy +. 60.) ;
    Cairo.line_to ctx   (offx +. 60.)	(offy +. 60.) ;
    Cairo.line_to ctx   (offx +. 60.)	(offy +. 40.) ;
    Cairo.line_to ctx   (offx +. 40.)	(offy +. 40.) ;
    Cairo.move_to ctx   (offx +. 40.)	(offy +. 50.) ;
    Cairo.show_text ctx (vlbls c.op);
    (* arrows *)
    (* arr_up 40 0; *)
    Cairo.move_to ctx   (offx +. 40.)	(offy +. 20.) ;
    Cairo.line_to ctx   (offx +. 40.)	(offy +. 0.) ;
    arr_dn 60 20 (vlbls c.o.d);
    Cairo.move_to ctx   (offx +. 60.)	(offy +. 20.) ;
    Cairo.line_to ctx   (offx +. 60.)	(offy +. 0.) ;

    arr_up 40 80 (vlbls c.i.u);
    Cairo.move_to ctx   (offx +. 40.)	(offy +. 80.) ;
    Cairo.line_to ctx   (offx +. 40.)	(offy +. 100.) ;
    (* arr_dn 60 100; *)
    Cairo.move_to ctx   (offx +. 60.)	(offy +. 80.) ;
    Cairo.line_to ctx   (offx +. 60.)	(offy +. 100.) ;

    arr_rt 80 40 (vlbls c.i.r);
    Cairo.move_to ctx   (offx +. 80.)	(offy +. 40.) ;
    Cairo.line_to ctx   (offx +. 100.)	(offy +. 40.) ;
    (* arr_lt 100 60; *)
    Cairo.move_to ctx   (offx +. 80.)	(offy +. 60.) ;
    Cairo.line_to ctx   (offx +. 100.)	(offy +. 60.) ;

    (* arr_rt 0 40; *)
    Cairo.move_to ctx   (offx +. 20.)	(offy +. 40.) ;
    Cairo.line_to ctx   (offx +.  0.)	(offy +. 40.) ;
    arr_lt 20 60 (vlbls c.o.l);
    Cairo.move_to ctx   (offx +. 20.)	(offy +. 60.) ;
    Cairo.line_to ctx   (offx +.  0.)	(offy +. 60.) ;
  in
  let f = rcu'.f in
  for y = 0 to (dy-1) do
    for x = 0 to (dx-1) do
      draw_cell (f.(y).(x)) x y;
    done
  done;
  
  Cairo.stroke ctx ;
  Cairo_png.surface_write_to_file surface outf
