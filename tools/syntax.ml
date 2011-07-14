(* Abstract syntax. *)
exception Unimplemented
(* Variable names *)
type name = string

(* Types *)
type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TArrow of ty * ty (* Functions *)
  | TCons of ty       (* Constructions *)

type funcargs = (name * ty) list

(* Expressions *)
type expr =
  | Var of name          		(* Variable *)
  | Int of int           		(* Non-negative integer constant *)
  | Bool of bool         		(* Boolean constant *)
  | Args of (expr list)
  | Times of expr * expr 		(* Product [e1 * e2] *)
  | Plus of expr * expr  		(* Sum [e1 + e2] *)
  | Minus of expr * expr 		(* Difference [e1 - e2] *)
  | Equal of expr * expr 		(* Integer comparison [e1 = e2] *)
  | Less of expr * expr  		(* Integer comparison [e1 < e2] *)
  | More of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | If of expr * expr * expr 		(* Conditional [if e1 then e2 else e3] *)
  | Fun of name * funcargs * ty * expr  (* Function [fun f(x:s):t is e] *)
  | Apply of expr * expr		(* Application [e1 e2] *)
  | Cons of expr * expr
  | Nil 
  | Fst of expr
  | Snd of expr
  | Out of expr
  | Identified of (expr list) * (expr list list) (* identified as n-in m-out *)
  | Let of name * expr * expr

(* Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (* Expression *)
  | Def of name * expr (* Value definition [let x = e] *)
  | CDef of name * expr (* Constants *)

let of_elist = function
    Args x -> x
  | _ -> []
let of_cons_args x = List.hd (of_elist x)

(* [subst [(x1,e1);...;(xn;en)] e] replaces in expression [e] all
    free occurrences of variables [x1], ..., [xn] with expressions
    [e1], ..., [en], respectively. *)
let rec subst s = function
  | (Var x) as e -> (try List.assoc x s with Not_found -> e)
  | (Int _ | Bool _ | Nil) as e -> e
  | Args _ -> raise Unimplemented
  | Identified (_,_) -> raise Unimplemented
(* guards: *)
  | Times (e1, Args e2) -> Times (subst s e1, subst s (List.hd e2))
  | Plus (e1, Args e2) ->  Plus  (subst s e1, subst s (List.hd e2))
  | Minus (e1, Args e2) -> Minus (subst s e1, subst s (List.hd e2))
(* /guards: *)
  | Times (e1, e2) -> Times (subst s e1, subst s e2)
  | Plus (e1, e2) -> Plus (subst s e1, subst s e2)
  | Minus (e1, e2) -> Minus (subst s e1, subst s e2)
  | Equal (e1, e2) -> Equal (subst s e1, subst s e2)
  | Less (e1, e2) -> Less (subst s e1, subst s e2)
  | More (e1, e2) -> More (subst s e1, subst s e2)
  | And (e1, e2) -> And (subst s e1, subst s e2)
  | Or (e1, e2) -> Or (subst s e1, subst s e2)
  | Out (Args e1) -> Out (subst s (List.hd e1))
  | Fst (Args e1) -> Fst (subst s (List.hd e1))
  | Snd (Args e1) -> Snd (subst s (List.hd e1))
  | Out a -> Out (subst s a)
  | Fst (e1) -> Fst (subst s e1)
  | Snd (e1) -> Snd (subst s e1)
  | Cons(e1, e2) -> Cons (subst s e1, subst s e2)
  | If (e1, e2, e3) -> If (subst s e1, subst s e2, subst s e3)
  | Fun (f, a, ty2, e) ->
      let s' = List.remove_assoc f
	  (List.filter (fun x -> not ((List.exists (fun y -> (fst y) == (fst x))) a)) s) in
      Fun (f, a, ty2, subst s' e)
  | Apply (e1, e2) -> Apply (subst s e1, Args(List.map (subst s) (of_elist e2)))
  | Let (v,ev,e) -> Let (v, subst s ev, subst s e)
