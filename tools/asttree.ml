open Utils
open Syntax
exception AstTreeException of  string
(* tree vs tree diff function *)
let rec diff t1 t2 = match (t1,t2) with
  Nil, Nil -> true
| (Out a1, Out a2) -> diff a1 a2
| (Var a1, Var a2) -> a1 = a2
| (Int i1, Int i2) -> i1 = i2
| (Bool b1, Bool b2) -> b1 = b2
| (Times (a1, b1), Times (a2, b2))  -> (diff a1 a2) && (diff b1 b2)
| (Plus  (a1, b1), Plus  (a2, b2))  -> (diff a1 a2) && (diff b1 b2)
| (Minus (a1, b1),  Minus (a2, b2)) -> (diff a1 a2) && (diff b1 b2)
| (Cons(a1,b1), Cons(a2,b2)) -> (diff a1 a2) && (diff b1 b2)
|  Nil, _ -> false
| (Out a1, _) -> false
| (Var a1, _) -> false
| (Int i1, _) -> false
| (Bool b1, _) -> false
| (Times (a1, b1), _)-> false
| (Plus  (a1, b1), _)-> false
| (Minus (a1, b1), _) ->false
| (Cons(a1,b1), _) ->false
|  _ -> raise (AstTreeException "_")

(*produce all subtrees of given tree *)
let rec subtrees t1 acc = match t1 with
  Nil as e   ->  acc @ [e]
| Out a as e ->  subtrees a (acc @ [e])
| Var a as e ->  acc @ [e]
| Int a as e ->  acc @ [e]
| Bool a as e->  acc @ [e]
| Times (a1, a2)  as e-> subtrees a1 (subtrees a2 (acc@ [e]))
| Plus  (a1, a2) as e -> subtrees a1 (subtrees a2 (acc@ [e]))
| Minus (a1, a2) as e -> subtrees a1 (subtrees a2 (acc@ [e]))
| Cons(a1, a2) as e   -> subtrees a1 (subtrees a2 (acc@ [e]))
| _ -> raise (AstTreeException "_")

(* replace 'r' in 't' with 'Var v' *)
let rec replace_sub t r v =
match t with
  Nil as e   ->  e
| Var a as e ->  e
| Int a as e ->  e
| Bool a as e->  e
| Out a as e ->  if diff e r then v else Out(replace_sub a r v)
| Times (a1, a2) as e -> if diff e r then v else Times(replace_sub a1 r v, replace_sub a2 r v)
| Plus  (a1, a2) as e -> if diff e r then v else Plus (replace_sub a1 r v, replace_sub a2 r v)
| Minus (a1, a2) as e -> if diff e r then v else Minus(replace_sub a1 r v, replace_sub a2 r v)
| Cons(a1, a2)   as e -> if diff e r then v else Cons (replace_sub a1 r v, replace_sub a2 r v)
| _ -> raise (AstTreeException "_")
  

(* compare lists of subtrees: equal trees are marked 
   with (pos1,pos2, t1,t2), unequal (-1,-1,..,..) *)
let compareTree2Tree l1 l2 =
  List.map (fun (x1,y1) ->
    List.map (fun (x2,y2) ->
      if (x1!=x2 && diff y1 y2)
      then (x1,x2,y1,y2)
      else (-1,-1,y1,y2)) l1) l2;;

(* find all duplicates in given expression *)
let duplicates ex = 
  let subtr = subtrees ex [] in
  let subtr_numed = 
    zipWith2 (fun x y -> (x,y))
      (mk_list 0 (List.length subtr)) subtr in
  let listeqsubtr = List.concat (compareTree2Tree subtr_numed subtr_numed) in
  let filt_eqlist = 
    List.filter (fun (x,y,z,v) -> 
      x!=y && match (z,v) with (Var _, Var _)|(Int _, Int _) -> false | _ ->true) listeqsubtr in
  BatList.unique ( List.map (fun (x,y,z,v) -> v) filt_eqlist)

(* return true iff t is a subtree of tin *)
let isASubTree tin t = let sts = subtrees tin [] in List.exists (fun x -> x=t) sts

(* return filtered duplicates of given ast *)
let findAstDuplicates ex = 
  let duplicate = duplicates ex in
  let subtrees =
    List.map (fun x ->
      List.map (fun y ->
	x != y && isASubTree x y) duplicate) duplicate in
  let skipduplicate = List.fold_left (fun x y -> orl x y) [] subtrees
  in
  List.map (fun (z,v) ->v)
    (List.filter (fun (x,y) -> x=false)
       (zipWith2 (fun a b -> (a,b)) skipduplicate duplicate))
