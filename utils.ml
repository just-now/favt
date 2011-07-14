let rec mk_list f t = if (f = t) then [] else f::(mk_list (f+1) t)
let rec zipWith f l1 l2 = match (l1,l2) with
  ([],[])  -> []
| ([],a)   -> a
| (a,[])   -> a
| (x::xs,y::ys) -> (f x y) :: (zipWith f xs ys)
let orl = zipWith (fun x y -> x||y)
let rec iterate f times a = if times = 0 then f(a) else f(iterate f (times - 1) a);;
let compose f g = function x -> f(g(x))
let (%) = compose
let soi i = string_of_int i

(* buggy version... *)
let rec zipWith2 f l1 l2 = match (l1,l2) with
  ([],[]) -> []
| _ -> (f (List.hd l1) (List.hd l2))::zipWith2 f (List.tl l1) (List.tl l2);;

(* hashtable to list *)
let h2l h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
