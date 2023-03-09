type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* #1 *)
let subset a b = 
  List.for_all (fun g -> List.exists (fun e -> e = g) b) a

(* #2 *)
let equal_sets a b = 
  subset a b && subset b a

(* #3 *)
let rec set_union a b = 
  List.append a b

(* #4 *)
let rec set_all_union a = 
  List.concat a

(* #5 *)
(* Clearly, S must be a set of sets (at least two layers). If not, an element of the set cannot be a set. To test 
* whether an element of S is S, we must compare them. However, we cannot, as they have different TYPES in OCaml. For 
* example, [1] has type int list and [[1]] has type int list list. An element of S must have one less `list` in its 
* type than S. Thus, since we cannot compare two elements of different types, we cannot write such a function 
* self_member s. *)

(* #6 *)
let rec computed_fixed_point eq f x = 
  if eq (f x) x then x else computed_fixed_point eq f (f x)

(* #7 start *)
let rec find_p_val f p x = 
  if p = 0 then x else find_p_val f (p - 1) (f x)

let rec computed_periodic_point eq f p x = 
  if eq x (find_p_val f p x) then x else computed_periodic_point eq f p (f x)
(* #7 end *)

let rec whileseq s p x = (* #8 *)
  if p x then x::whileseq s p (s x) else []

let all_term lst t_syms = (* #9 start *) 
  List.for_all (fun e -> match e with 
                    | N n -> List.mem e t_syms
                    | T t -> true) lst

let rec update_t_syms t_syms rules = match rules with
  (symbol, lst)::tail -> update_t_syms (if all_term lst t_syms then ((N symbol)::t_syms) else t_syms) tail
  | _ -> t_syms

let find_t_syms t_syms rules = 
	computed_fixed_point (fun a b -> equal_sets (snd a) (snd b)) (fun (rules, t_syms) -> (rules, update_t_syms t_syms rules)) (rules, t_syms)

let filter_blind_alleys g = (* #9 end *)
  let (rules, t_syms) = find_t_syms [] (snd g) in
	(fst g, List.filter (fun e -> all_term (snd e) t_syms) (snd g))