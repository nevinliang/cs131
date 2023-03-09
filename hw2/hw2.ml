let convert_grammar old_grammar = 
    let getlist inp = List.filter (function 
        | (a, _) when a = inp -> true
        | _ -> false) (snd old_grammar) in
    (fst old_grammar, fun inp -> (getlist inp))

type ('nonterminal, 'terminal) parse_tree = 
    | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
    | Leaf of 'terminal

(* function traverses tree left to right and returns list of leaves *)
let parse_tree_leaves tree = 
    let rec parse_tree_leaves_h = function
        | h::t -> (match h with 
                    | Leaf l -> [l] 
                    | Node (_, lst) -> parse_tree_leaves_h lst) 
                @ parse_tree_leaves_h t
        | _ -> [] in
    parse_tree_leaves_h [tree]

(* takes in a production function, a set of rules, the acceptor function, and a fragment to analyze *)
let match_acceptor rules_list prod_func accept frag = 
    

let make_matcher gram = 
    let start_sym = fst gram and prod_func = snd gram in
    fun accept frag -> match_acceptor (prod_func start_sym) prod_func accept frag