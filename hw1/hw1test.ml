let subset_test0 = subset [1;2;3;4] [4;4;3;4;2;3;1]
let subset_test1 = not (subset [1;2;3;4;5] [4;4;3;4;2;3;1])

let equal_sets_test0 = equal_sets [2; 2; 2; 2] [2]
let equal_sets_test1 = not (equal_sets [] [3])

let set_union_test0 = equal_sets (set_union [] []) []

let set_all_union_test1 =
    equal_sets (set_all_union [[5;2]; [5;2]; [5;2]]) [2;5;2;5]

let computed_fixed_point_test0 =
    computed_fixed_point (=) sqrt 0. = 0.

let computed_periodic_point_test0 =
    computed_periodic_point (=) (fun x -> x *. x -. 1.) 4 0.75 = -1.

let whileseq_test0 =
    whileseq ((+) 4) ((>) 15) 2 = [2; 6; 10; 14]

type my_nonterminals =
  | Start | Bob | Joe | Fred

let my_rules =
   [Start, [T"hi"; N Joe; T"bye"];
    Start, [N Fred];
    Start, [N Joe; N Fred; N Bob];
    Start, [T"haha"; N Joe];
    Bob, [T"1"];
    Bob, [T"2"];
    Bob, [T"3"];
    Bob, [T"4"];
    Fred, [T"5"];
    Fred, [T"6"];
    Fred, [T"7"]]

let my_grammar = Start, my_rules

let my_test0 =
    filter_blind_alleys my_grammar = (Start,
   [(Start, [N Fred]); (Bob, [T "1"]); (Bob, [T "2"]); (Bob, [T "3"]);
    (Bob, [T "4"]); (Fred, [T "5"]); (Fred, [T "6"]); (Fred, [T "7"])])