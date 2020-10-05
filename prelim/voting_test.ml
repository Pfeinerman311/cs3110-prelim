open OUnit2
open Voting

let tally_compare t1 t2 =
  let u1 = List.sort_uniq compare t1 in
  let u2 = List.sort_uniq compare t2 in
  u1 = u2

let plur_tally_test
    (name : string)
    (ballots : pballot list)
    (expected_output : tally) : test =
  name >:: (fun _ ->
      assert_equal ~cmp:tally_compare ~printer:(tally_to_string)
        expected_output (plur_tally ballots)) 

let plur_winner_test
    (name : string)
    (ballots : pballot list)
    (expected_output : name) : test =
  name >:: (fun _ ->
      assert_equal expected_output (plur_winner ballots))

let borda_tally_test
    (name : string)
    (ballots : bballot list)
    (expected_output : tally) : test =
  name >:: (fun _ ->
      assert_equal ~cmp:tally_compare ~printer:(tally_to_string)
        expected_output (borda_tally ballots)) 

let borda_winner_test
    (name : string)
    (ballots : bballot list)
    (expected_output : name) : test =
  name >:: (fun _ ->
      assert_equal expected_output (borda_winner ballots))


let t1b = ["David"; "David"; "Anne"; "David"; "Michael"; "Anne"; "Michael";
           "David"; "Anne"; "David"]
let t1t = [{name = "Anne"; votes = 3}; {name = "David"; votes = 5};
           {name = "Michael"; votes = 2}]
let t3b = ["David"; "David"; "Anne"; "David"; "Michael"; "Anne"; "Michael";
           "David"; "Anne"; "Anne"]

let t4b = [["David"; "Anne"; "Michael"]; ["Anne"; "David"; "Michael"];
           ["David"; "Anne"; "Michael"]; ["Anne"; "Michael"; "David"]]
let t4t = [{name = "Anne"; votes = 10}; {name = "David"; votes = 9};
           {name = "Michael"; votes = 5}]

let t5b = [["David"; "Anne"; "Michael"]; ["Anne"; "David"; "Michael"];
           ["David"; "Anne"; "Michael"]; ["Michael"; "David"; "Anne"]]

let t6b = [["David"; "Anne"; "Michael"]; ["Anne"; "David"; "Michael"];
           ["Michael"; "Anne"; "David"]; ["Michael"; "David"; "Anne"]]


let tests = [
  (* TODO: add at least the 6 required tests here, and delete this comment. *)
  plur_tally_test "Test 1" t1b t1t;
  plur_winner_test "Test 2" t1b "David";
  plur_winner_test "Test 3" t3b "No Winner";
  borda_tally_test "Test 4" t4b t4t;
  borda_winner_test "Test 5" t5b "David";
  borda_winner_test "Test 6" t6b "No Winner";
]

let suite = "suite" >::: tests

let _ = run_test_tt_main suite
