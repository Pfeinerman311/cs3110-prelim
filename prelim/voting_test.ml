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


let t1b = ["David"; "David"; "Anne"; "David"; "Michael"; "Anne"; "Michael";
           "David"; "Anne"; "David"]
let t1t = [{name = "Anne"; votes = 3}; {name = "David"; votes = 5};
           {name = "Michael"; votes = 2}]
let tests = [
  (* TODO: add at least the 6 required tests here, and delete this comment. *)
  plur_tally_test "Test 1" t1b t1t;
  plur_winner_test "Test 2" t1b "David"
]

let suite = "suite" >::: tests

let _ = run_test_tt_main suite
