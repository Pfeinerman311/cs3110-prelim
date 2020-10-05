open OUnit2
open Voting

(** [tally_compare t1 t2] unique sorts tally t1 and t2 using the candidate
    compare function and checks for equality *)
let tally_compare t1 t2 =
  let u1 = List.sort_uniq compare t1 in
  let u2 = List.sort_uniq compare t2 in
  u1 = u2

(** [plur_tally_test name ballots expected_output] constructs an
    OUni test named [name] that asserts the quality of [expected_output]
    with [plur_tally ballots]. *)
let plur_tally_test
    (name : string)
    (ballots : pballot list)
    (expected_output : tally) : test =
  name >:: (fun _ ->
      assert_equal ~cmp:tally_compare ~printer:(tally_to_string)
        expected_output (plur_tally ballots)) 

(** [plur_winner_test name ballots expected_output] constructs an
    OUni test named [name] that asserts the quality of [expected_output]
    with [plur_winner ballots]. *)
let plur_winner_test
    (name : string)
    (ballots : pballot list)
    (expected_output : name) : test =
  name >:: (fun _ ->
      assert_equal expected_output (plur_winner ballots))

(** [borda_tally_test name ballots expected_output] constructs an
    OUni test named [name] that asserts the quality of [expected_output]
    with [borda_tally ballots]. *)
let borda_tally_test
    (name : string)
    (ballots : bballot list)
    (expected_output : tally) : test =
  name >:: (fun _ ->
      assert_equal ~cmp:tally_compare ~printer:(tally_to_string)
        expected_output (borda_tally ballots)) 

(** [borda_winner_test name ballots expected_output] constructs an
    OUni test named [name] that asserts the quality of [expected_output]
    with [borda_winner ballots]. *)
let borda_winner_test
    (name : string)
    (ballots : bballot list)
    (expected_output : name) : test =
  name >:: (fun _ ->
      assert_equal expected_output (borda_winner ballots))

(** [t1_ballot] is the ballot for the first given test case
    [t1_tally] is the expected tally result for the first given test case *)
let t1_ballot = ["David"; "David"; "Anne"; "David"; "Michael";
                 "Anne"; "Michael"; "David"; "Anne"; "David"]
let t1_tally = [{name = "Anne"; votes = 3}; {name = "David"; votes = 5};
                {name = "Michael"; votes = 2}]

(** [t2_ballot] is the ballot for the second given test case
    [t2_winner] is the expected tally result for the second given test case *)
let t2_ballot = ["David"; "David"; "Anne"; "David"; "Michael";
                 "Anne"; "Michael";"David"; "Anne"; "David"]
let t2_winner = "David"

(** [t3_ballot] is the ballot for the third given test case
    [t3_winner] is the expected tally result for the third given test case *)
let t3_ballot = ["David"; "David"; "Anne"; "David"; "Michael";
                 "Anne"; "Michael"; "David"; "Anne"; "Anne"]
let t3_winner = "No Winner"

(** [t4_ballot] is the ballot for the fourth given test case
    [t4_tally] is the expected tally result for the fourth given test case *)
let t4_ballot = [["David"; "Anne"; "Michael"]; ["Anne"; "David"; "Michael"];
                 ["David"; "Anne"; "Michael"]; ["Anne"; "Michael"; "David"]]
let t4_tally = [{name = "Anne"; votes = 10}; {name = "David"; votes = 9};
                {name = "Michael"; votes = 5}]

(** [t5_ballot] is the ballot for the fifth given test case
    [t5_winner] is the expected tally result for the fifth given test case *)
let t5_ballot = [["David"; "Anne"; "Michael"]; ["Anne"; "David"; "Michael"];
                 ["David"; "Anne"; "Michael"]; ["Michael"; "David"; "Anne"]]
let t5_winner = "David"

(** [t6_ballot] is the ballot for the sixth given test case
    [t6_winner] is the expected tally result for the sixth given test case *)
let t6_ballot = [["David"; "Anne"; "Michael"]; ["Anne"; "David"; "Michael"];
                 ["Michael"; "Anne"; "David"]; ["Michael"; "David"; "Anne"]]
let t6_winner = "No Winner"


let tests = [
  (** These tests use the above variables for the test cases.
      The string "No Winner" is the output in the case of no winner *)
  plur_tally_test "Test 1 from Pg. 4" t1_ballot t1_tally;
  plur_winner_test "Test 2 from Pg. 4" t2_ballot t2_winner;
  plur_winner_test "Test 3 from Pg. 4" t3_ballot t3_winner;
  borda_tally_test "Test 4 from Pg. 4" t4_ballot t4_tally;
  borda_winner_test "Test 5 from Pg. 4" t5_ballot t5_winner;
  borda_winner_test "Test 6 from Pg. 4" t6_ballot t6_winner;
]

let suite = "suite" >::: tests

let _ = run_test_tt_main suite
