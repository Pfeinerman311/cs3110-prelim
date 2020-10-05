(** CS 3110 Fall 2020 Prelim
    @author Parker Feinerman (pjf73) *)

(************************************************************ 

   Academic Integrity Statement

   I, the person named in the @author comment above, have fully reviewed the
   course academic integrity policies, and the instructions document for this
   exam.  I acknowledge that the minimum penalty for cheating on this exam
   is a score of -100%.  I acknowledge that no collaboration is permitted,
   nor is use of sites such as Chegg.

   If there are any violations I want to admit, I have documented them here,
   or I will send them by email to the professor:

   - none

 ************************************************************)  

(** [name] represents the name of a a candidate *)
type name = string

(** [pballot] represents ballot of a plurality election *)
type pballot = name

(** [bballot] represents ballot of a Borda election *)
type bballot = name list

(** [candidate] represents a candidate with both a name and number
    of votes *)
type candidate = {
  name : name;

  votes : int;
}

(** [tally] represents the list of candidates returned in a tally *)
type tally = candidate list

(** [compare c1 c2] is the compare function for candidate variables.
    Sorts by the number of votes first,
    then defaults to alphabetical names.*)
let compare c1 c2 =
  match c1.votes - c2.votes with
  | 0 -> String.compare c1.name c2.name
  | x -> x

(** [result_upd c v results] searches through the results and adds or 
    updates the candidate c to have v more votes *)
let rec result_upd c v results =
  match results with
  | [] -> [{name = c; votes = v}]
  | h::t -> if h.name = c then
      {name = c; votes = h.votes + v}::t else
      h::(result_upd c v t)

(** [plur_calc ballots results] goes through each element in a plurality 
    ballot and calls result_upd to give the candidate one more vote *)
let rec plur_calc ballots results =
  match ballots with
  | [] -> results
  | h::t -> plur_calc t (result_upd h 1 results)

(** [plur_tally ballots] takes in a list of plurality ballots and calls
    plur_calc to get the updated tally. Then unique sorts the list using
    the above compare funtion and reverse the list to have winner first *)
let plur_tally ballots =
  plur_calc ballots [] |> List.sort_uniq compare |> List.rev

(** [win_check w results] checks the given possible winner against
    the next candidate in results to check if there is a tie/no winner.
    Returns the string "No Winner" if there is a tie *)
let win_check w results =
  match results with
  | [] -> w.name
  | h::t -> if w.votes = h.votes then
      "No Winner" else w.name

(** [plur_winner ballots] takes in a list of plurality ballots and calls
    plur_tally to get the tally results, then calls win_check on the first
    element to check if there is a tie *)
let plur_winner ballots =
  match plur_tally ballots with
  | [] -> failwith "No Votes"
  | h::t -> win_check h t

(** [borda_helper ballot results] goes through each element in a Borda 
    ballot and calls result_upd to give the candidate the respective
    number of more vote *)
let rec borda_helper ballot results =
  match ballot with
  | [] -> results
  | h::t -> borda_helper t (result_upd h (List.length ballot) results)

(** [borda_calc ballots results] goes through each element in a Borda 
    ballot list and calls borda_helper to compute and update the Borda
    ballot votes *)
let rec borda_calc ballots results =
  match ballots with
  | [] -> results
  | h::t -> borda_calc t (borda_helper h results)

(** [borda_tally ballots] takes in a list of Borda ballots and calls
    borda_calc to get the updated tally. Then unique sorts the list using
    the above compare funtion and reverse the list to have winner first *)
let borda_tally ballots = 
  borda_calc ballots [] |> List.sort_uniq compare |> List.rev

(** [borda_winner ballots] takes in a list of Borda ballots and calls
    borda_tally to get the tally results, then calls win_check on the first
    element to check if there is a tie *)
let borda_winner ballots =
  match borda_tally ballots with
  | [] -> failwith "No Votes"
  | h::t -> win_check h t

(** [candidate_to_strring candidate] converts a candidate variable into
    a string of format (NAME, # votes) *)
let candidate_to_string candidate =
  String.concat "" ["("; candidate.name; ", "; 
                    string_of_int candidate.votes; " votes)"]

(** [tally_to_strring candidate] converts a tally variable into
    a string of format (NAME, # votes), ... , (NAME, # votes) *)
let rec tally_to_string tally =
  match tally with
  | [] -> ""
  | h::t -> String.concat ", " 
              [candidate_to_string h; tally_to_string t]