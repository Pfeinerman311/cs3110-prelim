(** CS 3110 Fall 2020 Prelim
    @author Parker Feinerman (pjf73) *)

(* TODO: complete the academic integrity statement below, then delete this
   TODO comment. *)

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


(* TODO: Write your solution below, then delete this comment. *)
type name = string

type pballot = name

type bballot = name list

type candidate = {
  name : name;

  votes : int;
}

type tally = candidate list

let compare c1 c2 =
  match c1.votes - c2.votes with
  | 0 -> String.compare c1.name c2.name
  | x -> x

let rec result_upd res v results =
  match results with
  | [] -> [{name = res; votes = v}]
  | h::t -> if h.name = res then
      {name = res; votes = h.votes + v}::t else
      h::(result_upd res v t)

let rec plur_calc ballots results =
  match ballots with
  | [] -> results
  | h::t -> plur_calc t (result_upd h 1 results)

let plur_tally ballots =
  plur_calc ballots [] |> List.sort_uniq compare |> List.rev

let win_check w results =
  match results with
  | [] -> w.name
  | h::t -> if w.votes = h.votes then
      "No Winner" else w.name

let plur_winner ballots =
  match plur_tally ballots with
  | [] -> failwith "No Votes"
  | h::t -> win_check h t

let rec borda_helper ballot results =
  match ballot with
  | [] -> results
  | h::t -> borda_helper t (result_upd h (List.length ballot) results)

let rec borda_calc ballots results =
  match ballots with
  | [] -> results
  | h::t -> borda_calc t (borda_helper h results)

let borda_tally ballots = 
  borda_calc ballots [] |> List.sort_uniq compare |> List.rev

let borda_winner ballots =
  match borda_tally ballots with
  | [] -> failwith "No Votes"
  | h::t -> win_check h t


let candidate_to_string candidate =
  String.concat "" ["("; candidate.name; ", "; 
                    string_of_int candidate.votes; " votes)"]

let rec tally_to_string tally =
  match tally with
  | [] -> ""
  | h::t -> String.concat ", " 
              [candidate_to_string h; tally_to_string t]