(*
  This file is part of scilla.

  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.
  
  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.
 
  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License along with
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)

(* Library to create and operate on multivariate polynomials *)

open Core

exception Polynomial_error of string

(* Variables in the polynomial are of type 'a.
 * Representation: co-efficient * (variable ^ pow) list
 * Example: (3, ['A', 2]) is 3 * (a^2)
 *)
type 'a term = int * ('a * int) list

(* A polynomial is a sum of terms.
 * NOTE:
 *  The representation used here does not in itself guarantee
 *  a canonicalized form. Use canonicalize_* below as needed.
 *)
type 'a polynomial = 'a term list

(* If a variable occurs more than once in a term,
 * combine the occurrences by adding the powers. *)
let canonicalize_term (t : 'a term) : 'a term =
  let (c, vplist) = t in
  if c = 0 then (0, []) else
  (* check if the first element of the list occurs again later, 
    * and recursively, the rest of the list. *)
  let rec merger vplist =
    (match vplist with
    | (cur_v, cur_p) :: rem ->
        (* Split the remaining into those equal to cur and not equal to cur. *)
        let (cureq, curneq) = List.partition_tf rem ~f:(fun (v, _) -> v = cur_v) in
        (* Add the powers of cur_v and those in cureq. *)
        let p = List.fold cureq ~init:cur_p ~f:(fun acc (_, t_p) -> acc + t_p) in
        (* Recursively eliminate duplicates in the remaining part of the list. *)
        let merged_rem' = merger curneq in
        if p = 0 then merged_rem' else ((cur_v, p)::merged_rem')
    | [] -> [])
  in
  let res = merger vplist in
    (c, res)

(* Check if two terms are equivalent. 
 * ~coef=false ignores co-efficients in the comparison. *)
let eq_term ?(coef=true) (t1' : 'a term) (t2' : 'a term) =
  let t1, t2 = canonicalize_term t1', canonicalize_term t2' in
  let (c1, vplist1), (c2, vplist2) = t1, t2 in
  (not coef || (c1 = c2)) && (List.length vplist1 = List.length vplist2) &&
  List.for_all vplist1 ~f:(fun (cur_v, cur_p) ->
    List.exists vplist2 ~f:(fun (cur_v', cur_p') ->
      cur_v = cur_v' && cur_p = cur_p'
    )
  )

let mul_term (t1 : 'a term) (t2 : 'a term) : 'a term =
  let (c1, vplist1), (c2, vplist2) = t1, t2 in
  canonicalize_term (c1 * c2, vplist1 @ vplist2)

(* If a term occurs more than once in a polynomial,
 * combine the occurrences by adding up the co-efficients. *)
let canonicalize_pn (tlist' : 'a polynomial) : 'a polynomial =
  (* Canonicalize each term first. *)
  let tlist = List.map tlist' ~f:(fun t -> canonicalize_term t) in
  (* Combine first term with same terms later on, and recursively for the remaining terms. *)
  let rec merger = function
    | [] -> []
    | h :: rem ->
      let (c_h, t_h) = h in
      (* If the coefficient of this term is 0, ignore it. *)
      if c_h = 0 then (merger rem) else
      (* Split the remaining terms into those equal to and not equal to h. *)
      let (heq, hneq) = List.partition_tf rem ~f:(fun t -> eq_term ~coef:false h t) in
      let h' = 
        let coef' = List.fold heq ~init:c_h ~f:(fun acc t ->
          let (c_t, _) = t in
              acc + c_t
        ) in
        (coef', t_h)
      in
        h' :: (merger hneq)
  in
    merger tlist

(* Check if two polynomials are equivalent. *)
let eq_pn (p1' : 'a polynomial) (p2' : 'a polynomial) =
 let p1, p2 = canonicalize_pn p1', canonicalize_pn p2' in
 (List.length p1 = List.length p2) &&
  List.for_all p1 ~f:(fun t1 -> List.exists p2 ~f:(fun t2 -> eq_term t1 t2 ))

(* Add two polynomials. *)
let add_pn (p1 : 'a polynomial) (p2 : 'a polynomial) =
  canonicalize_pn (p1 @ p2)

(* Multiply two polynomials. *)
let mul_pn (p1 : 'a polynomial) (p2 : 'a polynomial) =
  let prods = List.map p1 ~f:(fun t1 ->
    (* multiply t1 with each term of p2. *)
    List.map p2 ~f:(fun t2 -> mul_term t1 t2))
  in
    canonicalize_pn (List.concat prods)

(* Combine two polynomials (pairing each term in the first with those in the second)
 * using a custom function f, which returns (Some term) if two terms are to be combined *)
let combine_pn ~(cf : 'a term -> 'a term -> 'a term option) (p1' : 'a polynomial) (p2' : 'a polynomial) =
  let p1 = canonicalize_pn p1' in
  let p2 = canonicalize_pn p2' in
  (* Fold over p1 terms, building them up, while removing terms from p2. *)
  let (p1', p2') = List.fold p1 ~init:([], p2) ~f:(fun (p1_acc, p2_acc) t1 ->
    (* Fold over p2_acc, combining with t1 as possible. *)
    let (t1', p2_acc') = List.fold p2_acc ~init:(t1, []) ~f:(fun (t1_acc, p2_acc') t2 ->
      match cf t1_acc t2 with
      | Some t1' -> (t1', p2_acc')
      | None -> (t1_acc, t2 :: p2_acc')
    ) in
    (t1' :: p1_acc, List.rev p2_acc')
  ) in
  (* Just add up what's left of p2. *)
  canonicalize_pn (List.rev p1' @ p2')

(* Combine two polynomials by choosing terms with higher co-efficient from one of them. *)
let max_combine_pn (p1' : 'a polynomial) (p2' : 'a polynomial) =
  let p1 = canonicalize_pn p1' in
  let p2 = canonicalize_pn p2' in
  let (res, p2') = List.fold p1 ~init:([], p2) ~f:(fun (res, p2') (c1, t1) ->
    let (heq, hneq) = List.partition_tf p2' ~f:(fun t -> eq_term ~coef:false (c1, t1) t) in
    (* TODO assert that (List.length heq <= 1) due to canonicalize_pn earlier. *)
    match heq with
    | [(c2, t2)] when c2 > c1 -> ((c2, t2) :: res, hneq)
    | _ -> ((c1, t1) :: res, hneq)
  ) in
  res @ p2'

(* Replace every variable in the polynomial using a replacer. *)
let var_replace_pn (pn : 'a polynomial) ~(f:'a -> 'a) =
  let term_replacer (ter : 'a term) =
    let (coef, vlist) = ter in
      let vlist' = List.map vlist ~f:(fun (v, i1) ->
        let v' = f v in
          (v', i1)
      ) in
      (coef, vlist')
  in
    List.map pn ~f:(fun t -> term_replacer t)

(* Expand parameters in a polynomial into full polynomials. 
 * TODO: Make this efficient. *)
let expand_parameters_pn (pn' : 'a polynomial) ~(f: 'a -> 'a polynomial option) =
  let pn = canonicalize_pn pn' in
  (* Expand at-most one variable in term. *)
  let expand_parameters_term ((coef, vplist) : 'a term) : 'a polynomial =
    (* Partition vplist based on whether a variable should be substituted. *)
    let (nosubl, subl) = List.partition_tf vplist ~f:(fun (v, _) -> is_none (f v)) in
    match subl with
    | (svar, spow) :: restsub ->
      (* Substitute svar only (one substitution at-most to keep things simple). *)
      let rest_pol = [(coef, nosubl @ restsub)] in
      let subst_pol = BatOption.get (f svar) in
      if spow < 0 then raise (Polynomial_error "Cannot expand paramter with non-positive power") else
      let subst_pols = List.init spow ~f:(fun _ -> subst_pol) in
      List.fold subst_pols ~init:rest_pol ~f:(fun acc p -> mul_pn acc p)
    | [] -> [(coef, vplist)] (* nothing to substitute *)
  in
  (* Until no more substitutions, keep expanding. *)
  let rec subst pn =
    (* Expand at-most once in each term and accummulate the result. *)
    let pn' = List.fold pn ~init:[] ~f:(fun accum t ->
      let t' = expand_parameters_term t in
      add_pn accum t'
    ) in
    let pn'' = canonicalize_pn pn' in
    (* Repeat, till stabilization. *)
    if eq_pn pn'' pn then pn'' else subst pn''
  in
    subst pn

(* Print a polynomial, calling ~f to print a variable. *)
let sprint_pn (pn : 'a polynomial) ~(f: 'a -> string) =
  let sprint_term (coef, vplist) =
    if coef = 0 then "0" else
    List.fold_left vplist ~init:(Int.to_string coef)
      ~f:(fun acc (v, p) ->
        (if p = 0 then acc
        else if p = 1 then (acc ^ "(" ^ (f v) ^ ")")
        else (acc ^ "(" ^ (f v) ^ " ^ " ^ (Int.to_string p) ^ ")"))
      )
  in
  if pn = [] then "0" else
  List.fold_left pn ~init:"" ~f:(fun acc t -> 
    (if acc = "" then "" else acc ^ " + ") ^ sprint_term t)

(*********** Utilities to build polynomials easily **************)

(* Build a polynomial with only a constant term. *)
let const_pn i : 'a polynomial =
  [(i, [])]

(* Build an empty polynomial. *)
let empty_pn : 'a polynomial = []

(* Build a polynomial with a single variable v.
 * p = v *)
let single_simple_pn v : 'a polynomial =
  [(1, [(v, 1)])]

(* Is this is a simple constant? *)
let is_const_term t = match t with | (_, []) -> true | _ -> false
(* Is this is a simple constant? *)
let is_const_pn p =
  let p' = canonicalize_pn p in
  match p' with
  | [] -> true
  | [t] -> is_const_term t
  | _ -> false
