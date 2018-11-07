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
open MonadUtil
open Core.Result.Let_syntax

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

(* Replace every variable in the polynomial using a replacer. *)
let var_replace_pn (pn : 'a polynomial) ~(f:'a -> ('a, 'b) result) =
  let term_replacer (ter : 'a term) =
    let (coef, vlist) = ter in
      let%bind vlist' = mapM ~f:(fun (v, i1) ->
        let%bind v' = f v in
        pure (v', i1)
      ) vlist in
      pure (coef, vlist')
  in
    mapM ~f:(fun t -> term_replacer t) pn

(* Print a polynomial, calling ~f to print a variable. *)
let sprint_pn (pn : 'a polynomial) ~(f: 'a -> string) =
  let sprint_term (coef, vplist) =
    if coef = 0 then "" else
    List.fold_left vplist ~init:(if coef = 1 then "" else (Int.to_string coef))
      ~f:(fun acc (v, p) ->
        (if acc = "" then "" else ".") ^ 
        (if p = 0 then acc
        else if p = 1 then (acc ^ "(" ^ (f v) ^ ")")
        else (acc ^ "(" ^ (f v) ^ " ^ " ^ (Int.to_string p) ^ ")"))
      )
  in
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
