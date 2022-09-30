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
val canonicalize_term : 'a term -> 'a term

(* Check if two terms are equivalent. 
 * ~coef=false ignores co-efficients in the comparison. *)
val eq_term : ?coef:bool -> 'a term -> 'a term -> bool
val mul_term : 'a term -> 'a term -> 'a term

(* If a term occurs more than once in a polynomial,
 * combine the occurrences by adding up the co-efficients. *)
val canonicalize_pn : 'a polynomial -> 'a polynomial

(* Check if two polynomials are equivalent. *)
val eq_pn : 'a polynomial -> 'a polynomial -> bool

(* Add two polynomials. *)
val add_pn : 'a polynomial -> 'a polynomial -> 'a polynomial

(* Multiply two polynomials. *)
val mul_pn : 'a polynomial -> 'a polynomial -> 'a polynomial

(* Combine two polynomials (pairing each term in the first with those in the second)
 * using a custom function f, which returns (Some term) if two terms are to be combined *)
val combine_pn :
  cf:('a term -> 'a term -> 'a term option) ->
  'a polynomial ->
  'a polynomial ->
  'a polynomial

(* Combine two polynomials by choosing terms with higher co-efficient from one of them. *)
val max_combine_pn : 'a polynomial -> 'a polynomial -> 'a polynomial

(* Replace every variable in the polynomial using a replacer. *)
val var_replace_pn : 'a polynomial -> f:('a -> 'a) -> 'a polynomial

(* Expand parameters in a polynomial into full polynomials. 
 * TODO: Make this efficient. *)
val expand_parameters_pn :
  'a polynomial -> f:('a -> 'a polynomial option) -> 'a polynomial

(* Print a polynomial, calling ~f to print a variable. *)
val sprint_pn : 'a polynomial -> f:('a -> string) -> string

(*********** Utilities to build polynomials easily **************)

(* Build a polynomial with only a constant term. *)
val const_pn : int -> 'a polynomial

(* Build an empty polynomial. *)
val empty_pn : 'a polynomial

(* Build a polynomial with a single variable v.
 * p = v *)
val single_simple_pn : 'a -> 'a polynomial

(* Is this is a simple constant? *)
val is_const_term : 'a term -> bool

(* Is this is a simple constant? *)
val is_const_pn : 'a polynomial -> bool
