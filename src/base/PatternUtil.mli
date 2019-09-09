open Core

(* Pattern match expression descriptions *)
module Exp_descriptions :
  sig
    type yes_no_maybe = Yes | No | Maybe
    (* Positive constructor matches followed by string list of negatives *)
    type exp_dsc = Pos of string * exp_dsc list | Neg of string list
    (* Add negative description to exp_dsc, fail if positive *)
    val add_neg : exp_dsc -> string -> exp_dsc
    (* Build descriptions using positives and existing exp_dsc*)
    val build_dsc :
      (string * exp_dsc List.t) list ->
      exp_dsc -> ('a * 'b * exp_dsc list) list -> exp_dsc
    (* Add arguments alongside constructor *)
    val augment_ctx : ('a * 'b list) list -> 'b -> ('a * 'b list) list
    val pos_ctx :
      (string * exp_dsc List.t) list ->
      (string * exp_dsc List.t) list
  end

module Decision_Tree :
  sig
    type ('v, 'tv, 'cv) decision_tree =
        Success of 'v
      | Fail
      | IfEq of 'tv * 'cv * ('v, 'tv, 'cv) decision_tree *
          ('v, 'tv, 'cv) decision_tree
  end
