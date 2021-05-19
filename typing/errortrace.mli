(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*              Antal Spector-Zabusky, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

type position = First | Second

val swap_position : position -> position
val print_pos : Format.formatter -> position -> unit

type expanded_type = { ty: type_expr; expanded: type_expr }
type 'a diff = { got: 'a; expected: 'a }

(** [map_diff f {expected;got}] is [{expected=f expected; got=f got}] *)
val map_diff: ('a -> 'b) -> 'a diff -> 'b diff

(** Scope escape related errors *)
type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

val map_escape : ('a -> 'b) -> 'a escape -> 'b escape

val explain: 'a list ->
  (prev:'a option -> 'a -> 'b option) ->
  'b option

(* Type indices *)
type unification = private Unification
type comparison  = private Comparison

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type 'variety variant =
  (* Common *)
  | Incompatible_types_for : string -> _ variant
  | No_tags : position * (Asttypes.label * row_field) list -> _ variant
  (* Unification *)
  | No_intersection : unification variant
  | Fixed_row :
      position * fixed_row_case * fixed_explanation -> unification variant
  (* Equality & Moregen *)
  | Presence_not_guaranteed_for : position * string -> comparison variant
  | Openness : position (* Always [Second] for Moregen *) -> comparison variant

type 'variety obj =
  (* Common *)
  | Missing_field : position * string -> _ obj
  | Abstract_row : position -> _ obj
  (* Unification *)
  | Self_cannot_be_closed : unification obj

type ('a, 'variety) elt =
  (* Common *)
  | Diff : 'a diff -> ('a, _) elt
  | Variant : 'variety variant -> ('a, 'variety) elt
  | Obj : 'variety obj -> ('a, 'variety) elt
  | Escape : 'a escape -> ('a, _) elt
  | Incompatible_fields : { name:string; diff: type_expr diff } -> ('a, _) elt
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) elt

type ('a, 'variety) t = ('a, 'variety) elt list

type 'variety trace = (type_expr,     'variety) t
type 'variety error = (expanded_type, 'variety) t

val map : ('a -> 'b) -> ('a, 'variety) t -> ('b, 'variety) t

val incompatible_fields :
  name:string -> got:type_expr -> expected:type_expr -> (type_expr, _) elt

val swap_trace : ('a, 'variety) t -> ('a, 'variety) t

(* The traces (['variety t]) are the core error types.  However, we bundle them
   up into three "top-level" error types, which are used elsewhere:
   [unification_error], [equality_error], and [moregen_error].  In the case of
   [equality_error], this has to bundle in extra information; in general, it
   distinguishes the three types of errors and allows us to distinguish traces
   that are being built (or processed) from those that are complete and have
   become the final error. *)

type unification_error = { trace : unification error } [@@unboxed]

type equality_error =
  { trace : comparison error;
    subst : (type_expr * type_expr) list }

type moregen_error = { trace : comparison error } [@@unboxed]

(* Wraps up the two different kinds of [comparison] errors in one type *)
type comparison_error =
  | Equality_error of equality_error
  | Moregen_error  of moregen_error

(* Lift [swap_trace] to [unification_error] *)
val swap_unification_error : unification_error -> unification_error

module Subtype : sig
  type 'a elt =
    | Diff of 'a diff

  type 'a t = 'a elt list

  type trace = type_expr     t
  type error = expanded_type t

  val map : ('a -> 'b) -> 'a t -> 'b t
end
