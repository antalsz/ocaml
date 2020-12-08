open Types

type position = First | Second

val swap_position : position -> position
val print_pos : Format.formatter -> position -> unit

type desc = { t: type_expr; expanded: type_expr option }
type 'a diff = { got: 'a; expected: 'a}

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

val short : type_expr -> desc

val explain: 'a list ->
  (prev:'a option -> 'a -> 'b option) ->
  'b option

type no_info = |

type rec_occur = Rec_occur of type_expr * type_expr

(* Used when printing traces *)
type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

(* Provided by {!Printtyp} for the trace-printing functions *)
type type_printers = {
  mark_loops : type_expr -> unit;
  type_expr  : Format.formatter -> type_expr -> unit;
  path       : Format.formatter -> Path.t -> unit
}

(* [function _ _ -> .], but doesn't rely on type inference *)
val explain_no_info : _ -> no_info -> _

val explain_rec_occur :  type_printers -> rec_occur -> (Format.formatter -> unit) option

module type Trace = sig
  type variant_info
  type obj_info
  type elt_info

  type variant =
    | Incompatible_types_for of string
    | Vinfo of variant_info

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Oinfo of obj_info

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of { name:string; diff: type_expr diff }
    | Info of elt_info

  type t = desc elt list

  val diff : type_expr -> type_expr -> desc elt

  (** [flatten f trace] flattens all elements of type {!desc} in
      [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
      or [f x.t x.t] otherwise *)
  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

  val map : ('a -> 'b) -> 'a elt list -> 'b elt list

  val incompatible_fields : string -> type_expr -> type_expr -> desc elt

  (* The following are for printing traces, and are used by Printtyp *)

  (* "is not compatible with type" is the archetypical value *)
  val incompatibility_phrase : string

  val constraint_escape_status : printing_status
  val drop_from_trace : (Types.type_expr * 'a) elt -> bool
  val explain_contextless_escaped_field_mismatch : bool

  val explain_variant_info :
    type_printers -> variant_info -> (Format.formatter -> unit) option
  val explain_obj_info :
    type_printers -> obj_info -> (Format.formatter -> unit) option
  val explain_elt_info :
    type_printers -> elt_info -> (Format.formatter -> unit) option
end

module Unification : sig
  type fixed_row_case =
    | Cannot_be_closed
    | Cannot_add_tags of string list

  type variant_info =
    | No_intersection
    | No_tags of position * (Asttypes.label * row_field) list
    | Fixed_row of position * fixed_row_case * fixed_explanation

  type obj_info = Self_cannot_be_closed

  include Trace with type variant_info := variant_info
                 and type obj_info := obj_info
                 and type elt_info = rec_occur

  (** Switch [expected] and [got] *)
  val swap : t -> t
end

module Equality : sig
  type variant_info =
    | Openness of position
    | Missing of position * Asttypes.label
    (* [Missing] is shared with [Moregen.variant_info], but [Moregen.Openness] takes 0
       arguments instead of 1 *)

  include Trace with type variant_info := variant_info
                 and type obj_info = no_info
                 and type elt_info = no_info
end

module Moregen : sig
  type variant_info =
    | Openness
    | Missing of position * Asttypes.label
    (* [Missing] is shared with [Equality.variant_info], but [Equality.Openness] takes 1
       argument instead of 0 *)

  include Trace with type variant_info := variant_info
                 and type obj_info = no_info
                 and type elt_info = rec_occur
end

module Subtype : sig
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val flatten : (type_expr -> type_expr -> 'a) -> t -> 'a elt list
end
