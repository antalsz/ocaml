open Types

type position = First | Second

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

module type Trace = sig
  type variant_info
  type obj_info
  type elt_info

  type variant =
    | Incompatible_types_for of string
    | Info of variant_info

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Info of obj_info

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of { name:string; diff: type_expr diff }
    | Note of string
    | Info of elt_info

  type t = desc elt list

  val diff : type_expr -> type_expr -> desc elt

  val debug_note : __LOC__:string -> string -> 'a elt

  (** [flatten f trace] flattens all elements of type {!desc} in
      [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
      or [f x.t x.t] otherwise *)
  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

  (* CR aspectorzabusky: Could be polymorphic (['a -> 'b]) *)
  val map : (desc -> desc) -> desc elt list -> desc elt list

  val incompatible_fields : string -> type_expr -> type_expr -> desc elt
end

module Unification : sig
  type fixed_row_case =
    | Cannot_be_closed
    | Cannot_add_tags of string list

  type variant_info =
    | No_intersection
    | No_tags of position * (Asttypes.label * row_field) list
    | Fixed_row of position * fixed_row_case * fixed_explanation

  type self_cannot_be_closed = Self_cannot_be_closed

  type nonrec rec_occur = rec_occur = Rec_occur of type_expr * type_expr
  (* CR aspectorzabusky: Shared with [Moregen] *)

  include Trace with type variant_info := variant_info
                 and type obj_info := self_cannot_be_closed
                 and type elt_info := rec_occur

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
                 and type obj_info := no_info
                 and type elt_info := no_info
end

module Moregen : sig
  type variant_info =
    | Openness
    | Missing of position * Asttypes.label
    (* [Missing] is shared with [Equality.variant_info], but [Equality.Openness] takes 1
       argument instead of 0 *)

  type nonrec rec_occur = rec_occur = Rec_occur of type_expr * type_expr
  (* CR aspectorzabusky: Shared with [Unification] *)

  include Trace with type variant_info := variant_info
                 and type obj_info := no_info
                 and type elt_info := rec_occur
end

module Subtype : sig
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val flatten : (type_expr -> type_expr -> 'a) -> t -> 'a elt list
end

