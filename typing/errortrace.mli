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

(* Type indices *)
module type Type_bool = sig
  type yes = private Yes
  type no  = private No
end

module Is_unification : Type_bool
module Is_equality    : Type_bool
module Is_moregen     : Type_bool

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type ('is_equality, 'is_moregen) openness =
  | WhenEquality : position -> (Is_equality.yes, Is_moregen.no) openness
  | WhenMoregen : (Is_equality.no, Is_moregen.yes) openness

type ('is_unification, 'is_equality, 'is_moregen) variant =
  (* Common *)
  | Incompatible_types_for : string -> (_, _, _) variant
  (* Unification *)
  | No_intersection : (Is_unification.yes, Is_equality.no, Is_moregen.no) variant
  | No_tags : position * (Asttypes.label * row_field) list -> (Is_unification.yes, Is_equality.no, Is_moregen.no) variant
  | Fixed_row : position * fixed_row_case * fixed_explanation -> (Is_unification.yes, Is_equality.no, Is_moregen.no) variant
  (* Equality & Moregen *)
  | Openness : ('e, 'm) openness -> (Is_unification.no, 'e, 'm) variant
  | Missing :  position * Asttypes.label -> (Is_unification.no, _, _) variant

type ('is_unification, 'is_equality, 'is_moregen) obj =
  (* Common *)
  | Missing_field : position * string -> (_, _, _) obj
  | Abstract_row : position -> (_, _, _) obj
  (* Unification *)
  | Self_cannot_be_closed : (Is_unification.yes, Is_equality.no, Is_moregen.no) obj

type ('a, 'is_unification, 'is_equality, 'is_moregen) elt =
  (* Common *)
  | Diff : 'a diff -> ('a, _, _, _) elt
  | Variant :  ('u, 'e, 'm) variant -> ('a, 'u, 'e, 'm) elt
  | Obj :  ('u, 'e, 'm) obj -> ('a, 'u, 'e, 'm) elt
  | Escape : 'a escape -> ('a, _, _, _) elt
  | Incompatible_fields : { name:string; diff: type_expr diff } -> ('a, _, _, _) elt
  (* Unification & Moregen *)
  | Rec_occur : type_expr * type_expr -> ('a, _, Is_equality.no, _) elt

type ('is_unification, 'is_equality, 'is_moregen) t =
  (desc, 'is_unification, 'is_equality, 'is_moregen) elt list

val diff : type_expr -> type_expr -> (desc, _, _, _) elt

(** [flatten f trace] flattens all elements of type {!desc} in
    [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
    or [f x.t x.t] otherwise *)
val flatten: (type_expr -> type_expr -> 'a) -> ('u, 'e, 'm) t -> ('a, 'u, 'e, 'm) elt list

val map : ('a -> 'b) -> ('a, 'u, 'e, 'm) elt list -> ('b, 'u, 'e, 'm) elt list

val incompatible_fields : string -> type_expr -> type_expr -> (desc, _, _, _) elt


module type Trace = sig
  type is_unification
  type is_equality
  type is_moregen

  type nonrec variant = (    is_unification, is_equality, is_moregen) variant
  type nonrec obj     = (    is_unification, is_equality, is_moregen) obj
  type nonrec 'a elt  = ('a, is_unification, is_equality, is_moregen) elt
  type nonrec t       = (    is_unification, is_equality, is_moregen) t
end

module Unification : sig
  include Trace with type is_unification := Is_unification.yes
                 and type is_equality    := Is_equality.no
                 and type is_moregen     := Is_moregen.no

  val swap : t -> t
end

module Equality : Trace with type is_unification := Is_unification.no
                         and type is_equality    := Is_equality.yes
                         and type is_moregen     := Is_moregen.no

module Moregen : Trace with type is_unification := Is_unification.no
                        and type is_equality    := Is_equality.no
                        and type is_moregen     := Is_moregen.yes

module Subtype : sig
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val flatten : (type_expr -> type_expr -> 'a) -> t -> 'a elt list
end
