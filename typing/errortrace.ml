open Types
open Format

type position = First | Second

let swap_position = function
  | First -> Second
  | Second -> First

let print_pos ppf = function
  | First -> fprintf ppf "first"
  | Second -> fprintf ppf "second"

type desc = { t: type_expr; expanded: type_expr option }
type 'a diff = { got: 'a; expected: 'a}

let short t = { t; expanded = None }
let map_diff f r =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f r.got in
  let expected = f r.expected in
  { got; expected}

let flatten_desc f x = match x.expanded with
  | None -> f x.t x.t
  | Some expanded -> f x.t expanded

let swap_diff x = { got = x.expected; expected = x.got }

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

let explain trace f =
  let rec explain = function
    | [] -> None
    | [h] -> f ~prev:None h
    | h :: (prev :: _ as rem) ->
      match f ~prev:(Some prev) h with
      | Some _ as m -> m
      | None -> explain rem in
  explain (List.rev trace)

(* Type indices *)
module type Type_bool = sig
  type yes = private Yes
  type no  = private No
end

module Type_bool = struct
  type yes = private Yes
  type no  = private No
end

module Is_unification : Type_bool = Type_bool
module Is_equality    : Type_bool = Type_bool
module Is_moregen     : Type_bool = Type_bool

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type ('is_equality, 'is_moregen) openness =
  | WhenEquality : position -> (Is_equality.yes, Is_moregen.no) openness
  | WhenMoregen : (Is_equality.no, Is_moregen.yes) openness

type ('is_unification, 'is_equality, 'is_moregen) variant =
  (* Common *)
  | Incompatible_types_for : string -> ('maybe_unification, 'maybe_equality, 'maybe_moregen) variant
  (* Unification *)
  | No_intersection : (Is_unification.yes, Is_equality.no, Is_moregen.no) variant
  | No_tags : position * (Asttypes.label * row_field) list -> (Is_unification.yes, Is_equality.no, Is_moregen.no) variant
  | Fixed_row : position * fixed_row_case * fixed_explanation -> (Is_unification.yes, Is_equality.no, Is_moregen.no) variant
  (* Equality & Moregen *)
  | Openness : ('maybe_equality, 'maybe_moregen) openness -> (Is_unification.no, 'maybe_equality, 'maybe_moregen) variant
  | Missing :  position * Asttypes.label -> (Is_unification.no, 'maybe_equality, 'maybe_moregen) variant
(* ASZ: Missing → No_tags, it's just better *)


type ('is_unification, 'is_equality, 'is_moregen) obj =
  (* Common *)
  | Missing_field : position * string -> ('maybe_unification, 'maybe_equality, 'maybe_moregen) obj
  | Abstract_row : position -> ('maybe_unification, 'maybe_equality, 'maybe_moregen) obj
  (* Unification *)
  | Self_cannot_be_closed : (Is_unification.yes, Is_equality.no, Is_moregen.no) obj

type ('a, 'is_unification, 'is_equality, 'is_moregen) elt =
  (* Common *)
  | Diff : 'a diff -> ('a, 'maybe_unification, 'maybe_equality, 'maybe_moregen) elt
  | Variant :  ('maybe_unification, 'maybe_equality, 'maybe_moregen) variant -> ('a, 'maybe_unification, 'maybe_equality, 'maybe_moregen) elt
  | Obj :  ('maybe_unification, 'maybe_equality, 'maybe_moregen) obj -> ('a, 'maybe_unification, 'maybe_equality, 'maybe_moregen) elt
  | Escape : 'a escape -> ('a, 'maybe_unification, 'maybe_equality, 'maybe_moregen) elt
  | Incompatible_fields : { name:string; diff: type_expr diff } -> ('a, 'maybe_unification, 'maybe_equality, 'maybe_moregen) elt
  (* Unification & Moregen *)
  | Rec_occur : type_expr * type_expr -> ('a, 'maybe_unification, Is_equality.no, 'maybe_moregen) elt
  (* ASZ: Either [Rec_occur] doesn't belong in Moregen or it'll just be easier to put it in all three *)
(* ASZ: Why isn't [Incompatible_fields] in [Obj]?  Meh, doesn't matter. *)

(* U | UM* | EM *)

type ('is_unification, 'is_equality, 'is_moregen) t =
  (desc, 'is_unification, 'is_equality, 'is_moregen) elt list

let diff got expected = Diff (map_diff short { got; expected })

let map_elt (type u e m) f : ('a, u, e, m) elt -> ('b, u, e, m) elt = function
  | Diff x -> Diff (map_diff f x)
  | Escape { kind = Equation x; context} -> Escape { kind = Equation (f x); context }
  | Escape { kind = (Univ _ | Self | Constructor _ | Module_type _ | Constraint); _ }
  | Variant _ | Obj _ | Incompatible_fields _ as x -> x
  | Rec_occur (_, _) as x -> x

let map f t = List.map (map_elt f) t

(* Convert desc to type_expr * type_expr *)
let flatten f = map (flatten_desc f)

let incompatible_fields name got expected =
  Incompatible_fields { name; diff={got; expected} }

module type Trace_params = sig
  type is_unification
  type is_equality
  type is_moregen
end

module type Trace = sig
  include Trace_params

  type nonrec variant = (    is_unification, is_equality, is_moregen) variant
  type nonrec obj     = (    is_unification, is_equality, is_moregen) obj
  type nonrec 'a elt  = ('a, is_unification, is_equality, is_moregen) elt
  type nonrec t       = (    is_unification, is_equality, is_moregen) t
end

module Make_trace (T : Trace_params) : Trace with type is_unification := T.is_unification
                                              and type is_equality    := T.is_equality
                                              and type is_moregen     := T.is_moregen =
struct
  type nonrec variant = (    T.is_unification, T.is_equality, T.is_moregen) variant
  type nonrec obj     = (    T.is_unification, T.is_equality, T.is_moregen) obj
  type nonrec 'a elt  = ('a, T.is_unification, T.is_equality, T.is_moregen) elt
  type nonrec t       = (    T.is_unification, T.is_equality, T.is_moregen) t
end

module Unification = struct
  include Make_trace (struct
      type is_unification = Is_unification.yes
      type is_equality    = Is_equality.no
      type is_moregen     = Is_moregen.no
    end)

  let swap_elt = function
    | Diff x -> Diff (swap_diff x)
    | Incompatible_fields { name; diff } ->
      Incompatible_fields { name; diff = swap_diff diff}
    | Obj (Missing_field(pos,s)) -> Obj (Missing_field(swap_position pos,s))
    | Obj (Abstract_row pos) -> Obj (Abstract_row (swap_position pos))
    | Variant (Fixed_row(pos,k,f)) ->
      Variant (Fixed_row(swap_position pos,k,f))
    | Variant (No_tags(pos,f)) ->
      Variant (No_tags(swap_position pos,f))
    | x -> x

  let swap e = List.map swap_elt e
end

module Equality = Make_trace (struct
    type is_unification = Is_unification.no
    type is_equality    = Is_equality.yes
    type is_moregen     = Is_moregen.no
  end)

module Moregen = Make_trace (struct
    type is_unification = Is_unification.no
    type is_equality    = Is_equality.no
    type is_moregen     = Is_moregen.yes
  end)

module Subtype = struct
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  let diff got expected = Diff (map_diff short {got;expected})

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)

  let map f t = List.map (map_elt f) t

  let flatten f t = map (flatten_desc f) t
end

