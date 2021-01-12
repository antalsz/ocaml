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
type unification     = private Unification
type non_unification = private Non_unification

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type 'variety variant =
  (* Common *)
  | Incompatible_types_for : string -> _ variant
  (* Unification *)
  | No_intersection : unification variant
  | No_tags : position * (Asttypes.label * row_field) list -> unification variant
  | Fixed_row : position * fixed_row_case * fixed_explanation -> unification variant
  (* Equality & Moregen *)
  | Openness : position (* Always [Second] for Moregen *) -> non_unification variant
  | Missing : position * Asttypes.label -> non_unification variant
(* ASZ: Missing → No_tags, it's just better *)

type 'variety obj =
  (* Common *)
  | Missing_field : position * string -> _ obj
  | Abstract_row : position -> _ obj
  (* Unification *)
  | Self_cannot_be_closed : unification obj

type ('a, 'variety) elt =
  (* Common *)
  | Diff : 'a diff -> ('a, _) elt
  | Variant :  'variety variant -> ('a, 'variety) elt
  | Obj :  'variety obj -> ('a, 'variety) elt
  | Escape : 'a escape -> ('a, _) elt
  | Incompatible_fields : { name:string; diff: type_expr diff } -> ('a, _) elt
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) elt
  (* ASZ: Either [Rec_occur] doesn't belong in Moregen or it'll just be easier to put it in all three *)
(* ASZ: Why isn't [Incompatible_fields] in [Obj]?  Meh, doesn't matter. *)

type 'variety t =
  (desc, 'variety) elt list

let diff got expected = Diff (map_diff short { got; expected })

let map_elt (type variety) f : ('a, variety) elt -> ('b, variety) elt = function
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


let swap_unification_elt = function
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

let swap_unification_trace e = List.map swap_unification_elt e

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

