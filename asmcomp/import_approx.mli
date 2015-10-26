(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Create simple value approximations from the export information in
    .cmx files. *)

(** Given an approximation description, load .cmx files (possibly more
    than one) until the description is fully resolved.  If a necessary .cmx
    file cannot be found, "unresolved" will be returned. *)
val really_import : Simple_value_approx.descr -> Simple_value_approx.descr

(** Maps the description of the given approximation through [really_import]. *)
val really_import_approx : Simple_value_approx.t -> Simple_value_approx.t

(** Read and convert the approximation of a given identifier from the
    relevant .cmx file.  Unlike the "really_" functions, this does not
    continue to load .cmx files until the approximation is fully
    resolved. *)
val import_global : Ident.t -> Simple_value_approx.t

(** Like [import_global], but for symbols. *)
val import_symbol : Symbol.t -> Simple_value_approx.t
