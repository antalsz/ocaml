(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                    Leo White, Jane Street Europe                    *)
(*                                                                     *)
(*  Copyright 2016 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

module Position : sig

  type t

  val filename : t -> string
  val line_number : t -> int
  val start_char : t -> int
  val end_char : t -> int

  val compare : t -> t -> int
  val hash : t -> int

end

module Location : sig

  type t

  val address : t -> Int64.t
  val symbol : t -> string option
  val position : t -> Position.t option
  val foreign : t -> bool

  val compare : t -> t -> int
  val hash : t -> int

end

module Backtrace : sig

  type t = Location.t list

  val compare : t -> t -> int
  val hash : t -> int

end

module Entry : sig

  type t

  val backtrace : t -> Backtrace.t
  val blocks : t -> int
  val words : t -> int

  val compare : t -> t -> int
  val hash : t -> int

end

module Entries : Set.S with type elt = Entry.t
                         and type t = Set.Make(Entry).t

module Stats : sig

  type t

  val minor_words : t -> int
  val promoted_words : t -> int
  val major_words : t -> int
  val minor_collections : t -> int
  val major_collections : t -> int
  val heap_words : t -> int
  val heap_chunks : t -> int
  val compactions : t -> int
  val top_heap_words : t -> int

  val compare : t -> t -> int
  val hash : t -> int

end


module Snapshot : sig

  type t

  val time : t -> float
  val stats : t -> Stats.t
  val entries : t -> Entries.t

  val compare : t -> t -> int
  val hash : t -> int

end

module Series : sig

  type t = Snapshot.t list

  val create : ?executable:string -> string -> t

end
