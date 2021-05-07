(* TEST
   * expect
*)

module M : sig
  val x : bool * int
end = struct
  let x = false , "not an int"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = false , "not an int"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : bool * string end
       is not included in
         sig val x : bool * int end
       Values do not match:
         val x : bool * string
       is not included in
         val x : bool * int
|}]

module T : sig
  val f : int -> (float * string ref) list
end = struct
  let f x = x + List.length [0.0, ref true]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = x + List.length [0.0, ref true]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> (float * string ref) list end
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> (float * string ref) list
|}]

(* Alpha-equivalence *)
module T : sig
  val f : ('a list * 'b list -> int)
end = struct
  let f : ('c list * 'd ref  -> int) = assert false
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : ('c list * 'd ref  -> int) = assert false
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'c list * 'd ref -> int end
       is not included in
         sig val f : 'a list * 'b list -> int end
       Values do not match:
         val f : 'c list * 'd ref -> int
       is not included in
         val f : 'a list * 'b list -> int
|}]

module T : sig
  type t = int * float
end = struct
  type t = bool * float
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = bool * float
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = bool * float end
       is not included in
         sig type t = int * float end
       Type declarations do not match:
         type t = bool * float
       is not included in
         type t = int * float
|}]
