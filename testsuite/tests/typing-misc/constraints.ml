(* TEST
   * expect
*)

type 'a t = [`A of 'a t t] as 'a;; (* fails *)
[%%expect{|
Line 1, characters 0-32:
1 | type 'a t = [`A of 'a t t] as 'a;; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of t contains a cycle:
       'a t t as 'a
|}, Principal{|
Line 1, characters 0-32:
1 | type 'a t = [`A of 'a t t] as 'a;; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of t contains a cycle:
       [ `A of 'a t t ] as 'a
|}];;
type 'a t = [`A of 'a t t];; (* fails *)
[%%expect{|
Line 1, characters 0-26:
1 | type 'a t = [`A of 'a t t];; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the definition of t, type 'a t t should be 'a t
|}];;
type 'a t = [`A of 'a t t] constraint 'a = 'a t;; (* fails since 4.04 *)
[%%expect{|
Line 1, characters 0-47:
1 | type 'a t = [`A of 'a t t] constraint 'a = 'a t;; (* fails since 4.04 *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
|}];;
type 'a t = [`A of 'a t] constraint 'a = 'a t;; (* fails since 4.04 *)
[%%expect{|
Line 1, characters 0-45:
1 | type 'a t = [`A of 'a t] constraint 'a = 'a t;; (* fails since 4.04 *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
|}];;
type 'a t = [`A of 'a] as 'a;;
[%%expect{|
type 'a t = 'a constraint 'a = [ `A of 'a ]
|}, Principal{|
type 'a t = [ `A of 'b ] as 'b constraint 'a = [ `A of 'a ]
|}];;
type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)
[%%expect{|
Line 1, characters 0-41:
1 | type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of v contains a cycle:
       t
|}];;

type 'a t = 'a;;
let f (x : 'a t as 'a) = ();; (* ok *)
[%%expect{|
type 'a t = 'a
val f : 'a -> unit = <fun>
|}];;

let f (x : 'a t) (y : 'a) = x = y;;
[%%expect{|
val f : 'a t -> 'a -> bool = <fun>
|}];;

(* PR#6505 *)
module type PR6505 = sig
  type 'o is_an_object = < .. > as 'o
  and 'o abs constraint 'o = 'o is_an_object
  val abs : 'o is_an_object -> 'o abs
  val unabs : 'o abs -> 'o
end
;; (* fails *)
[%%expect{|
Line 3, characters 2-44:
3 |   and 'o abs constraint 'o = 'o is_an_object
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of abs contains a cycle:
       'a is_an_object as 'a
|}];;

module PR6505a = struct
  type 'o is_an_object = < .. > as 'o
  and ('k,'l) abs = 'l constraint 'k = 'l is_an_object
  let y : ('o, 'o) abs = object end
end;;
let _ = PR6505a.y#bang;; (* fails *)
[%%expect{|
module PR6505a :
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    and ('a, 'l) abs = 'l constraint 'a = 'l is_an_object
    val y : (<  > is_an_object, <  > is_an_object) abs
  end
Line 6, characters 8-17:
6 | let _ = PR6505a.y#bang;; (* fails *)
            ^^^^^^^^^
Error: This expression has type
         (<  > PR6505a.is_an_object, <  > PR6505a.is_an_object) PR6505a.abs
       It has no method bang
|}, Principal{|
module PR6505a :
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    and ('a, 'l) abs = 'l constraint 'a = 'l is_an_object
    val y : (<  >, <  >) abs
  end
Line 6, characters 8-17:
6 | let _ = PR6505a.y#bang;; (* fails *)
            ^^^^^^^^^
Error: This expression has type (<  >, <  >) PR6505a.abs
       It has no method bang
|}]

module PR6505b = struct
  type 'o is_an_object = [> ] as 'o
  and ('k,'l) abs = 'l constraint 'k = 'l is_an_object
  let x : ('a, 'a) abs = `Foo 6
end;;
let () = print_endline (match PR6505b.x with `Bar s -> s);; (* fails *)
[%%expect{|
module PR6505b :
  sig
    type 'o is_an_object = 'o constraint 'o = [>  ]
    and ('a, 'l) abs = 'l constraint 'a = 'l is_an_object
    val x : (([> `Foo of int ] as 'a) is_an_object, 'a is_an_object) abs
  end
Line 6, characters 23-57:
6 | let () = print_endline (match PR6505b.x with `Bar s -> s);; (* fails *)
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`Foo _
Exception: Match_failure ("", 6, 23).
|}]

type 'a t = T
  constraint 'a = int
  constraint 'a = float
[%%expect{|
Line 3, characters 13-23:
3 |   constraint 'a = float
                 ^^^^^^^^^^
Error: The type constraints are not consistent.
       Type int is not compatible with type float
|}]

type ('a,'b) t = T
  constraint 'a = int -> float
  constraint 'b = bool -> char
  constraint 'a = 'b
[%%expect{|
Line 4, characters 13-20:
4 |   constraint 'a = 'b
                 ^^^^^^^
Error: The type constraints are not consistent.
       Type int -> float is not compatible with type bool -> char
       Type int is not compatible with type bool
|}]

class type ['a, 'b] a = object
  constraint 'a = 'b
  constraint 'a = int * int
  constraint 'b = float * float
end;;
[%%expect{|
Line 4, characters 2-31:
4 |   constraint 'b = float * float
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class constraints are not consistent.
       Type int * int is not compatible with type float * float
       Type int is not compatible with type float
|}]
