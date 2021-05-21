(* TEST
   * expect
*)

type i = int

type 'a t = T : i
[%%expect{|
|}]

type 'a t = T : i t
type 'a s = 'a t = T : i t
[%%expect{|
|}]

type 'a t = T : i s
and  'a s = 'a t
[%%expect{|
|}]
