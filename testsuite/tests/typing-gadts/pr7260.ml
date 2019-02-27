(* TEST
   * expect
*)

type bar = < bar: unit >

type _ ty = Int : int ty

type dyn = Dyn : 'a ty -> dyn;;

class foo =
  object (this)
    method foo (Dyn ty) =
      match ty with
      | Int -> (this :> bar)
  end;;  (* fail, but not for scope *)

[%%expect{|
type bar = < bar : unit >
type _ ty = Int : int ty
type dyn = Dyn : 'a ty -> dyn
Line 8, characters 2-96:
 8 | ..object (this)
 9 |     method foo (Dyn ty) =
10 |       match ty with
11 |       | Int -> (this :> bar)
12 |   end.................................
Warning 17: the virtual method bar is not declared.
Line 7, characters 0-108:
 7 | class foo =
 8 |   object (this)
 9 |     method foo (Dyn ty) =
10 |       match ty with
11 |       | Int -> (this :> bar)
12 |   end.................................
Error: This class should be virtual.
       The following methods are undefined : bar
|}];;
