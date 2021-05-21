(* TEST
   * expect
*)

class type virtual ['a] c = object constraint 'a = [<`A of int & float] end
[%%expect {|
Line 1, characters 0-75:
1 | class type virtual ['a] c = object constraint 'a = [<`A of int & float] end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type of this class,
       class virtual ['a] c :
         object constraint 'a = _[< `A of int & float ] end,
       contains non-collapsible conjunctive types in constraints.
       Type int is not compatible with type float
|}]

class type ct = object
  method x : int
end

class c (y : 'a * float) : ct = object
  method x = y
end
[%%expect{|
|}]
