
(* for testing *)

fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun slToString(l : string list) : string =
    case l of
        [] => "[]"
      | x :: xs => x ^ "::" ^ slToString(xs)

fun islToString(l : (int * string) list) : string =
    case l of
        [] => "[]"
      | (n,s) :: xs => "(" ^ Int.toString n ^ "," ^ s ^ ")" ^ "::" ^ islToString(xs)

fun testisl (s : string) (n : (int * string) list) (m : (int * string) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ islToString m ^ "\n    Got: " ^ islToString n ^ "\n")

fun testilsl (s : string) ((is,ss) : int list * string list) ((is',ss') : int list * string list) : unit =
    case (is,ss) = (is',ss') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ slToString ss' ^ "\n    Got: " ^ ilToString is ^ "," ^ slToString ss ^  "\n")

fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

fun testili (s : string) ((is,i) : int list * int) ((is',i') : int list * int) : unit =
    case (is,i) = (is',i') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ Int.toString i' ^ "\n    Got: " ^ ilToString is ^ "," ^ Int.toString i ^  "\n")
            
fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")


(* ---------------------------------------------------------------------- *)
(*Purpose: returns a list which pairs each elements with the corresponding element of the other. 
Assumes 1st list is of integers and second list is of strings
Examples:
zip ((5,1),("a","b")) ==> [(5,"a"),(1,"b")]
zip ((78,1,5),("a","s")) ==> [(78,"a"),(1,"s")]
zip ((5,10,67),("anan","b","haha","s")) ==> [(5,"anan"),(10,"b"),(67,"haha")]
*)

fun zip (l1 : int list, l2 : string list) : (int * string) list =
 case (l1,l2) of 
 ([],l2)=> []
 | (l1,[])=>[]
 | (x::xs,y::ys) =>  (x,y) :: zip(xs,ys)


(* Tests for zip *)
fun test_zip() =
    (testisl "z1" (zip ([1,2], ["a","b"])) [(1,"a"), (2,"b")];
     testisl "z2" (zip ([5,10,67],["anan","b","haha","s"])) [(5,"anan"),(10,"b"),(67,"haha")];
     testisl "z3" (zip ([78,1,5],["a","s"])) [(78,"a"),(1,"s")]
     )


(*Purpose: returns a tuple of lists where the first list in the tuple is the list of first elements 
and the second list in the tuple is the list of second elements. 
Assumes the first elements of the tuple are integers and the second elements of the tuple are strings
Examples:
unzip [(5,"a"),(1,"b")] ==> ((5,1),("a","b"))
unzip [(78,"a"),(1,"s")] ==> ((78,1,5),("a","s"))
unzip [(5,"anan"),(10,"b"),(67,"haha")] ==> ((5,10,67),("anan","b","haha","s"))
*)

fun unzip (l : (int * string) list) : int list * string list =
 case l of 
 [] => ([],[])
 | (p,q)::xs => let val (a,b) = unzip(xs)   
 in (p::a , q::b) end

(* Tests for unzip *)
fun test_unzip() =
    (testilsl "u1" (unzip [(1,"a"), (2,"b")]) ([1,2], ["a","b"]);
     testilsl "u2" (unzip [(5,"anan"),(10,"b"),(67,"haha")]) ([5,10,67],["anan","b","haha"]);
     testilsl "u3" (unzip [(78,"a"),(1,"s")]) ([78,1],["a","s"])
     )




fun lasHelp (l : int list, y : int) : int list * int =
case (l,y) of 
([],y) => ([],0)
| (a::xs,y) =>
(case a=y of 
  false => (l,0)
  | true => let val (p,cnt) = lasHelp(xs,y)
  in (p,cnt+1)
  end
  )

(* Tests for lasHelp *)
fun test_lasHelp() =
    (testili "help1" (lasHelp ([2], 1)) ([2], 0);
     testili "help2" (lasHelp ([2,2,3,4], 2)) ([3,4], 2);
     testili "help3" (lasHelp ([1,1,1,2], 1)) ([2], 3)
     )


(*Purpose: returns a list of look and say sequence 
Assumes l is a list of integers only
Examples:
look_and_say [2,2,2] ==> [3,2]
look_and_say [1,2,2] ==> [1,1,2,2]
look_and_say [2,3,4,4,5,3,3] ==> [1,2,1,3,2,4,1,5,2,3]
*)

fun look_and_say (l : int list) : int list = (*raise Fail "las unimplemented"*)
case l of
 [] => []
 | x::xs => let val (m,p) = lasHelp (xs,x) 
 in (p+1)::x::look_and_say(m) end


(* Tests for look_and_say *)
fun test_look_and_say() =
    (testil "las1" (look_and_say [1]) [1,1];
     testil "las2" (look_and_say [1,2,2,2,10,2]) [1,1,3,2,1,10,1,2];
     testil "las3" (look_and_say []) []
     )



(*Purpose: returns a bool on whether the input list has a subset that sums to the target number n
Assumes the list is of integers and the number is an integer
Examples:
subset_sum ([], 0)) ==> true
subset_sum ([1,5,2,-4,0],5) ==> true
subset_sum ([3,4,6,-3,1],2) ==> false
*)

fun subset_sum (l : int list, s : int) : bool = (*raise Fail "subsetsum unimplemented"*)
 case (l,s) of
  (l,0)=> true
  | ([],s)=> (case s of
              0 => true
              | s => false)
  | (x::xs,s)=> subset_sum (xs,s-x) orelse subset_sum(xs,s) (*if used in a let val manner, the time complexity
  and everything would have been the same*)

      (*if s = 0 then true
      else false
  | subset_sum (_ : int list, 0 : int) : bool * int list = true
  | subset_sum (x::xs : int list, s : int) : bool * int list =
      let val ((w,wList),(wo,woList)) =
      (subset_sum(xs, s-x), subset_sum(xs, s)) in
        case (w,wo) of
          (true,_) => true
        | (_,true) => true
        | (_,_) => false
      end*)


(* Tests for subset_sum *)
fun test_subset_sum() =
    (testb "ss1" ( subset_sum ([], 0)) true
     (* write your tests here *)
     )

    
fun inverse_adjacent(n : int) : real = (*raise Fail "inverse_adjacent unimplemented"*)
case n of 
 1 => 0.5
| _ => let val x = inverse_adjacent(n-1) in x + (1.0/(real(n)*(real(n)+1.0))) end
    
fun show(r : real) : string = Real.fmt (StringCvt.FIX (SOME 20)) r

val s = show(200.0/201.0)

    
    
fun run() =
    (test_zip();
     test_unzip();
     test_lasHelp();
     test_look_and_say();
     test_subset_sum())