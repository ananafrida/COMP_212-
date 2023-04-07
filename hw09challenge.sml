(*  

   NON-COLLABORATIVE CHALLENGE PROBLEM
   25 points

   All of the previous homeworks have had copied and pasted test
   functions for each type -- many of these are below.
   The 'print' function prints strings on the screen.  

   Use higher-order functions and polymorphism to abstract this repeated
   code, avoiding as much code duplication as possible.  

   Show how to give more concise definations of the test functions below
     testi
     testii
     testil
     testisl
     testilsl
     testb
     testill
     testr
     testrr
   as instances of your abstract version.  

   The way that the your test function prints the output and expected output
   does not have to match the functions below exactly, as long 
   as the necessary information is displayed unambiguously
   (e.g. in can differ in whitespace, parentheses).  

*)

fun newtest (s: string, convert: 'a -> string, n: 'a, m: 'a, 
compare: 'a * 'a -> bool, p: string * 'a * 'a * ('a -> string) -> unit): unit =
    case compare (n,m) of
        true => print ("Test " ^ s ^ " OK\n")
        | false => p(s,n,m,convert)
       (* | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ convert(m) ^ "\n    Got: " ^ convert(n) ^ "\n") *)

fun newtestpair (s: string, convert: 'a -> string, n: 'a * 'a, m: 'a * 'a, 
compare: ('a * 'a) * ('a * 'a) -> bool, p: string * ('a * 'a) * ('a * 'a) * ('a -> string) -> unit): unit =
    case compare (n,m) of
        true => print ("Test " ^ s ^ " OK\n")
        | false => p(s,n,m,convert)

fun newtestdifpair (s: string, convert1: 'a -> string, convert2: 'b -> string, n: 'a * 'b, m: 'a * 'b, 
compare: ('a * 'b) * ('a * 'b) -> bool, p: string * 
('a * 'b) * ('a * 'b) * ('a -> string) * ('b -> string)-> unit): unit =
    case compare (n,m) of
        true => print ("Test " ^ s ^ " OK\n")
        | false => p(s,n,m, convert1, convert2)

fun printnonpair (s: string, n: 'a, m: 'a, convert: 'a -> string): unit = 
print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ convert(m) ^ "\n    Got: " ^ convert(n) ^ "\n")

fun printpair (s: string, n: 'a * 'a, m: 'a * 'a, convert: 'a -> string): unit =
    let val (x,y) = n
        val (x',y') = m 
    in
        print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ convert (x') ^ " , " ^ convert (y')  ^ "\n    Got: " ^ convert(x) ^ " , " ^ convert(y) ^ "\n")
    end

fun printdifpair (s: string, n: 'a * 'b, m: 'a * 'b, convert1: 'a -> string, convert2: 'b -> string): unit =
    let val (x,y) = n
        val (x',y') = m 
    in
        print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ convert1 (x') ^ " , " ^ convert2 (y')  ^ "\n    Got: " ^ convert1(x) ^ " , " ^ convert2(y) ^ "\n")
    end


(***************************abstract section***********************************)
fun testi (s: string) (n: int) (m: int): unit =
newtest(s, Int.toString, n, m, fn(n,m) => n = m, printnonpair)

fun testii (s : string) (n : int * int) (m : int * int) : unit =
newtestpair(s, Int.toString, n, m, fn(n,m) => n = m, printpair)

(*helper function of testilsl, testil*)
fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun testil (s : string) (n : int list) (m : int list) : unit = 
newtest(s, ilToString, n, m, fn(n,m) => n = m, printnonpair)

(*helper function of testisl*)
fun islToString(l : (int * string) list) : string =
    case l of
        [] => "[]"
      | (n,s) :: xs => "(" ^ Int.toString n ^ "," ^ s ^ ")" ^ "::" ^ islToString(xs)

fun testisl (s : string) (n : (int * string) list) (m : (int * string) list) : unit =
newtest(s, islToString, n, m, fn(n,m) => n = m, printnonpair)

(*helper function of testilsl*)
fun slToString(l : string list) : string =
    case l of
        [] => "[]"
      | x :: xs => x ^ "::" ^ slToString(xs)

fun testilsl (s : string) ((is,ss) : int list * string list) ((is',ss') : int list * string list) : unit =
newtestdifpair(s, ilToString, slToString, (is,ss), (is',ss'), fn((is,ss),(is',ss')) => (is,ss) = (is',ss'), printdifpair)

fun testb (s : string) (n : bool) (m : bool) : unit =
newtest(s, Bool.toString, n, m, fn(n,m) => n = m, printnonpair)

(*helper function of testill*)
fun illToString(l : int list list) : string =
    case l of
        [] => "[]"
      | x :: xs => "(" ^ ilToString x ^ ") :: " ^ illToString(xs)

fun testill (s : string) (n : (int list) list) (m : (int list) list) : unit =
newtest(s, illToString, n, m, fn(n,m) => n = m, printnonpair)


val epsilon = 0.000000000001

fun testr (s : string) (n : real) (m : real) : unit =
newtest(s, Real.toString, n, m, fn(n,m) => Real.abs(n - m) < epsilon, printnonpair)

fun testrr (s : string) (n : real * real) (m : real * real) : unit =
newtestpair(s, Real.toString, n, m, fn(n,m) => 
    let val (x,y) = n
        val (x',y') = m
    in
Real.abs(x - x') < epsilon andalso Real.abs(y - y') < epsilon
    end, printpair)


(*non abstract section*)

(*fun testilsl (s : string) ((is,ss) : int list * string list) ((is',ss') : int list * string list) : unit =
    case (is,ss) = (is',ss') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ slToString ss' ^ "\n    Got: " ^ ilToString is ^ "," ^ slToString ss ^  "\n")*)
