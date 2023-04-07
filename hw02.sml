(* helper functions for testing -- you don't need to read these! *)

(* test a function that returns an int *)
fun testi (s : string) (n : int) (m : int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString m ^ "\n    Got: " ^ Int.toString n ^ "\n")

fun testii (s : string) (n : int * int) (m : int * int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => let val (x,y) = n
                     val (x',y') = m
                 in
                     print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString x' ^ " , " ^ Int.toString y'  ^ "\n    Got: " ^ Int.toString x ^ " , " ^ Int.toString y ^ "\n")
                 end

(* ********************************************************************** *)

(* READ THIS COMMENT!
 *
 * In this file there are various lines marked by a comment like so:
 *
 *    raise Fail "unimplemented"  (* DELETE THIS LINE *)
 *
 * You do not need to delete these lines immediately, but they should be gone by
 * the time you hand in your homework. They are placeholders for your
 * implementations of the functions specified in the homework. Without them,
 * this file would not load.
 *
 * If you remove such a line without implementing the function it is associated
 * with, this file will not load. Only remove such lines when you are ready to
 * implement their associated function.
 *)
(* Purpose: returns true if n is even, false otherwise.
   Assumes n is a natural number *)
fun evenP (n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP (n-2)

(* Purpose: returns true if n is odd, false otherwise.
   Assumes n is a natural number *)
fun oddP (n : int) : bool =
    case n
     of 0 => false
      | 1 => true
      | _ => oddP (n-2)

(* Purpose: returns m + n. Assumes m and n are natural numbers. *)
fun add (m : int, n : int) =
  case m of
    0 => n
  | _ => 1 + (add (m - 1, n))

(* Task: Implement and document this function. *)
(* DOCUMENTATION GOES HERE *)
(* Purpose: returns m * n. Assumes m and n are natural numbers
   Examples:
   mult 4*3 ==> 12
   mult 0*3 ==> 0
   mult 3*0 ==> 0
   mult 10*3 ==> 30
 *)

fun mult (m : int, n : int) : int =
    case n of 
    0 => 0
    | _ => add(m, mult(m, n-1))

(*testing function for mult*)
fun test_mult() =
    (testi "m1" (mult(4,3)) 12;
     testi "m2" (mult(0,3)) 0;
     testi "m3" (mult(4,0)) 0;
     testi "m4" (mult(10,3)) 30
     )
        
(* Task: Implement and document this function. *)
(* DOCUMENTATION GOES HERE *)
(* Purpose: returns (n/2, n/2) or ((n/2)+1, n/2). Assumes n is a natural number
   Examples:
   halves 12 ==> (6,6)
   halves 7 ==> (4,3)
   halves 4 ==> (2,2)
   halves 23 ==> (12,11)
 *)

fun halves (n : int) : int * int =
    case n of 
    0 => (0,0)
    |1 => (1,0)
    | _ => let val (x,y) = halves(n-2) in (x+1,y+1) end

(*testing function for halves*)
fun test_halves() = 
    (testii "h1" (halves(4)) (2,2);
     testii "h2" (halves(5)) (3,2);
     testii "h3" (halves(100)) (50,50);
     testii "h4" (halves(23)) (12,11)
     )

(* Task: Implement this function. *)
(*  DOCUMENTATION GOES HERE *)
(* Purpose: returns the modulo fo a number. Assumes n and d are natural numbers
   Examples:
   divmod (100,10) ==> (10,0);
   divmod (101,20) ==> (5,1);
   divmod (230,20)==> (11,10)
 *)

fun divmod (n : int, d : int) : int * int =
    case n<d of 
      true => (0,n)
      | false => (let val (q,r) = divmod (n-d,d)
       in (q+1,r) end)

(*testing function for divmod*)
fun test_divmod() =
    (testii "d1" (divmod(100,10)) (10,0);
     testii "d2" (divmod(101,20)) (5,1);
     testii "d3" (divmod(230,20)) (11,10)
     )

(* Task: Implement this function. *)
(* DOCUMENTATION GOES HERE *)
(* Purpose: returns sum of all digits. Assumes n and b is a natural number where b>1
   Examples:
   sum_digits (12,10) ==> 3
   sum_digits (34,6) ==> 9
   sum_digits (12,2) ==> 2
 *)

fun sum_digits (n : int, b : int) : int =
    case (n,b) of 
    (0,b) => 0
    | _ => (let val (q,r) = divmod (n,b) in sum_digits(q,b) + r end)

(*testing function for sum_digits*)
fun test_sum_digits() =
    (testi "s1" (sum_digits(123,10)) 6;
     testi "s2" (sum_digits(7234,10)) 16;
     testi "s3" (sum_digits (34,6)) 9;
     testi "s4" (sum_digits(12,2)) 2
     )

fun run() =
    (test_mult();
     test_halves();
     test_divmod();
     test_sum_digits())

