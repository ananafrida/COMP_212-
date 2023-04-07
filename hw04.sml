
fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun illToString(l : int list list) : string =
    case l of
        [] => "[]"
      | x :: xs => "(" ^ ilToString x ^ ") :: " ^ illToString(xs)
            
fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")

fun testill (s : string) (n : (int list) list) (m : (int list) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ illToString m ^ "\n    Got: " ^ illToString n ^ "\n")

            
(* ---------------------------------------------------------------------- *)

(* Purpose: add n to each element of the list l
 * Examples:
 *  add_to_each ([], 7) ==> []
 *  add_to_each (1::2::3::[], 3) ==> 4::5::6::[]
 *  add_to_each (6::5::4::[], ~3) ==> 3::2::1::[]
 *)
fun add_to_each (l : int list, n : int) : int list =
    case l of
        [] => []
      | x::xs => x + n :: add_to_each (xs, n)

fun test_add_to_each() =
    (testil "ae1" (add_to_each ([], 7)) [];
     testil "ae2" (add_to_each ([1, 2, 3], 3)) [4, 5, 6];
     testil "ae2" (add_to_each ([6, 5, 4], ~3)) [3, 2, 1])

(* Purpose: computes the list of prefix sums for the argument list.  The
 *          i-th int in the result list is the sum of the first i int's
 *          in the argument list.
 * Examples:
 *  prefixSum [] ==> []
 *  prefixSum (1::2::3::[]) ==> 1::3::6::[]
 *  prefixSum (5::3::1::[]) ==> 5::8::9::[]
 *)
fun prefixSum (l : int list) : int list =
    case l of
      [] => []
    | x::xs => x :: add_to_each (prefixSum xs, x)

(* Tests for prefixSum *)
fun test_prefix_sum() =
    (testil "ps1" (prefixSum []) [];
     testil "ps2" (prefixSum [1,2,3]) [1,3,6];
     testil "ps3" (prefixSum [5,3,1]) [5,8,9])


(* Specification: takes a list l and adds n to each of it's prefix sum. The prefix sum is a list where
           i-th int in the result list is the sum of the first i int's
           in the argument list. If [x1,x2,x3] is a list and 3 is the integer, the resulting string will be 
           [x1+3,x1+x2+3,x1+x2+x3+3]

  Examples:
  prefixSumHelp ([],7) ==> []
  prefixSumHelp ([1], 7)) ==> [8]
  prefixSumHelp ()
 *)

fun prefixSumHelp (l : int list, n : int) : int list =
case (l,n) of 
 ([],n) => []
 | (x::xs,n) => (x+n) :: prefixSumHelp(xs,(x+n))

(*fun prefixSumHelp (l : int list, n : int) : int list =
case (l,n) of 
 ([],n) => []
 | (x::[],n) => [x+n]
 | (x::y::xs,n) => (x+n) :: prefixSumHelp((x+y)::xs,n)*)

(*tests for prefixSumHelp*)
fun test_pshelp() =
    (testil "psh1" (prefixSumHelp ([], 7)) [];
    testil "psh2" (prefixSumHelp ([1], 7)) [8];
    testil "psh3" (prefixSumHelp ([1,2], 7)) [8,10];
    testil "psh4" (prefixSumHelp ([1,5,10], 2)) [3,8,18]
     )


(* Purpose: computes the list of prefix sums for the argument list.  The
           i-th int in the result list is the sum of the first i int's
           in the argument list.
  Examples:
   prefixSumFast [] ==> []
   prefixSumFast [1,2,3] ==> 1::3::6
   prefixSumFast [5,3,1]] ==> 5::8::9
 *)

fun prefixSumFas
t (l : int list) : int list =
case l of 
 [] => []
 | (x::xs) => x :: prefixSumHelp(xs,x)

(*tests for prefixSumFast*)
fun test_psfast() =    
    (testil "psf1" (prefixSumFast []) [];
     testil "psf2" (prefixSumFast [2,5]) [2,7];
     testil "psf3" (prefixSumFast [10]) [10];
     testil "psf4" (prefixSumFast [6,0,3,0,4]) [6,6,9,9,13];
     testil "psf5" (prefixSumFast [0,3,7,1]) [0,3,10,11]
    )
        
(* ---------------------------------------------------------------------- *)

(*Purpose: computes the list with all and only those elements whose values are less than
the pivot p which is the bound of the function *)
fun filter_less (l : int list, bound : int) : int list = (*raise Fail "unimplemented"*)
    case (l,bound) of 
    ([],bound) => []
    | (x::xs,bound) => (case x < bound of
     true => x::filter_less(xs,bound)
     | false => filter_less(xs,bound))

(*tests for filter_less*)
fun test_filter_less() =
(
    testil "psh1" (filter_less ([1], 7)) [1];
    testil "psh2" (filter_less ([10,7,3,56,3], 7)) [3,3];
    testil "psh3" (filter_less ([1,5,10], 2)) [1]
)

(*Purpose: computes the list with all and only those elements whose values are greater than or equal to
the pivot p which is the bound of the function *)
fun filter_greatereq (l : int list, bound : int) : int list =  (*raise Fail "unimplemented"*)
    case (l,bound) of 
    ([],bound) => []
    | (x::xs,bound) => (case x >= bound of
     true => x::filter_greatereq(xs,bound)
     | false => filter_greatereq(xs,bound))

(*tests for filter_greatereq*)
fun test_filter() =
(
    testil "psh1" (filter_greatereq ([7], 7)) [7];
    testil "psh2" (filter_greatereq ([10,7,3,56,3], 7)) [10,7,56];
    testil "psh3" (filter_greatereq ([1,5,10], 2)) [5,10]
)

        
(*Purpose: for any list l, (quicksort_l l) should evaluate to a permutation of l that is sorted in the increasing order.
So, the resulted list should be a permutation of l *)
fun quicksort_l (l : int list) : int list = (*raise Fail "unimplemented"*)
 case l of 
 [] => []
 | x::xs => quicksort_l(filter_less(xs,x)) @ [x] @ quicksort_l(filter_greatereq(xs,x))


(*tests for quicksort_l*)       
fun test_qs() =
     (testil "qs1" (quicksort_l [9]) [9];
      testil "qs2" (quicksort_l [9,45,7,6]) [6,7,9,45];
      testil "qs3" (quicksort_l [9,45,700,9,8]) [8,9,9,45,700]
      )

(* ---------------------------------------------------------------------- *)

fun run() =
    (test_add_to_each();
     test_prefix_sum();
     test_pshelp();
     test_psfast();
     test_filter();
     test_qs()
     )

