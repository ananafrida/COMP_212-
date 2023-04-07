
Control.Print.printDepth := 100;
use "hw05-lib.sml";

(* ---------------------------------------------------------------------- *)
(*Purpose: returns a bool whether the input tree has the specfic node element in it
Assumes *)

fun contains(t : tree, i : int) : bool = raise Fail "unimplemented"
(*case (t,i) of
 (Empty,i) => false
 | (Node (L,x,R),i) => (case (x=i) of
  true => true
  | false => (case contains(R,i) of 
  true => true
  |false => contains(L,i)
  ))*)

fun test_contains() = 
    (testb "c1" (contains (fromlist [1,2,3,4,5,6,8], 3)) true;
     testb "c2" (contains (fromlist [10,2,3,6,8], 1)) false;
     testb "c3" (contains (fromlist [10,2,3,6,8], 3)) true
     )

(* ---------------------------------------------------------------------- *)
(* quicksort on trees *)

fun combine (t1 : tree, t2 : tree) : tree = 
    case t1 of
        Empty => t2
      | Node(l1,x1,r1) => Node(combine(l1,r1),x1,t2)


(*Purpose: outputs a tree that contains elements of the tree that are less than the given integer*)
fun filter_less (t : tree, i : int) : tree = (* raise Fail "unimplemented"*)
case (t,i) of 
 (Empty,i) => Empty
 | (Node (L,x,R),i) => (case (x < i) of
 false => combine(filter_less(L,i), filter_less(R,i))
 | true => Node (filter_less(L,i), x, filter_less(R,i)))



(*Purpose: computes a tree that containes elements of the -tree that are greater than or equal to the given integer*)
fun filter_greatereq (t : tree, i : int) : tree = (*raise Fail "unimplemented"*)
case (t,i) of
 (Empty,i) => Empty
 | (Node (L,x,R),i) => (case (x >= i) of
  false => combine(filter_greatereq(L,i), filter_greatereq(R,i))
 | true => Node (filter_greatereq(L,i), x, filter_greatereq(R,i)))

(* filter is hard to test on its own because the order of the items in the resulting tree 
   is up to you.  you should try running filter on some trees in smlnj and 
   see if the results look reasonable.*)

val a_tree = Node(Node(leaf 1, 2, leaf 3), 4, Node(leaf 5, 6, leaf 7))
    

(*Purpose: computes a tree that is sorted and contains only the elements of T *)
fun quicksort_t (t : tree) : tree = (*raise Fail "unimplemented"*)
case t of
 Empty => Empty
 | Node(L,x,R) => Node(quicksort_t(filter_less(combine(L,R),x)),x,quicksort_t(filter_greatereq(combine(L,R),x)))

fun test_quicksort()=
    ((* one way to test is to convert to and from lists -- 
        this way different arrangements of the same items in a tree 
        won't affect the results
        *)
     testil "f1" (tolist (quicksort_t (fromlist [8,1,4,5,6,7,3,2]))) [1,2,3,4,5,6,7,8];
     testil "f2" (tolist (quicksort_t (fromlist [8,1,4,77,34,2,2]))) [1,2,2,4,8,34,77];
     testil "f3" (tolist (quicksort_t (fromlist [8,2]))) [2,8]
     )
    
(* ---------------------------------------------------------------------- *)
(* rebalance *)

(*Purpose takeanddrop(T,i) separates a tree T into “left” and “right” subtrees, T1 and T2 respectively. 
T1 contains the leftmost i elements of T, in their original order, and T2 the remaining elements, 
also in their original order
Examples: 
takeandrop (Node(leaf 1, 2, leaf 3),0) => (Empty, Node(leaf, 1, 2, leaf 3))
takeandrop (Node(leaf 1, 2, leaf 3), 1) (leaf 1,Node(Empty, 2, leaf 3))
*)
fun takeanddrop (t : tree, i : int) : tree * tree = (*raise Fail "unimplemented"*)
case (t,i) of 
(Empty,i) => (Empty,Empty)
| (Node(L,x,R),i) => (case i <= size(L) of
true => let val (t1,t2) = takeanddrop(L,i) 
in (t1, Node(t2,x,R)) end
| false => let val (t1,t2) = takeanddrop(R,i-size(L)-1) 
in (Node(L,x,t1), t2) end
)

fun test_tad()=
    (
     testtt "tad1" (takeanddrop (Node(leaf 1, 2, leaf 3), 0)) (Empty, Node(leaf 1, 2, leaf 3));
     testtt "tad2" (takeanddrop (Node(leaf 1, 2, leaf 3), 1)) (leaf 1, Node(Empty, 2, leaf 3));
     testtt "tad3" (takeanddrop (Node(leaf 1, 2, leaf 3), 2)) (Node(leaf 1, 2, Empty), leaf 3)
     (* write your tests here *)
     )
    
(* the rest of rebalance interms of your takeanddrop *)
fun halves (t : tree) : tree * int * tree =
    let
      val (l , vr) = takeanddrop (t , (size t) div 2)
      val (Node (Empty, v , Empty) , r) = takeanddrop (vr , 1)
    in
      (l , v , r)
    end

(*Purpose: takes a tree that is not necessarily balanced, and computes 
a balanced tree with the same elements
*)
fun rebalance (t : tree) : tree =
    case t
     of Empty => Empty
      | _ =>
        let
          val (l , x , r) = halves t
        in
          Node (rebalance l , x , rebalance r)
        end

(* ---------------------------------------------------------------------- *)

(* cargo *)

(* each shipping box is represented by a (weight, profit) pair *)
type box = int * int 

(*Purpose: Helper function of cargo which takes in a list of pairs and add the second element 
of each pair of the list
*)
fun sum(l: box list): int =
case l of
 [] => 0
 | x::xs => let val (a,b) = x
 in b + sum (xs)
 end

(*Purpose: given a list of shipping boxes as a list of pair of elements and an integer of maximum allowed weight. 
Each pair consists of weight as first and profit as second element.
Returns the list of items whose second element of the pairs will add upto the maximum value
but the first elements of the pairs still have to add up to lower than the max allowed weight
Example:
cargo([(5,22), (25,1000), (5,15), (5,19), (5,21), (5,25) ], 25) => [(25,1000)]
cargo([(5,22), (25,1000), (5,15), (5,19), (5,21), (5,25) ], 30) => [(25,1000), (5,25)]
cargo([(3,2),(4,3),(6,1),(5,4)], 8) => [(3,2),(5,4)]
*)
fun cargo(l : box list, max_weight : int) : box list =
case (l,max_weight) of
 ([],max_weight) => []
 | (l,0) => []
 | (x::xs,max_weight) => let val (w,p) = x
                         in (case (w > max_weight) of
                            true => cargo(xs,max_weight)
                            | false => let val l1 = cargo(xs,max_weight-w)
                                       in let val l2 = cargo(xs,max_weight) 
                                          in (case (p + sum(l1) > sum(l2)) of
                                              true => x::l1
                                              | false => l2
                                             )
                                          end
                                       end
                                       )
                         end

(*test function for cargo*)
fun test_cargo() =
    (testiil "c1" (cargo([(5,22), (25,1000), (5,15), (5,19), (5,21), (5,25) ], 25)) [(25,1000)];
     testiil "c2" (cargo([(5,22), (25,1000), (5,15), (5,19), (5,21), (5,25) ], 30)) [(25,1000), (5,25)];
     testiil "c3" (cargo ([(10,60),(20,100),(30,120)], 50)) [(20,100),(30,120)];
     testiil "c4" (cargo([(3,2),(4,3),(6,1),(5,4)], 8)) [(3,2),(5,4)]
     )

(* ---------------------------------------------------------------------- *)

fun run() =
    (test_contains();
     test_quicksort();
     test_tad();
     test_cargo())