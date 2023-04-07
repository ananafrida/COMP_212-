use "hw07-lib.sml";
exception Unimplemented

(* ---------------------------------------------------------------------- *)
(* map *)

fun pluralize_rec (t : string tree) : string tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (x ^ "s")
      | Node(l,r) => Node(pluralize_rec l , pluralize_rec r)

fun mult_rec (c : int, t : int tree) : int tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (c * x)
      | Node(l,r) => Node(mult_rec (c,l) , mult_rec (c,r))

(*
Purpose: given two integers, outputs the sum of them. 
Also a helper function of pluralize 
*)
fun add_s (x: string) : string = x ^ "s" (*output type of the function is qiuestionable*)

(*
Purpose: given a function and a tree of type 'a, computes a tree of type 'b by changing the 
nodes of the tree with the output of the function and iterating through them.
*)
fun map (f : 'a -> 'b, t : 'a tree) : 'b tree =
   case t of
   Empty => Empty
   | Leaf x => Leaf (f x)
   | Node(l,r) => Node(map (f,l) , map (f,r)) (*********why not f x. how f is working?
    ans: calling the function in previous line 29 "Leaf x => Leaf (f x)"
   but in line 30, map is not calling the function, map is just calling itself 
   recursively and we are just passing the function f as an arguement
   where we don't need the arguement of the function f itself*)

(*
Purpose: a function that (given a tree t) evaluates to a tree t', where the tree has the same structure as
tree t, and the string at each leaf of t' is the string at the corresponding position 
in t, with an ’s’ affixed to the end.
 *)
fun pluralize (t : string tree) : string tree = (*raise Unimplemented*) map (add_s,t)

(*
Purpose: a function that (given a tree t) evaluates to a tree t’, where t’ has 
the same structure as t, and the int at each leaf of t’ 
is the int at the corresponding position in t, multiplied by c
 *)
fun mult (c : int, t : int tree) : int tree = (*raise Unimplemented*) map (fn x => x*c, t)

fun test_pluralize() =
    (testsl "p1" (tolist (pluralize (fromlist ["a","b","c"]))) ["as","bs","cs"];
    testsl "p2" (tolist (pluralize (fromlist ["dog","cat","bear"]))) ["dogs","cats","bears"];
    testsl "p3" (tolist (pluralize (fromlist ["book","b","c","","table"]))) ["books","bs","cs","s","tables"]
    )

fun test_mult() =
    (
    testil "m1" (tolist (mult (4,fromlist [1,2,3]))) [4,8,12];
    testil "m2" (tolist (mult (1,fromlist [1,2,3]))) [1,2,3];
    testil "m3" (tolist (mult (0,fromlist []))) []
    )


(* ---------------------------------------------------------------------- *)
(* reduce *)

fun sum_rec (t : int tree) : int =
    case t of
        Empty => 0
      | Leaf x => x
      | Node(t1,t2) => (sum_rec t1) + (sum_rec t2)

fun join_rec (t : string tree) : string =
    case t of
        Empty => ""
      | Leaf x => x
      | Node(t1,t2) => (join_rec t1) ^ (join_rec t2)

(* 
Purpose: a function that (given a function and a type 'a and a tree of type 'a 
as the arguements) evaluates to type 'a by implement the function over the leafs of the tree 
*)
fun reduce (n : 'a * 'a -> 'a, b : 'a, t : 'a tree) : 'a = (*raise Unimplemented*)
 case t of 
   Empty => b
   | Leaf x => x
   | Node(t1,t2) => n(reduce (n,b,t1), reduce(n,b,t2))

fun add(x: int, y: int): int = x + y
fun concatenate(x: string, y: string): string = x ^ y

(* 
Purpose: a function that takes an integer tree t as an arguement and 
evaluates to a integer n, where n is the sum of all of the 
numbers at the leaves of t
*)        
fun sum (t : int tree) : int = reduce(add,0,t)

(*
Purpose: a function that takes a string tree as an arguement
evaluates to a string s, where s is the concatenation of all 
of the strings at the leaves of t, in order from left to right
*)
fun join (t : string tree) : string = reduce(concatenate,"",t)

fun test_sum() =
    (
    testi "s1" (sum (fromlist [1,2,3,4])) 10;
    testi "s2" (sum (fromlist [5,2,4])) 11;
    testi "s3" (sum (fromlist [])) 0
    )

fun test_join() =
    (
    tests "j1" (join (fromlist ["a","b","c"])) "abc";
    tests "j2" (join (fromlist ["a","y","c","g"])) "aycg";
    tests "j3" (join (fromlist ["a","c"])) "ac"
    )

(* ---------------------------------------------------------------------- *)
(* programming with map and reduce *)

(* 
Purpose: a function that (given a tree of tree) computes a tree that contains all of the 
elements of all of the trees in t. The elements of each tree t1 in t 
should occur in flatten t in the same order in which they occur in t1; 
if a tree t1 is to the left of a tree t2 in t, the elements of t1 should 
occur to the left of the elements of t2 in flatten t.

For example: flatten (Node (Leaf (Node (Leaf 1, Leaf 2)), Node (Leaf (Leaf 3), Empty))) 
== Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty))
*)
fun anal(x: 'n tree, y: 'n tree): 'n tree = Node (x,y)

fun flatten (t : ('a tree) tree) : 'a tree = (*raise Unimplemented*)reduce(anal,Empty,t)

fun test_flatten() =
    (
    testil "f1" (tolist (flatten (fromlist [ fromlist [1,2,3], fromlist [4,5,6]]))) [1,2,3,4,5,6];
    testil "f2" (tolist (flatten (fromlist [ fromlist [1,2], fromlist [12,5,6]]))) [1,2,12,5,6];
    testil "f3" (tolist (flatten (fromlist [ fromlist [], fromlist [4,5,6]]))) [4,5,6]
    )

(*
Purpose: a function filter(p,t) that computes a tree that contains all 
and only the elements x : ’a of t for which p x returns true. 
The elements that are kept should be in the same order as in the original tree. 
For example: filter (fn x => x > 2, Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty))) 
== Node (Node (Empty,Empty),Node (Leaf 3,Empty))
*)
fun filter (p : 'a -> bool, t : 'a tree) : 'a tree = flatten(map(fn x => 
(case p x of
false => Empty
|true => Leaf x),t))

fun test_filter() =
    (testil "fi1" (tolist (filter (fn x => (x mod 2) = 0, fromlist [1,2,3,4,5,6]))) [2,4,6];
    testil "fi2" (tolist (filter (fn x => (x mod 2) = 1, fromlist [1,2,3,4,5,6]))) [1,3,5];
    testil "fi3" (tolist (filter (fn x => (x mod 3) = 0, fromlist [1,2,3,4,5,6,7]))) [3,6])
    
(*
Purpose: a function  allpairs(t1, t2) that computes a tree of the pairs (x,y) 
for every element x of t1 and y of t2. The order of the pairs is unspecified.
For example, allpairs (Node(Leaf 1, Leaf 2), Node(Leaf "a", Leaf "b")); 
== Node (Node (Leaf (1,"a"),Leaf (1,"b")),Node (Leaf (2,"a"),Leaf (2,"b")))
*)
fun allpairs (tree1 : 'a tree, tree2 : 'b tree) : ('a * 'b) tree = (*raise Unimplemented*)
flatten (map(fn x => map(fn y => (x,y), tree2), tree1))

fun test_allpairs() =
    (* note: if your output comes out in a different order, that's OK; just change the expected result *)
    (testisl "ap1" (tolist (allpairs (fromlist [1,2,3], fromlist ["a","b"])))
                   [(1,"a"),(1,"b"),(2,"a"),(2,"b"),(3,"a"),(3,"b")])

fun run() =
    (test_pluralize();
     test_mult();
     test_sum();
     test_join();
     test_flatten();
     test_filter();
     test_allpairs())
    
(* ---------------------------------------------------------------------- *)
(* partnr *)

type answers = int * int * int * int

fun same(x : int, y : int) : real = 
    case x = y of
        true => 1.0
      | false => 0.0

fun count_same ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real = 
    same (a1,a1') + same (a2,a2') + same (a3,a3') + same (a4,a4')

(*
Purpose: returns a compatibility score as a real number based on the number of answers which are same. 
2nd question is the most important for me because I can't use and am not used to Hangouts or Facetime.
So, my study partner has to be used to Zoom. I can't study other than morning. 
So, my partner has to be a morning study person too. That's why I weigh them as high as 2 in my scoring.
*)
fun my_scoring ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real = (*raise Unimplemented*)
    2.0 * same (a1,a1') + 2.0 * same (a2,a2') + same (a3,a3') + same (a4,a4')


(*Purpose: given similarity which is a scoring function, cutoff which is a real number,
people which is a tree of person's name and asnwers, this function 
computes the tree of pairs (person1,person2,score) where 
 .each score is the similarity score of person1 and person2
 .the tree is sorted from highest scores to lowest scores
 .only pairs of people whose score is bigger than cutoff are included
 .a pair of people of the form (person1,person2,_) or both the pair (person1,person2,_)
 and the pair (person2,person1,_)

 For example: show_matches (count_same, 0.0, test_data); 
 B and C have compatibility 3.0 
 A and C have compatibility 1.0
*)
fun matches (similarity : answers * answers -> real,
             cutoff : real,
             people : (string * answers) tree) : (string * string * real) tree = 
let val t1 = allpairs(people, people)
    val t2 = filter(fn ((s1,ans1), (s2,ans2)) =>  s1 < s2, t1)
    val t3 = map(fn ((s1,ans1), (s2,ans2)) => (s1, s2, similarity(ans1, ans2)), t2)
    val t4 = filter(fn (s1, s2, score) => score > cutoff, t3)
in sort(fn ((persona1, personb1, score1), (persona2, personb2, score2)) => Real.compare(score2, score1), t4)
end

val test_data : (string * answers) tree = fromlist [ ("A",(1,1,1,1)), ("B",(2,2,2,2)), ("C",(1,2,2,2)), ("D",(1,1,2,1)) ]

fun show_matches (similarity : answers * answers -> real, cutoff : real, people : (string * answers) tree) : unit =
    List.app (fn (n1,n2,score) => print (n1 ^ " and " ^ n2 
                                         ^ " have compatibility " ^ (Real.toString score ^ "\n"))) 
             (tolist (matches (similarity, cutoff, people)))

val survey : (string * (int * int * int * int)) tree =
fromlist
[
("adj", (5, 3, 3, 1) ),
("UltrastableGlass", (3, 1, 2, 1 )),
("dontcha", (3,3,3,1)),
("clb", (4, 1, 3, 2)),
("dkw",(3,1,1,1)),
("ad", (5, 3, 3, 1)),
("letsgojets", (3, 1, 1, 2)),
("lalala", (4,3,1,1)),
("012", (5, 1, 3, 2)),
("swk", (5, 3, 1, 2)),
("cerulean33", (1, 3, 3, 2)),
("tlc",(3,1,3,2)),
("acdc",( 5, 1, 3, 2)),
("awg",(2,1,3,1)),
("unik",(4, 3, 3,1 )),
("abm",( 3,3,1,1)),
("pop",(1,1,3,2)),
("adp",(3,3,1,1)),
("jeanralphio",(3,3,1,1)),
("ericartman",(4,3,3,2)),
("xug",( 2, 1, 3, 4)),
("FoxShine",(4,1,1,2)),
("elz",( 2, 1, 1, 1)),
("kurt",(5,3,1,1)),
("computersciencestudent",(5,1,1,1)),
("tree",( 3,1,2,2)),
("jdl", (5, 1, 3, 1)),
("JAF",(2,1,3,1)),
("whosthatpokemon", (5, 3, 3, 2)),
("kots",( 2, 3, 3, 2)),
("rthornley",(3,3,3,2)),
("nkykia",(3,1,1,2)),
("poh",( 5, 3, 3, 2)),
("keyboardcat",( 5, 3, 3, 2)),
("sar",(1,1,1,1)),
("dt",( 1, 3, 3, 1)),
("chrisnolan78",( 4, 3, 3, 1)),
("huh",(2,3,3,1)),
("abc",( 5, 3, 3, 2)),
("bloop",(5,1,1,2))]

(* try running, for example, show_matches(count_same,3.0,survey); *)

