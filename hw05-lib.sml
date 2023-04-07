(* datatypes *)
datatype tree = Empty | Node of tree * int * tree

(* functions on trees *)
fun depth (t : tree) : int =
    case t
     of Empty => 0
      | Node(l,_,r) => 1 + Int.max(depth l, depth r)

fun size (t : tree) : int =
    case t
     of Empty => 0
      | Node(l,_,r) => (size l) + (size r) + 1

fun tolist (t : tree) : int list =
    case t
     of Empty => []
      | Node(l,x,r) => (tolist l) @ [x] @ (tolist r)

fun isbalanced (t : tree) : bool =
    case t
     of Empty => true
      | Node(l,_,r) =>
        let
          val dl = depth l
          val dr = depth r
        in
          Int.abs(dl - dr) <= 1 andalso isbalanced l andalso isbalanced r
        end

fun inteq(x1:int,x2:int) : bool =
    case Int.compare(x1,x2) of EQUAL => true | _ => false

fun treeeq(t1:tree, t2:tree) : bool =
    case (t1,t2) of
        (Empty,Empty) => true
      | (Node(l1,x1,r1),Node(l2,x2,r2)) =>
            treeeq(l1,l2) andalso inteq(x1,x2) andalso treeeq(r1,r2)
      | _ => false

local
  (* true iff every y in t is less or equal to  x *)
  fun lteq_all(x,t) =
      case t
       of Empty => true
        | Node(l,y,r) => x <= y andalso lteq_all(x,l) andalso lteq_all (x,r)

  (* true iff every y in t is greater than x *)
  fun grt_all(x,t) =
      case t
       of Empty => true
        | Node(l,y,r) => x > y andalso grt_all(x,l) andalso grt_all (x,r)
in
  fun issorted (t : tree) : bool =
      case t
       of Empty => true
        | Node(l,x,r) => lteq_all(x,r) andalso
                         grt_all(x,l) andalso
                         issorted(l) andalso
                         issorted(r)
end

local 
    (*
    If l is non-empty, then there exist l1,x,l2 such that
    split l == (l1,x,l2) and
    l == l1 @ x::l2 and
    length(l1) and length(l2) differ by no more than 1
    *)
    fun split (l : int list) : (int list * int * int list) =
        case l of
            [] => raise Fail "split should never be called on an empty list"
          | _ => let
                     val midlen = (length l) div 2
                     val front = (List.take (l,midlen))
                         
                     (* because we round down, if the list is non-empty, this
                        * has at least one thing in it
                        *)
                     val x :: back = (List.drop (l,midlen))
                 in
                     (front, x, back)
                 end
in
    (* Purpose: transforms an int list into a balanced tree *)
    fun fromlist (l : int list) : tree =
        case l of
            [] => Empty
          | _ =>
                let val (l1, x,  l2) = split l
                in
                    Node (fromlist l1, x , fromlist l2)
                end
            
    (* val Empty = fromlist nil *)
    (* val Node (Empty , 3 , Empty) = fromlist [3] *)
    (* val Node(Node(Empty,5,Empty),8,Node(Empty,2,Empty)) = fromlist [5,8,2] *)
end



fun itToString(l : tree) : string =
    case l of
        Empty => "Empty"
      | Node(l,x,r) => "Node(" ^ itToString l ^ "," ^ Int.toString x ^ "," ^ itToString(r) ^ ")"
         
fun testt (s : string) (n : tree) (m : tree) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ itToString m ^ "\n    Got: " ^ itToString n ^ "\n")

fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n ^ "\n")

fun ttts ((t1,t2) : tree * tree) : string =
    "\n      Tree1 : " ^ itToString t1 ^
    "\n      Tree2 : " ^ itToString t2 ^ "\n"
            
fun testtt (s : string) (n : tree * tree) (m : tree * tree) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ttts m ^ "\n    Got: " ^ ttts n ^ "\n")

fun leaf (x : int) : tree = Node(Empty, x, Empty)
    
fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")

(* test a function that returns an int *)
fun testi (s : string) (n : int) (m : int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString m ^ "\n    Got: " ^ Int.toString n ^ "\n")

fun iilToString(l : (int * int) list) : string =
    case l of
        [] => "[]"
      | (n,s) :: xs => "(" ^ Int.toString n ^ "," ^ Int.toString s ^ ")" ^ "::" ^ iilToString(xs)

fun testiil (s : string) (n : (int * int) list) (m : (int * int) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ iilToString m ^ "\n    Got: " ^ iilToString n ^ "\n")
