
structure TreeDict :> DICT =
struct

  datatype ('k, 'v) tree =
      Empty
    | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type ('k,'v) dict = ('k, 'v) tree 

  val empty = Empty

  fun size t =
        case t of
            Empty => 0
          | Node(l,_,r) => 1 + size l + size r
      
  fun insert (cmp, d, (k, v)) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => Node (L, (k, v), R)
      | LESS => Node (insert (cmp, L, (k, v)), (k', v'), R)
      | GREATER => Node (L, (k', v'), insert (cmp, R, (k, v)))

  fun lookup (cmp, d, k) =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => SOME v'
      | LESS => lookup (cmp, L, k)
      | GREATER => lookup (cmp, R, k)

(* computes the sequence of all (key, value) pairs in the dictionary.
   ordered from smallest key to largest key
*)
fun toSeq (d) =
    case d of
    Empty => Seq.empty()
    | Node(L, (k', v'), R) => Seq.append(toSeq(L), Seq.cons((k', v'), toSeq(R)))

(*
helper of merge
*)
fun splitAt(d, k, cmp) =
    case d of 
    Empty => (Empty, Empty, NONE)
    | Node(L, (k',v'), R) => 
          case cmp (k, k') of
          EQUAL => (L, R, lookup(cmp, d, k))

          | LESS => let val (ll, lr, v) = splitAt(L, k, cmp)
                    in 
                      (ll, Node(lr, (k', v'), R), lookup(cmp, d, k))
                    end

          | GREATER => let val (rl, rr, v) = splitAt(R, k, cmp)
                       in 
                        (Node(L, (k', v'), rl), rr, lookup(cmp, d, k))
                       end

(*merge (cmp, combine, d1, d2) == d where
 - k in d if and only if k is in d1 or k is in d2
 - If k~v in d1 and k is not in d2, then k ~ v in d
 - If k~v in d2 and k is not in d1, then k ~ v in d
 - If k~v1 in d1 and k~v2 in d2, then k ~ combine (v1, v2) in d
 *)
 fun merge (cmp, combine, d1, d2) =
     case d1 of
      Empty => d2
    | Node (L, (k', v'), R) => let val (l, r, v) = splitAt(d2, k', cmp)
                               in
                               case v of
                               NONE => Node(merge(cmp, combine, L, l), (k', v'), merge(cmp, combine, R, r))
                               | SOME v2' => Node(merge(cmp, combine, L, l), (k', combine (v', v2')), merge(cmp, combine, R, r))
                               end
     

end

structure Dict = TreeDict


structure TestDict =
struct

    open Testing
    
    fun test() =
        let
            fun ins(d,(k,v)) = Dict.insert(Int.compare, d, (k,v))
            fun look(d,k) = Dict.lookup(Int.compare, d, k)
            
            val d = Dict.empty
            val d2 = ins (ins (d, (1,1)), (2,2))
            val d3 = ins (ins (ins (d, (1,1)), (1,2)), (2,2))

            val d4 = ins (ins (ins (d, (1,11)), (3,13)), (5,15))
            val d6 = ins (ins (ins (d, (1,21)), (2,22)), (4,24))
                
            val dm = Dict.merge (Int.compare, fn (x,_) => x , d4,d6)
            val dm2 = Dict.merge (Int.compare, fn (_,y) => y, d4,d6)
        in
            (
             testio "l1" (look( dm  , 1)) (SOME 11);
             testio "l2" (look( dm  , 2)) (SOME 22);
             testio "l3" (look( dm  , 3)) (SOME 13);
             testio "l4" (look( dm  , 4)) (SOME 24);
             testio "l5" (look( dm  , 5)) (SOME 15);
             testio "l6" (look( dm2 , 1)) (SOME 21);
             testiil "ts1" (Seq.tolist (Dict.toSeq d3)) [(1,2),(2,2)] 
             )
        end
             

end
