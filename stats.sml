
signature STATISTICS =
sig

    type documents = (string Seq.seq) Seq.seq
    (* given a collection of documents, compute a dictionary 
       mapping each word to the number of times it occured *)
    val frequencies : documents -> (string, int) Dict.dict
     
    (* given a collection of documents, compute the total number
       of distinct words in the documents *)
    val num_distinct_words : documents -> int

    (* given a collection of documents, compute a sequence (without duplicates)
       of all of the words in the documents *)
    val distinct_words : documents -> string Seq.seq

    (* given a collection of documents, compute the total number of words 
       (counting duplicates more than once) in the documents *)
    val num_words : documents -> int

end


structure Stats :> STATISTICS = 
struct

type documents = (string Seq.seq) Seq.seq

fun frequencies (doc) = 
                 let
                   val l = Seq.flatten(doc)
                 in
                   Seq.reduce(fn (d1,d2) => TreeDict.merge(String.compare, fn (v1,v2) => v1+v2, d1, d2), 
                   TreeDict.empty, Seq.map(fn k => TreeDict.insert(String.compare, TreeDict.empty, (k,1)), l))
                 end

fun num_distinct_words (doc) = Dict.size(frequencies(doc))
fun distinct_words (doc) = Seq.map(fn (k,v)=> k, Dict.toSeq(frequencies(doc)))
fun num_words (doc) = Seq.reduce(fn (v1,v2) => v1+v2, 0, Seq.map(fn (k,v)=> v, Dict.toSeq(frequencies(doc))))

end



structure TestStats =
struct

    fun split (s : string) : string Seq.seq = Seq.fromlist (String.tokens Char.isSpace s)

    val example : Stats.documents = Seq.fromlist [ split "this is document one", split "this is document two" ]

    fun dictToList (d : (string, int) Dict.dict) : (string * int) list = Seq.tolist (Dict.toSeq d)

    (* uncomment to test *)

    open Testing
        
    fun test() =
        (testi "ndw" (Stats.num_distinct_words example) 5;
         testi "nw" (Stats.num_words example) 8;
         testsl "dw" (Seq.tolist (Stats.distinct_words example)) ["document","is","one","this","two"];
         testsil "fr" (dictToList (Stats.frequencies example)) [("document",2),("is",2),("one",1),("this",2),("two",1)]) 

end 