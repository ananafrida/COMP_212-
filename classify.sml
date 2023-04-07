
structure NaiveBayes :> NAIVE_BAYES_CLASSIFIER =
struct

    type category = string

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq
        
    type statistics = 
          (category,int) Dict.dict           (* maps each category to number of documents with that category *)
        * (category,int) Dict.dict           (* maps each category to number of words in documents with that category *)
        * (category * string, int) Dict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq                   (* list of categories (no duplicates) *)
        * int                                (* total number of documents *)
        * int                                (* total number of different words *)

    (* TASK *)
    fun gather (train : labeled_document MR.mapreducable) : statistics =
      let
        val fg = (* category, number of documents *)
              ExtractCombine.extractcombine(
                String.compare,
                fn (cat,_) => Seq.singleton((cat,1)),
                Int.+,
                train
              )
        val gh = (* category, total number of words *)
              ExtractCombine.extractcombine(
                String.compare,
                fn (cat,doc) => Seq.singleton(cat,Seq.length(doc)),
                Int.+,
                train
              )
        val hj = (* (category * word), frequency *)
              ExtractCombine.extractcombine(
                fn ((c1,w1),(c2,w2)) => String.compare(c1 ^ w1,c2 ^ w2),
                fn (cat,doc) => Seq.map(fn word => ((cat,word),1),doc),
                Int.+,
                train
              )
        val w =
              ExtractCombine.extractcombine(
                String.compare,
                fn (cat,doc) => Seq.map(fn word => (word,1),doc),
                Int.+,
                train
              )
        val all_categories = Seq.map(fn (cat,_) => cat,Dict.toSeq(fg))
        val total_num_docs = MR.mapreduce(fn (cat,_) => 1,0,fn (d1,d2) => d1+d2,train)
        val total_num_words = Dict.size(w)
      in
        (fg, gh, hj, all_categories, total_num_docs, total_num_words)
      end
      
        
    (* TASK *)
    fun possible_classifications 
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories, 
          total_num_docs,
          total_num_words) : statistics,
         test_doc : document) : (category * real) Seq.seq =
      Seq.map(
        fn (cat,num_docs) =>
          (
            cat,
            Math.ln(Real.fromInt(num_docs)/Real.fromInt(total_num_docs)) +
            Seq.reduce(
              Real.+,
              0.0,
              Seq.map(
                (fn w =>
                  case Dict.lookup(fn ((c1,w1),(c2,w2)) => String.compare(c1 ^ w1,c2 ^ w2),freqs,(cat,w)) of
                    NONE => Math.ln(1.0/Real.fromInt(total_num_words))
                  | SOME freq => Math.ln(Real.fromInt(freq)/Real.fromInt(Dict.lookup'(String.compare,num_words_by_cat,cat)))
                ),
                test_doc
              )
            )
          ),
        Dict.toSeq(num_docs_by_cat)
      )
      

    (* TASK *)
    fun classify (stats : statistics, test_doc : document) : (category * real) =
      Seq.reduce(
        fn ((c1,p1),(c2,p2)) =>
          (
            case p1<=p2 of
              true => (c2,p2)
            | _ => (c1,p1)
          ),
        ("",Real.negInf),
        possible_classifications(stats,test_doc)
      )

    (* TASK *)
    fun train_classifier (train : labeled_document MR.mapreducable) : document -> (category * real) =
        let
          val (a,b,c,d,e,f) = gather(train)
        in
          fn doc => classify((a,b,c,d,e,f),doc)
        end
        
end


(*

SMALL TRAINING DATA
  1. Big test:
    ==> (52220,78899)
    66.2%

  2. Medium test:
    ==> (534,808)
    66.1%

  3. Small test:
    ==> (5,8)
    62.5%

MEDIUM TRAINING DATA
  1. Big test:
    ==> (68145,78899)
    86.4%

  2. Medium test:
    ==> (680,808)
    84.2%

  3. Small test:
    ==> (5,8)
    62.5%

BIG TRAINING DATA
  1. Big test:
    ==> (70122,78899)
    88.9%

  2. Medium test:
    ==> (704,808)
    87.1%

  3. Small test:
    ==> (3,8)
    37.5%

*)