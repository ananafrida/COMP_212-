(* ---------------------------------------------------------------------- *)
(* shopping cart *)

(* We'll talk about the code for the controller next week! 
   It's okay if you don't understand it yet.  
*)

fun controller (view : 'model -> string,
                respond : 'model * string -> 'model,
                m : 'model) : unit =
    let val () = print (view m ^ "\n")
    in
        case TextIO.inputLine TextIO.stdIn of
            NONE => raise Fail "text input errored"
          | SOME input => controller(view, respond, respond(m,String.substring(input,0,String.size input -1)))
    end

fun intToString(x : int) : string = Int.toString x

(* TASK 1: Add constructors to the type 'model' to model the states of the shopping application *)
    
datatype model = name | items of string*(string list)| price of string * string list * int

    
(* utility functions for writing tests *)    
fun tests (s : string) (n : string) (m : string) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ m ^ "\n    Got: " ^  n ^ "\n")

fun test_model (s : string) (n : model) (m : model) : unit =
    case n=m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED\n")    

(* TASK 2: write a function that displays the model as a string *)

(*
Purpose: a helper function of the function view to convert the stringlist to string
which takes out the elements of stringlist as a string
*)
fun stringlist_to_string(l: string list): string = 
case l of
   [] => ""
   | x::xs => x ^ " " ^ stringlist_to_string(xs)

(*
Purpose: displays model as a string
*)     
fun view (m : model) : string =
 case m of
    name => "Please enter your name:"
    | items(entered_name,_) => "Hi, " ^ entered_name ^ ".\nWhat would you like to buy?\napples $1/pound\nbananas $2/bunch\ncookies $2/box\nOr say 'checkout' to check out."
    | price(entered_name, item_list, total_price) => "Hi, " ^ entered_name ^ "\nYour cart contains " ^ stringlist_to_string(item_list) ^ "\nI will charge you $" ^ intToString(total_price) ^ "\nType 'pay' to pay."

(* tests for view function *)
fun test_view() = 
   (tests "f1" (view(name)) "Please enter your name:";
    tests "f2" (view(items("Anan",[]))) "Hi, Anan.\nWhat would you like to buy?\napples $1/pound\nbananas $2/bunch\ncookies $2/box\nOr say 'checkout' to check out.";
    tests "f3" (view(price("Anan",["apples","cookies","apples"],4))) "Hi, Anan\nYour cart contains apples cookies apples \nI will charge you $4\nType 'pay' to pay."
   )

(*
Purpose: a helper function of respond that takes a string list and 
calculates the total price of the string elements(items) using the 
assigned user input price of each item
*)
fun price_calc(l: string list): int =
 case l of
 [] => 0
 | x::xs => (case x = "apples" of
            true => 1 + price_calc(xs)
            | false => 2 + price_calc(xs)
  )
    
(* TASK 3: write a function that updates the model based on user input *)

(*
Purpose: updates the model based on the user input
*) 
fun respond(m : model, text : string) : model = 
 case m of
    name => items(text,[]) 
    | items(entered_name, l) => 
           (case text = "checkout" of 
                 false => items(entered_name, text::l)
                 | true => price(entered_name, l, price_calc(l))
           )
    | price(entered_name, item_list, total_price) => name

(* tests for respond function *)
fun test_respond() =
    (
    test_model "f1" (respond(name,"checkout")) (items ("checkout", []));
    test_model "f2" (respond(items("Anan",["apples","cookies","apples"]),"checkout")) (price ("Anan",["apples","cookies","apples"],4)); 
    test_model "f3" (respond(price("Anan",["apples","cookies","apples"], 4),"checkout")) name;
    
    test_model "f4" (respond(name,"apples")) (items ("apples", []));
    test_model "f5" (respond(items("Anan",["apples","cookies","apples"]),"apples")) (items ("Anan",["apples","apples","cookies","apples"])); 
    test_model "f6" (respond(price("Anan",["apples"], 1),"apples")) name
    )


(* TASK 4: choose an initial state, fill it, and uncomment this, 
           then run() will run the application 
    
fun run() = controller(view,respond, name)
*)
(*
Purpose: a function that runs the whole application using the function view, respond, and name
*)
fun run() = controller(view,respond,name)
