type constants = 
| INT of int 
| STRING of string 
| ERROR 
| UNIT
| BOOL of bool 
| NAME of string 
	
type commands =
| Push of constants
| Add
| Sub
| Mul
| Div
| Rem
| Neg
| Swap
| Pop
| Cat
| And
| Or
| Not
| LessThan
| Equal
| If
| Bind
| Let
| End
| FunBind_FunEnd
| Call
| ToSTRING
| ToPRINT
| Quit

let interpreter ( (input : string), (output : string )) : unit =     (* Reads the input file *)

	let ic = open_in input in

	let oc = open_out output in

	let rec loop_read acc =
		try
			let l = String.trim(input_line ic) in loop_read (l::acc)
		with
			| End_of_file -> List.rev acc in

	let file_write v1 = Printf.fprintf oc "%s\n" v1 in

	let file_write_1 v2 = Printf.fprintf oc "%s" v2 in

	let ls_str = loop_read [] in

	let tostring constants =
		match constants with
		| ERROR -> ":error:"
		| UNIT -> ":unit:"
		| INT a -> (string_of_int a)
		| STRING s -> s (* "" ^ (String.sub s 1 (String.length s - 2)) ^ "" *)
		| BOOL b -> ":" ^ string_of_bool b ^ ":"
		| NAME n -> "" ^ n ^"" in

	let checkInt str  =
		try 
			ignore (int_of_string str); true
		with 
			| _ -> false in

	let charList = ['q';'w';'e';'r';'t';'y';'u';'i';'o';'p';'a';'s';'d';'f';'g';'h';'j';'k';'l';'z';'x';'c';'v';'b';'n';'m';
					'Q';'W';'E';'R';'T';'Y';'U';'I';'O';'P';'A';'S';'D';'F';'G';'H';'J';'K';'L';'Z';'X';'C';'V';'B';'N';'M'] in

      let rec foo str charList =
         match charList with
            [] -> false
         | a::b -> if (str.[0]=a) then true 
                   else foo str b in

	let compare st =
		match String.split_on_char ' ' st with
		| ["add"] -> Add
		| ["sub"] -> Sub
		| ["mul"] -> Mul
		| ["div"] -> Div
		| ["pop"] -> Pop
		| ["rem"] -> Rem
		| ["neg"] -> Neg
		| ["swap"] -> Swap
		| ["toString"] -> ToSTRING
		| ["println"] -> ToPRINT
		| ["cat"] -> Cat
		| ["and"] -> And
		| ["or"] -> Or
		| ["not"] -> Not
		| ["equal"] -> Equal
		| ["lessThan"] -> LessThan
		| ["bind"] -> Bind
		| ["if"] -> If
		| ["let"] -> Let
		| ["end"] -> End
		| ["quit"] -> Quit
		| hd::mid::tl ->
				begin
				match (hd,mid) with
				| ("push",":true:") -> Push (BOOL (true))
				| ("push",":false:") -> Push (BOOL (false))
				| ("push",":error:") -> Push ERROR
				| ("push",":unit:") -> Push UNIT
				| ("push",s) -> if (checkInt s) then Push (INT(int_of_string s))
								else if (s.[0]='\"') then Push (STRING (String.sub st 6 (String.length st - 7)))
								else if ((s.[0]='_') || foo s charList) then Push (NAME (s))			 
								else Push ERROR						   
				| (_,_) -> Push ERROR
				end	
		| _ -> Push ERROR in

let rec find subBindingList element = 
  match subBindingList with
   | [] -> None
   | (hd,v)::tl -> if (hd=element) then Some(v) else find tl element in

let rec check bindingList element = 
    match bindingList with
    | [] -> None
    | hd::tl -> (match (find hd element) with
                | Some(x) -> Some(x)
                | None -> check tl element) in

let add stack bindingList =
	match (stack) with
	| (INT(x)::INT(y)::tl) -> (INT(x+y)::tl)
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some INT(s),Some INT(t))-> (INT(s+t)::tl) 
                  						| _ -> (ERROR::NAME(x)::NAME(y)::tl) )
	| (INT(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some INT(s))-> (INT(x+s)::tl) 
                  						| _ -> (ERROR::INT(x)::NAME(y)::tl) )
	| (NAME(x)::INT(y)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> (INT(s+y)::tl) 
                  						| _ -> (ERROR::NAME(x)::INT(y)::tl) )
	| _ -> ERROR::stack in

let sub stack bindingList =
	match (stack) with
	| (INT(x)::INT(y)::tl) -> (INT(y-x)::tl) 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some INT(s),Some INT(t))-> (INT(t-s)::tl) 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (INT(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some INT(s))-> (INT(s-x)::tl) 
                  						| _-> (ERROR::INT(x)::NAME(y)::tl) )
	| (NAME(x)::INT(y)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> (INT(y-s)::tl) 
                  						| _-> (ERROR::NAME(x)::INT(y)::tl) )
	| _ -> ERROR::stack in

let mul stack bindingList =
	match (stack) with
	| (INT(x)::INT(y)::tl) -> (INT(x*y)::tl) 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some INT(s),Some INT(t))-> (INT(s*t)::tl) 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (INT(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some INT(s))-> (INT(x*s)::tl) 
                  						| _-> (ERROR::INT(x)::NAME(y)::tl) )
	| (NAME(x)::INT(y)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> (INT(s*y)::tl) 
                  						| _-> (ERROR::NAME(x)::INT(y)::tl) )
	| _ -> ERROR::stack  in

let div stack bindingList =
	match (stack) with
	| (INT(0)::INT(y)::tl) -> ERROR::INT(0)::INT(y)::tl 
	| (INT(x)::INT(y)::tl) -> (INT(y/x)::tl) 
	| (INT(0)::NAME(y)::tl) -> ERROR::INT(0)::NAME(y)::tl 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some INT(s),Some INT(t))-> if (s!=0) then (INT(t/s)::tl) else (ERROR::NAME(x)::NAME(y)::tl)
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl))
	| (INT(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some INT(s))-> (INT(s/x)::tl) 
                  						| _-> (ERROR::INT(x)::NAME(y)::tl) )
	| (NAME(x)::INT(y)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> if (s!=0) then (INT(y/s)::tl) else (ERROR::NAME(x)::INT(y)::tl) 
                  						| _-> (ERROR::NAME(x)::INT(y)::tl))
	| _ -> ERROR::stack  in

let pop stack =
	match stack with
	| hd::tl -> tl 
	| _ -> ERROR::stack  in

let swap stack =
	match stack with
	| top_1::top_2::tl -> (top_2::top_1::tl) 
	| _ -> ERROR::stack  in

let rem stack bindingList =
	match (stack) with
	| (INT(0)::INT(y)::tl) -> ERROR::INT(0)::INT(y)::tl 
	| (INT(x)::INT(y)::tl) -> (INT(y mod x)::tl) 
	| (INT(0)::NAME(y)::tl) -> ERROR::INT(0)::NAME(y)::tl 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
                                        | (Some INT(0),Some INT(s)) -> (ERROR::NAME(x)::NAME(y)::tl)
										| (Some INT(s),Some INT(t)) ->  INT(t mod s)::tl 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (INT(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some INT(s))-> (INT(s mod x)::tl) 
                  						| _-> (ERROR::INT(x)::NAME(y)::tl) )
	| (NAME(x)::INT(y)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> if (s!=0) then (INT(y mod s)::tl) else (ERROR::NAME(x)::INT(y)::tl)
                  						| _-> (ERROR::NAME(x)::INT(y)::tl) )
	| _ -> ERROR::stack  in

let neg stack bindingList =
	match (stack) with
	| (INT(x)::tl) -> (INT(-x)::tl) 
	| (NAME(x)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> (INT(-s)::tl) 
                  						| _ -> (ERROR::NAME(x)::tl) )
	| _ -> ERROR::stack  in

let nothing stack = stack in

(* Part 2 stuff START *)

let cat stack bindingList =
	match (stack) with
	| (STRING(x)::STRING(y)::tl) -> (STRING(y^x)::tl) 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some STRING(s),Some STRING(t))-> (STRING(t^s)::tl) 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (STRING(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some STRING(s))-> (STRING(s^x)::tl) 
                  						| _-> (ERROR::STRING(x)::NAME(y)::tl) )
	| (NAME(x)::STRING(y)::tl) -> (match (check bindingList x) with
										| (Some STRING(s))-> (STRING(y^s)::tl) 
                  						| _-> (ERROR::NAME(x)::STRING(y)::tl) )
	| _ -> ERROR::stack  in

let anD stack bindingList =
	match (stack) with
	| (BOOL(x)::BOOL(y)::tl) -> (BOOL(y&&x)::tl) 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some BOOL(s),Some BOOL(t))-> (BOOL(t&&s)::tl) 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (BOOL(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some BOOL(s))-> (BOOL(s&&x)::tl)
                  						| _-> (ERROR::BOOL(x)::NAME(y)::tl))
	| (NAME(x)::BOOL(y)::tl) -> (match (check bindingList x) with
										| (Some BOOL(s))-> (BOOL(y&&s)::tl) 
                  						| _-> (ERROR::NAME(x)::BOOL(y)::tl) )
	| _ -> ERROR::stack  in

let oR stack bindingList =
	match (stack) with
	| (BOOL(x)::BOOL(y)::tl) -> (BOOL(y||x)::tl) 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some BOOL(s),Some BOOL(t))-> (BOOL(s||t)::tl) 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (BOOL(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some BOOL(s))-> (BOOL(s||x)::tl) 
                  						| _-> (ERROR::BOOL(x)::NAME(y)::tl) )
	| (NAME(x)::BOOL(y)::tl) -> (match (check bindingList x) with
										| (Some BOOL(s))-> (BOOL(y||s)::tl) 
                  						| _-> (ERROR::NAME(x)::BOOL(y)::tl) )
	| _ -> ERROR::stack  in

let noT stack bindingList =
	match (stack) with
	| (BOOL(x)::tl) -> (BOOL(not (x))::tl) 
	| (NAME(x)::tl) -> (match (check bindingList x) with
										| (Some BOOL(s))-> (BOOL(not s)::tl) 
                  						| _-> (ERROR::NAME(x)::tl) )
	| _ -> ERROR::stack  in

let equal stack bindingList =
	match (stack) with
	| (INT(x)::INT(y)::tl) -> (BOOL(x=y)::tl) 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some INT(s),Some INT(t))-> (BOOL(s=t)::tl) 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (INT(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some INT(s))-> (BOOL(x=s)::tl) 
                  						| _-> (ERROR::INT(x)::NAME(y)::tl) )
	| (NAME(x)::INT(y)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> (BOOL(s=y)::tl) 
                  						| _-> (ERROR::NAME(x)::INT(y)::tl) )
	| _ -> ERROR::stack  in

let lessthan stack bindingList =
	match (stack) with
	| (INT(x)::INT(y)::tl) -> (BOOL(x>y)::tl) 
	| (NAME(x)::NAME(y)::tl) -> (match (check bindingList x , check bindingList y) with
										| (Some INT(s),Some INT(t))-> (BOOL(s>t)::tl) 
                  						| _-> (ERROR::NAME(x)::NAME(y)::tl) )
	| (INT(x)::NAME(y)::tl) -> (match (check bindingList y) with
										| (Some INT(s))-> (BOOL(x>s)::tl) 
                  						| _-> (ERROR::INT(x)::NAME(y)::tl))
	| (NAME(x)::INT(y)::tl) -> (match (check bindingList x) with
										| (Some INT(s))-> (BOOL(s>y)::tl) 
                  						| _-> (ERROR::NAME(x)::INT(y)::tl) )
	| _ -> ERROR::stack  in

let iF stack bindingList =
	match (stack) with
	| ((x)::(y)::BOOL(z)::tl) -> (match z with
										| true -> x::tl 
										| false -> y::tl )
	| ((x)::(y)::NAME(z)::tl) -> (match (check bindingList z) with
										| (Some BOOL(s))-> if s=true then x::tl else y::tl
										| _-> (ERROR::(x)::(y)::NAME(z)::tl) )
	| _ -> ERROR::stack  in

let binD stack bindingList = 
	match (stack,bindingList) with
	| (ERROR::NAME(y)::tl, listhd::listtl) -> (ERROR::ERROR::NAME(y)::tl, listhd::listtl)
	| (BOOL(x)::NAME(y)::tl, listhd::listtl) -> ((UNIT::tl), (((y,BOOL(x))::listhd)::listtl))
	| (STRING(x)::NAME(y)::tl, listhd::listtl) -> ((UNIT::tl), (((y,STRING(x))::listhd)::listtl))
	| (UNIT::NAME(y)::tl, listhd::listtl) -> ((UNIT::tl), (((y,UNIT)::listhd)::listtl))
	| (INT(x)::NAME(y)::tl, listhd::listtl) -> ((UNIT::tl), (((y,INT(x))::listhd)::listtl))
	| (NAME(x)::NAME(y)::tl, listhd::listtl) -> (match (check bindingList x) with
										| (Some s) -> ((UNIT::tl), (((y,s)::listhd)::listtl))
										| _ -> (ERROR::NAME(x)::NAME(y)::tl, listhd::listtl))
	| (_,_) -> (ERROR::stack, bindingList) in


let lFN com stack bindingList = (com, ([]::stack), ([]::bindingList)) in 

let eFN com stack bindingList =
	match (com, stack, bindingList) with
	| (com, (s::top1)::top2::tl1, hd2::tl2) -> (com, (s::top2)::tl1 ,tl2)
    | (com, hd1::tl1, hd2::tl2) -> (com, tl1, tl2)
    | _ -> (com, stack, bindingList) in

(* Part 2 stuff END*)


let rec run(commandList, (stacks: constants list list), (bindingList: (string*constants) list list)) =
	match (commandList,stacks,bindingList) with
    | (com, stk::stack, bindings) -> 
        begin
        match com with
    	| (Push INT(x)::tl) -> run(tl, (INT(x)::stk)::stack, bindingList)  
	    | (Push STRING(x)::tl) -> run(tl ,(STRING(x)::stk)::stack, bindingList)
    	| (Push ERROR::tl) -> run(tl, (ERROR::stk)::stack, bindingList)
    	| (Push UNIT::tl) -> run(tl, (UNIT::stk)::stack ,bindingList)
    	| (Push BOOL(x)::tl) -> run(tl, (BOOL(x)::stk)::stack, bindingList)
    	| (Push NAME(x)::tl) -> run(tl, (NAME(x)::stk)::stack, bindingList)
    	| (Add::tl) -> run(tl, (add stk bindingList)::stack, bindingList)
    	| (Sub::tl) -> run(tl, (sub stk bindingList)::stack, bindingList)
    	| (Pop::tl) -> run(tl, (pop stk)::stack, bindingList)
    	| (Swap::tl) -> run(tl, (swap stk)::stack, bindingList)
    	| (Mul::tl) -> run(tl, (mul stk bindingList)::stack, bindingList)
    	| (Div::tl) -> run(tl, (div stk bindingList)::stack, bindingList)
    	| (Rem::tl) -> run(tl, (rem stk bindingList)::stack, bindingList)
    	| (Neg::tl) -> run(tl, (neg stk bindingList)::stack, bindingList)
    	| (And::tl) -> run(tl, (anD stk bindingList)::stack, bindingList)
    	| (Or::tl) -> run(tl, (oR stk bindingList)::stack, bindingList)
    	| (Not::tl) -> run(tl, (noT stk bindingList)::stack, bindingList)
    	| (Equal::tl) -> run(tl, (equal stk bindingList)::stack, bindingList)
    	| (If::tl) -> run(tl, (iF stk bindingList)::stack, bindingList)
    	| (LessThan::tl) -> run(tl, (lessthan stk bindingList)::stack, bindingList)
    	| (Cat::tl) -> run(tl, (cat stk bindingList)::stack, bindingList)
    	| (Bind::tl) -> (match (binD stk bindingList) with 
                    | (s,b) -> run(tl, s::stack, b))
    	| (Let::tl) -> run((lFN tl stacks bindingList))
    	| (End::tl) -> run((eFN tl stacks bindingList))
    	| (ToSTRING::tl) -> stk (*run(tl, (nothing stk)::stack, bindingList)*)
    	| (ToPRINT::tl) -> run(tl, (nothing stk)::stack, bindingList)
	    | (Quit::[]) -> stk
	    | [] -> ERROR::stk
     	| _ -> ERROR::stk
        end
    | _ -> [] in

let rec printStack stack =
	match stack with
	| [] -> ()
	| last::[] -> (file_write_1 (tostring last)); printStack []
	| hd::tl -> (file_write (tostring hd)); printStack tl in
	
let rec iterator (listSTR : string list ) : commands list =
	match listSTR with
	| [] -> []
	| hd::tl -> (compare hd)::(iterator tl) in
	
let commandList = iterator ls_str in 

let finalList = run(commandList, [[]], [[]]) in printStack finalList;;

(* interpreter (("input1.txt"), ("output.txt")) *)  (* Used to run the program *)