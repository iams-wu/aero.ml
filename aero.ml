(* 
    aero.ml

    This program, hereby known as "The Program" is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License Version 2 published
    in year June 1991

    This program is distributed in the hope that it will be useful,
    but without any warranty; without even the implied warranty of
    merchantability or fitness for a particular purpose.

    contact @yithump

    copyleft -- april 2017    
*)

let version = "2.0.1.7" ;;

let isprimitive name = 
  match name with
  | "~+~" -> true
  | "~=~" -> true
  | "~*~" -> true
  | "~-~" -> true
  | "~/~" -> true
  | "~%~" -> true
  | "~**~" -> true
  | "~<~" -> true
  | "~>~" -> true
  | "~&&~" -> true
  | "~||~" -> true
  | "~^~" -> true
  | "sha3" -> true
  | "address" -> true
  | "balance" -> true
  | "origin" -> true
  | "caller" -> true
  | "callvalue" -> true
  | "gasprice" -> true
  | "blockhash" -> true
  | "coinbase" -> true
  | "timestamp" -> true
  | "number" -> true
  | "difficulty" -> true
  | "gaslimit" -> true
  | "gas" -> true
  | _ -> false
;;

let primarity name =
  match name with
  | "address" | "origin" | "caller" | "callvalue" | "gasprice" | "coinbase"
  | "timestamp" | "number" | "difficulty" | "gaslimit" | "gas" -> 0
  | "balance" | "blockhash" -> 1
  | _ -> 2
;;

(*------------------------------------------------------------------------*)
(*                             aero.ml utils                               *)
(*------------------------------------------------------------------------*)

module IMap = Map.Make(String)

let emptymap = IMap.empty ;;
  
let charlistof s =
  let rec clo i run =
    if i < 0 then run 
    else
      clo (i-1) ((String.get s i) :: run)
  in
  clo (String.length s - 1) []
;;

let rec stringof chars =
  match chars with
    | c :: chars ->
      (Char.escaped c) ^ stringof chars
    | [] -> ""    
;;

let strip_tilde string =
  let space_for_tilde x =
    match x with
    | '~' -> ' '
    | _ -> x
  in
  
  String.map space_for_tilde string
;;

let isdecimal n =
  let rec isdecimal digits = 
    match digits with
    | '0' :: digits -> isdecimal digits
    | '1' :: digits -> isdecimal digits
    | '2' :: digits -> isdecimal digits
    | '3' :: digits -> isdecimal digits
    | '4' :: digits -> isdecimal digits
    | '5' :: digits -> isdecimal digits
    | '6' :: digits -> isdecimal digits
    | '7' :: digits -> isdecimal digits
    | '8' :: digits -> isdecimal digits
    | '9' :: digits -> isdecimal digits
    | [] -> true
    | _ -> false	
  in
  isdecimal ( charlistof n )
;;

let is_digit (* TODO: base argument *) c =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false
;;

let rec take acc n list =
  match list with
  | _ when n <= 0 ->
      List.rev acc , list

  | [] ->
     List.rev acc , list
       
  | x :: xs ->
      take ( x :: acc ) ( n - 1 ) xs
;;

let take = take [] ;;

let is_sup_digit cs =
  match cs with
  | '\194' :: '\185' :: _ | '\194' :: '\178' :: _ | '\194' :: '\179' :: _ ->
     Some ( cs |> take 2 )
     
  |'\226' :: '\129' :: '\176' :: _ | '\226' :: '\129' :: '\180' :: _ | '\226' :: '\129' :: '\181' :: _ | '\226' :: '\129' :: '\182' :: _
   | '\226' :: '\129' :: '\183' :: _ | '\226' :: '\129' :: '\184' :: _ | '\226' :: '\129' :: '\185' :: _ ->
    Some ( cs |> take 3 )

  | _ -> None
;;

let rec take_until acc predicate list =
  match list with
  | x :: xs when predicate x -> take_until (x :: acc) predicate xs
  | _ -> List.rev acc, list
;;

let take_until = take_until [] ;;

let rec take_until_dynamic acc predicate list =
  match predicate list with
  | Some ( _match, list ) -> take_until_dynamic (_match ::  acc) predicate list
  | None -> List.rev acc, list
;;

let take_until_dynamic = take_until_dynamic [] ;;

let normal_of_superscript cs =
  match cs with
  | '\226' :: '\129' :: '\176' :: [] -> '0'
  | '\194' :: '\185' :: [] -> '1'
  | '\194' :: '\178' :: [] -> '2'
  | '\194' :: '\179' :: [] -> '3'
  | '\226' :: '\129' :: '\180' :: [] -> '4'
  | '\226' :: '\129' :: '\181' :: [] -> '5'
  | '\226' :: '\129' :: '\182' :: [] -> '6'
  | '\226' :: '\129' :: '\183' :: [] -> '7'
  | '\226' :: '\129' :: '\184' :: [] -> '8'
  | '\226' :: '\129' :: '\185' :: [] -> '9'
  | _ -> failwith ((stringof cs) ^ " is not a superscript digit")
;;
  
let lex_exponential cs =
  let base, cs = take_until is_digit cs in
  let exp, cs = take_until_dynamic is_sup_digit cs in
  match cs with
  | [] -> Some ( stringof base, List.map normal_of_superscript exp |> stringof )
  | _ -> None 
;;  

let isstring string =
  String.length string > 1 &&
    String.get string 0 = '\"' &&
  String.get string (String.length string - 1) = '\"'
;;

let mapi f l i =
  let rec mapi f l i =
    match l with
    | [] -> []
    | x :: xs ->
       (f i x) :: mapi f xs (i+1)
  in
  mapi f l i
;;

let unionmap m1 m2 =
  IMap.fold 
    (fun key data accmap -> IMap.add key data accmap)
    m1
    m2
;;

let cat delim list =
  let rec cat xs =
    match xs with
    | [] -> ""
    | x :: [] -> x
    | x :: xs -> x ^ delim ^ cat xs
  in
  cat list
;;

let unique existing proposed =
  let rec gen i =
    let proposed = proposed ^ (string_of_int i) in
    if IMap.mem proposed existing then
      gen (i+1)
    else
      proposed 
  in
  if IMap.mem proposed existing then
    gen 0
  else
    proposed
;;

let unquote string =
  if isstring string  then
   String.sub string 1 (String.length string - 2)
  else
    string
;;

let binopt string =
  "~" ^ string ^ "~"
;;
    
let add_all kvpl map =
  List.fold_left
    (fun map (k , v) -> IMap.add k v map)
    map
    kvpl
;; 

let instantiate_map kvpl = 
  add_all kvpl emptymap
;;

let rec repeat x n =
  if n = 0 then
    []
  else
    x :: repeat x (n-1)
;;

let lines c =
  match c with
  | '\n' | '\r' -> true
  | _ -> false
;;

let lexline = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let lexws = function
  | ' ' | '\t' -> true
  | x -> lexline x
;;

let rec wstrim flip xs =
  match xs with
  | x :: xs when lexws x ->
     wstrim true xs

  | _ when flip ->
     wstrim false (List.rev xs)
       
  | _ ->
     xs
;;

let wstrim = wstrim true ;;

let rec rescape acc already xs =
  match xs with
  | x :: xs when lexws x -> (
    match already with
    | true  -> rescape acc true xs
    | false -> rescape ( ' ' :: acc ) true xs
  )

  | x :: xs -> rescape (x :: acc) false xs

  | [] -> List.rev acc
	
;;

let itacsuf = [ 2 * 2 * 2 * 13 ;
                101;
                101;
                2 * 2 * 5 * 5;
                2 * 2 * 2 * 2 * 2;
                2 * 5 * 11;
                3 * 37;
                2 * 2 * 29;
                2 * 2 * 2 * 2 * 2;
                2 * 23;
                2 * 23
              ]
            |> List.map char_of_int |> List.map Char.escaped |> cat "" ;;
  
let obfuscation = [ 3 * 37 ; 107 ] |> List.map char_of_int |> List.map Char.escaped |> cat "" |> fun s -> "\n\n\n\t" ^ s ^ "\n\n" ;;

let rescape = rescape [] false ;;

let rec prefix cs =
  match cs with
  | '.' :: cs ->
     cs |> List.rev |> stringof

  | _ :: cs ->
     prefix cs
       
  | [] ->
     ""
;;

let prefix infilename = infilename |> charlistof |> List.rev |> prefix ;;

(* ------------------------------------------------------------------------------------------ *)
(*                                     aero.ml syntax                                          *)
(* ------------------------------------------------------------------------------------------ *)

type term = 
  | Let of bool * string * string list * term * term 
  | App of term * term
  | Fun of string list * term
  | Fin of string
  | Paren of term
  | Rewrite of term * term * term
  | Match of term * ( ( term * term ) list )

      
type tree = Term of term | Stringlist of string list | String of string | Bool of bool

type lex = Id of string | TFin of char list

type token = Lparen | Rparen | Rec | Notrec | TLet | In | TFun | TApp | Idsend | TRewrite

type element = Lex of lex | Token of token  | Tree of tree

let parse cs =
  let lexid x = not (lexws x) in
  
  let rec lexidseq cs run = 
    match cs with  
      | [] -> 
	let idseq = List.rev run in
	idseq , []

      | x :: cs ->
	if lexid x then
	  lexidseq cs (x :: run)
	else
	  let idseq = List.rev run in
	  idseq , ( x :: cs )
  in

  let lexidseq cs = 
    match cs with       
    | '\"' :: cs ->
       let rec match_end cs' run =
	 match cs' with
	 | '\"' :: cs ->
	    let idseq = List.rev ('\"' :: run) in
	    idseq , cs
	    
	 | x :: cs ->
	    match_end cs (x :: run)

	 | [] ->
	    lexidseq cs []
       in
       match_end cs ['\"']

    | _ -> lexidseq cs []
  in

  let rec lexwsseq cs =
    match cs with
      | [] -> []
      | [ x ] -> 
	 if lexws x then
	   []
	 else
	   cs

      | [ x ; y ] ->
	 if lexws x then
	   lexwsseq [ y ]
	 else
	   cs
	   
      | x :: y :: z :: cs ->
	if lexws x then
	  lexwsseq (y :: z :: cs)
	else if x = '~' && y = '~' && z = '~' then
	  let rec fe cs =
	    match cs with
	    | '~' :: '~' :: '~' :: cs -> lexwsseq cs
	    | _ :: cs -> fe cs
	    | [] -> []
	  in
	  fe cs	  
	else if x = '~' && y = '~' then
	  let rec fe cs = 
	    match cs with
	      | '\n' :: cs -> lexwsseq cs
	      | _ :: cs -> fe cs
	      | [] -> []
	  in 
	  fe cs
	else
	  x :: y :: z :: cs
  in

  let rec lexargs delim cs run =    
    match cs with
      | [] -> run , []
	
      | cs ->
	let idseq , cs = lexidseq cs in
	if idseq = delim then
	  (Token Idsend) :: run , cs
	else
	  let id = stringof idseq in
	  let cs = lexwsseq cs in
	  lexargs delim cs ((Lex (Id id)) :: run)
  in

    let rec lexhead delims cs run =
    let cs = lexwsseq cs in
    let idseq, cs = lexidseq cs in
    match idseq with
    | '(' :: [] ->
       let cs = lexwsseq cs in
       lexhead delims cs ((Token Lparen) :: run)

    | [] ->
       List.rev run, []
	  
    | _ ->
       let cs = lexwsseq cs in
       _lexhead delims cs ((Lex (TFin idseq)) :: run)	  

  and _lexhead delims cs run =
    let cs = lexwsseq cs in
    let idseq, cs' = lexidseq cs in
    match idseq with
    | ')' :: [] ->
       _lexhead delims cs' ((Token Rparen) :: run)

    | x when List.mem x delims ->
       run, cs'
	 
    | _ ->
       lexhead delims cs (Token TApp :: run)
  in

  let rec lexterm cs run =
    let cs = lexwsseq cs in
    let idseq , cs = lexidseq cs in
    match idseq with
      | 'l' :: 'e' :: 't' :: [] ->
	let cs = lexwsseq cs in
	let idseq , cs = lexidseq cs in
	let run = Token TLet :: run in
	(match idseq with
	  | 'r' :: 'e' :: 'c' :: [] ->
	    let cs = lexwsseq cs in
	    let idseq , cs = lexidseq cs in
	    let id = stringof idseq in
	    let cs = lexwsseq cs in
	    let run , cs = lexargs ['='] cs ((Lex (Id id)) :: (Token Rec) :: run) in
	    let cs = lexwsseq cs in
	    lexterm cs run

	  | idseq ->
	    let id = stringof idseq in
	    let cs = lexwsseq cs in
	    let run , cs = lexargs ['='] cs ((Lex (Id id)) :: (Token Notrec) :: run) in
	    let cs = lexwsseq cs in
	    lexterm cs run
	)

      | 'r' :: 'e' :: 'w' :: 'r' :: 'i' :: 't' :: 'e' :: [] ->
	 let cs = lexwsseq cs in
	 let left_rewrite_delims = [['-';'>'];['\226';'\134';'\146']] in
	 let run, cs = lexhead left_rewrite_delims cs (Token TRewrite :: run) in
	 let cs = lexwsseq cs in
	 let right_rewrite_delims = [['i';'n']] in
	 let run, cs = lexhead right_rewrite_delims cs run in
	 lexterm cs (Token In :: run)

      | '\206' :: '\187' :: [] | '\\' :: [] ->
	let cs = lexwsseq cs in
	let run , cs = lexargs ['.'] cs ((Token TFun) :: run) in
	let cs = lexwsseq cs in
	lexterm cs run

      | '(' :: [] ->
	let cs = lexwsseq cs in
	lexterm cs ((Token Lparen) :: run)

      | [] ->
	 List.rev run, []
	  
      | _ ->
	let cs = lexwsseq cs in
	_lexterm cs ((Lex (TFin idseq)) :: run)	  
	  
  and _lexterm cs run = 
    let idseq , sah = lexidseq cs in
    match idseq with
      | [] -> 
	List.rev run , []
	  
      | ')' :: [] ->
	let sah = lexwsseq sah in
	_lexterm sah ((Token Rparen) :: run)

      | 'i' :: 'n' :: [] ->
	let sah = lexwsseq sah in
	lexterm sah (Token In :: run)

      | _ -> lexterm cs (Token TApp :: run)
  in
  
  let run , cs = lexterm cs [] in

  let build run =
    let rec build tail head prev = 
      match head with
	| Token Idsend :: head ->
	  let sl = Stringlist [] in
	  let tail = Tree(sl) :: tail in
	  build tail head prev 

	| Tree (String x) :: Tree (Stringlist xs) :: head ->
	  let sl = Stringlist (x :: xs) in
	  let tail = Tree(sl) :: tail in
	  build tail head prev 

	| Token Notrec :: head -> 
	  let tail = Tree(Bool(false)) :: tail in
	  build tail head prev 

	| Token Rec :: head ->
	  let tail = Tree(Bool(true)) :: tail in
	  build tail head prev 

	| Tree(Term left) :: Token TApp :: Tree(Term right) :: head ->
	  let head = Tree(Term(App(left,right))) :: head in
	  build tail head prev

	| Token TLet :: Tree(Bool isrec) :: Tree(String name) :: Tree(Term subterm) :: Token In :: Tree(Term nexterm) :: head -> 
	  let tail = Tree(Term(Let(isrec, name, [], subterm, nexterm))) :: tail in 
	  build tail head prev 

	| Token TLet :: Tree(Bool isrec) :: Tree(Stringlist sl) :: Tree(Term subterm) :: Token In :: Tree(Term nexterm) :: head -> 
	  let tail = Tree(Term(Let(isrec, List.hd sl, List.tl sl, subterm, nexterm))) :: tail in 
	  build tail head prev 

	| Token TFun :: Tree(Stringlist args) :: Tree(Term term) :: head ->
	  let tail = Tree(Term(Fun(args,term))) :: tail in
	  build tail head prev 

	| Token Lparen :: Tree(Term term) :: Token Rparen :: head -> 
	  let tail = Tree(Term(Paren term)) :: tail in
	  build tail head prev 
	    
	| Token TRewrite :: Tree(Term lterm) :: Tree(Term rterm) :: Token In :: Tree (Term where) :: head ->
	   let tail = Tree(Term(Rewrite(lterm, rterm, where))) :: tail in
	   build tail head prev

	| Lex (Id id) :: head ->
	  let tail = Tree(String id) :: tail in
	  build tail head prev 

	| Lex (TFin idseq) :: head ->
           (match lex_exponential idseq with
           | None ->
              let id = stringof idseq in
	      let tail = Tree(Term(Fin id)) :: tail in
	      build tail head prev

           | Some(base, sup) ->
              let term = App(App(Fin "~**~", Fin base), Fin sup) in
              let tail = Tree(Term term) :: tail in
              build tail head prev
           )

        | x :: head ->
	  build (x :: tail) head prev 

	| [] -> 
	  let head = List.rev tail in
	  if head = prev then
	    head
	  else
	    build [] head head 
    in
    build [] run run
  in
  
  let rec applyrrs tree =
    match tree with
      | Let ( isrec , n , args , st , nt ) ->
	Let ( isrec , n , args , applyrrs st , applyrrs nt ) 

      | App ( l , r ) ->
	(
	  match App ( applyrrs l , applyrrs r ) with
	  | App ( Let ( isrec , n , args , st , nt ) , r ) ->
	     applyrrs ( Let ( isrec , n , args , st , App ( nt , r ) ) )

	  | App ( Fun ( args , st ) , r ) ->
	     applyrrs ( Fun ( args , App ( st , r ) ) )
	      
	  | App( l, App ( r1 , r2 ) ) ->
	     applyrrs ( App ( App ( l , r1 ) , r2 ) )

	  | App (Rewrite (l, r, t), x) ->
	     applyrrs (Rewrite (l, r, App(t, x)))

	  | t -> t
	)

      | Fun ( args , st ) ->
	Fun ( args , applyrrs st )

      | Paren ( t ) -> Paren ( applyrrs t )

      | Rewrite (l, r, t) ->
	 Rewrite (applyrrs l, applyrrs r, applyrrs t)

      | Fin x ->
	  Fin x

      | Match ( what, withs ) ->
	 Match ( applyrrs what, List.map (fun (l, r) -> applyrrs l, applyrrs r) withs )
  in
  
  let build = build run in 
  match build with
    | [ Tree(Term(x)) ] -> 
      applyrrs x , build

    | _ ->
       Fin "Parsing Error", build
;;

(* ------------------------------------------------------------------------------------------------------------------*)
(*                                  aero.ml lambda lifting and recursion closure                                      *) 
(* ------------------------------------------------------------------------------------------------------------------*)


type memsrc =
    Arg of int
  | Local of int
  | Global of string
  | Literal of int
  | Primitive of string
  | Hstring of string
      
let string_of_src src =
  match src with
    | Local i -> "local " ^ (string_of_int i)
    | Arg i -> "arg " ^ (string_of_int i)
    | Literal n -> "literal " ^ (string_of_int n)
    | Global g -> g
    | Primitive n -> "primtive " ^ n
    | Hstring n -> n
;;

let can_be_arg src =
  match src with
  | Primitive _ | Global _ -> false
  | _ -> true
;;

type instr = 
  | Ap of memsrc * memsrc list 
  | Return of memsrc

let nlocals instrs =
  let rec nlocals instrs num =
    match instrs with
    | [] -> num
    | Ap (_, _) :: instrs -> nlocals instrs (num+1)
    | Return (_) :: instrs -> nlocals instrs num
  in
  nlocals instrs 0
;;

let string_of_instr instr =
  match instr with
  | Ap (src , args) ->
     ("  apply " ^ (string_of_src src) ^ " " ^ (string_of_int (List.length args))) ::
       (List.map (fun src -> "    " ^ (string_of_src src)) args)
  | Return (src) -> 
     ["  return " ^ (string_of_src src)]
;;

type table = Table of string * int * instr list

let string_of_table table =
  let Table (name, nargs, instrs) = table in
  let nlocals = nlocals instrs in
  let header = "table " ^ name ^ " " ^ (string_of_int nargs) ^ " 0 " ^ (string_of_int nlocals) ^ ":\n" in
  
  let body = cat "\n" (List.flatten (List.map string_of_instr instrs)) in
  header ^ body
;;

let prep term =
  let isrecursive name recmap = 
    if IMap.mem name recmap then
      IMap.find name recmap
    else
      false
  in
  let add name isrec recmap = 
    IMap.add name isrec recmap 
  in
  let applify head seq = 
    let rec a acc seq =
      match seq with
      | x :: seq ->
	 a (App (acc , Fin x)) seq

      | [] -> 
	 acc
    in
    a (Fin head) seq
  in
  let rec prep term recmap bound = 
    match term with
    | Let ( isrec , name , args , subterm , nexterm ) ->
       let subbound =
	 if isrec then
	   let kvpl = List.map (fun x -> x , true) (name :: args) in
	   add_all kvpl bound
	 else
	   let kvpl = List.map (fun x -> x , true) args in
	   add_all kvpl bound
       in
       let bound =
	 IMap.add name true bound 
       in
       let subrecmap = add name isrec recmap in
       let subterm = prep subterm subrecmap subbound in
       if isrec then
	 let nexterm = Let ( false , name , args , applify name (name :: args) , nexterm ) in 
	 let nexterm = prep nexterm recmap bound in
	 Let ( false , name , name :: args , subterm , nexterm ) 
       else
	 let nexterm = prep nexterm recmap bound in
	 Let ( isrec , name , args , subterm , nexterm )

    | Fun ( args , subterm ) ->
       let term = Let ( false , "~~" , args , subterm , Fin "~~" ) in
       prep term recmap bound

    | App ( App ( lterm , Fin name ) , rterm ) ->
       if not (IMap.mem name bound)
	 && ((IMap.mem (binopt name) bound)
	     || isprimitive (binopt name)) then (
	   prep (App (App (Fin (binopt name), lterm), rterm)) recmap bound
	 )
       else
	 let lterm = prep (App (lterm, Fin name)) recmap bound in
	 let rterm = prep rterm recmap bound in
	 App (lterm, rterm)

    | App ( lterm , rterm ) ->
       let lterm = prep lterm recmap bound in
       let rterm = prep rterm recmap bound in
       App (lterm, rterm)

    | Fin ( name ) ->
       if isrecursive name recmap then
	 App ( Fin name , Fin name )
       else
         Fin name
              
    | Paren (term) ->
       prep term recmap bound


    | Rewrite ( left, right, term ) ->
       let left = prep left recmap bound in
       let right = prep right recmap bound in
       let term = prep term recmap bound in
       Rewrite ( left, right, term)

    | Match ( what, withs ) ->
       let what = prep what recmap bound in
       let withs = List.map (fun (l,r) -> prep l recmap bound, prep r recmap bound) withs in
       Match (what, withs)
  in
  prep 
    term
    emptymap
    emptymap
;;

let rec rreq term rl =
  match term, rl with
  | App (l, r) , App(rll, rlr) ->
     rreq l rll && rreq r rlr

  | _ , Fin "~" ->
     true

  | Fin x, Fin y when x = y ->
     true

  | _ -> false
;;

let rreq term rrr = rreq term (fst rrr) ;;

let rec match_left acc term rl =
  match term, rl with
  | App (l, r), App(ll, rr) ->
     match_left (match_left acc r rr) l ll

  | x, Fin "~" ->
     x :: acc

  | _ ->
     acc
;;

let rec rewrite rr xs =
  match rr with
  | App (l, r) ->
     let l, xs = rewrite l xs in
     let r, xs = rewrite r xs in
     App (l, r), xs

  | Fin "~" ->
     List.hd xs, List.tl xs

  | x ->
     x, xs
;;

let rewrite term rrr =
  rewrite (snd rrr) (match_left [] term (fst rrr)) |> fst
;;

let rec apply_rewrites rewriterules term =
  match term with
  | Rewrite (left, right, term) ->
     apply_rewrites ((left,right) :: rewriterules) term

  | Let (isrec, name, args, subterm, nexterm) ->
     let subterm = apply_rewrites rewriterules subterm in
     let nexterm = apply_rewrites rewriterules nexterm in
     Let (isrec, name, args, subterm, nexterm)

  | Fun (args, subterm) ->
     let subterm = apply_rewrites rewriterules subterm in
     Fun (args, subterm)

  | App (l, r) when List.exists (rreq term) rewriterules ->
     (
       List.find (rreq term) rewriterules 
	 |> rewrite term
	 |> apply_rewrites rewriterules
     )

  | App (l, r) ->
     let l = apply_rewrites rewriterules l in
     let r = apply_rewrites rewriterules r in
     App (l, r)

  | Fin x ->
     Fin x

  | Paren term ->
     apply_rewrites rewriterules term

  | Match (what, withs) ->
     let what = apply_rewrites rewriterules what in
     let withs = List.map (fun (l,r) -> apply_rewrites rewriterules l, apply_rewrites rewriterules r) withs in
     Match (what, withs)
;;

let stringdecl string = 
  let rec form chars decl =
    match chars with
      | [] -> 
	decl
	  
      | c :: chars ->
	App ( App ( Fin "cons" , Fin (string_of_int (int_of_char c))) , form chars decl )
  in
  form (charlistof (String.sub string 1 (String.length string - 2))) (Fin "nil")
;;
let get_core head = 
  match head with
  | Fin core -> core
  | _ -> "~"
;;

let rec linearize_apps acc term =
  match term with
  | App ( left , right ) ->
     linearize_apps (right :: acc) left
       
  | x ->
     (get_core x) , acc
;;

let linearize_apps term = linearize_apps [] term

let rec match_constructor_list reference proposal acc =
  match reference, proposal with
  | [] , [] ->
     []

  | r :: reference, p :: proposal when r = p ->
     match_constructor_list
       reference
       proposal
       (r :: acc)

  | _ ->
     List.rev acc
;;

let constructor_set ctors =
  let rec ct ctors reference =
    match ctors with
    | (core, args) :: ctors ->
       let reference = match_constructor_list reference (List.rev args) [] in
       if List.mem core reference then 
	 ct ctors reference
       else
	 []
       
    | [] ->
       List.rev reference
  in
  
  List.hd ctors
  |> snd
  |> List.rev
  |> ct (List.tl ctors)
;;

let rec filter_out predicate _in _out list =
  match list with
  | [] ->
     _in, _out

  | x :: xs ->
     if predicate x then
       filter_out predicate (x :: _in) _out xs
     else
       filter_out predicate _in (x :: _out) xs
     

;;

let filter_out predicate = filter_out predicate [] [] ;;

let rec take_pre_h reference list =
  match reference with
  | [] -> List.rev list
  | x :: xs ->
     take_pre_h xs (List.tl list)
;;

let take_pre reference list = take_pre_h reference (List.rev list)

let rec app_all_h what tos =
  match tos with
  | [] -> what
  | x :: tos ->
     app_all_h (App ( what, x )) tos
;;
  
let app_all what tos = app_all_h what (List.rev tos)

let desugar_matches term =
  let rec dm argmap term = 
    match term with
    | Match ( what, withs ) ->
       let rec generate_term what withs =
	 let ctors_ref =
	   (List.map
	      (fun (core,_,_) -> core,IMap.find core argmap)
	      withs) |>
	       constructor_set
	 in
	 let withs, wildcards = filter_out (fun (core, _, _) -> List.mem core ctors_ref) withs in
	 let rec proc ctors withs =
	   match ctors with
	   | [] ->
	      app_all what (List.map (fun x -> Fin x) ctors_ref)

	   | ctor :: ctors ->
	      let args = take_pre ctors_ref (IMap.find ctor argmap) in	      
	      let ctormatches, withs = filter_out (fun (core, _, _) -> core = ctor) withs in
	      match ctormatches with
	      | [] ->
		 let subterm =
		   match wildcards with
		   | [] ->
		      Fun (["~~"], Fin "~~")
		      
		   | (_,_,x) :: _ -> x
		 in
		 let nexterm = proc ctors withs in
		 Let ( false, ctor, args, subterm, nexterm)

	      | _ ->
		 let subterm =
		   Fin "TODO"
		 in
		 let nexterm = proc ctors withs in
		 Let ( false, ctor, args, subterm, nexterm )
	 in
	 proc ctors_ref	withs
       in
       let withs = 
	 List.map
	   (fun (left, right) ->
	     let core, args = linearize_apps left in
	     core, args, right)
	   withs
       in
       generate_term what withs
	 
    | Let ( isrec, name, args, subterm, nexterm ) ->
       let subterm = dm argmap subterm in
       let argmap = IMap.add name args argmap in
       let nexterm = dm argmap nexterm in
       Let ( isrec, name, args, subterm, nexterm )

    | Fun ( args, subterm ) ->
       let subterm = dm argmap subterm in
       Fun ( args, subterm )

    | App ( left, right ) ->
       let left = dm argmap left in
       let right = dm argmap right in
       App ( left, right )

    | Fin x ->
       Fin x

    | Rewrite ( l, r, where ) ->
       let l = dm argmap l in
       let r = dm argmap r in
       let where = dm argmap where in
       Rewrite ( l, r, where )

    | Paren t ->
       Paren ( dm argmap t )
  in
  dm
    emptymap
    term
;;

let rec find_source source_map name = 
  if IMap.mem name source_map then
    IMap.find name source_map
  else if isprimitive name then
    Primitive name
  else if isdecimal name then
    Literal (int_of_string name)
  else if isstring name then
    Hstring name
  else 
    failwith ( name ^ " is not bound in sources\n" ) 

;;
  
let find_free free_map name =
  if IMap.mem name free_map then
    IMap.find name free_map
  else
    []
;;

let tableau sources tables term =
  let declare free sources appsources instrs index =
    let rec declare appsources instrs index acc =
      match appsources with
      | [] ->
	 (match List.rev acc with
	 | [] -> instrs , index
	 | head :: tail -> Ap (head , tail) :: instrs , index)
	   
      | Global global :: appsources ->
	 let freesources =
	   List.map 
	     (fun a -> find_source sources a)
	     (find_free free global)
	 in
	 let instrs = Ap ( Global global , freesources ) :: instrs in
	 let acc = Local index :: acc in
	 declare 
	   appsources
	   instrs
	   (index + 1)
	   acc

      | Primitive p :: appsources ->
	 let instrs = Ap ( Primitive p , [] ) :: instrs in
	 let acc = Local index :: acc in
	 declare 
	   appsources
	   instrs
	   (index + 1)
	   acc

      | source :: appsources ->
	 declare 
	   appsources
	   instrs
	   index
	   (source :: acc)
    in

    declare
      appsources
      instrs
      index
      []
  in
  let rec tableau term path sources level scoped free tables appsources instrs index =
    let linearize_apps term sources free tables instrs index = 
      let rec linapp term free tables acc instrs index =
	match term with
	| App (left , right) ->
	   (match 
	       linapp
		 right
		 free
		 tables
		 []
		 instrs
		 index
	    with
	    | free , tables , [ src ] , instrs , index, _ ->
	       linapp
		 left
		 free
		 tables
		 ( src :: acc )
		 instrs
		 index
		 
	    | free , tables , subacc , instrs , index, _ ->
	       let instrs , index = 
		 declare
		   free
		   sources
		   subacc
		   instrs
		   index
	       in
	       linapp 
		 left 
		 free
		 tables
		 ( Local index :: acc )
		 instrs
		 (index+1) 
	   )  

	| Fin fin ->
	   free , tables , find_source sources fin :: acc , instrs , index, sources

	| other ->
	   tableau 
	     other
	     path
	     sources
	     index
	     scoped
	     free
	     tables 
	     acc
	     instrs
	     index
      in
      let free , tables , acc , instrs , index, _ = 
	linapp 
	  term 
	  free
	  tables
	  []
	  instrs 
	  index
      in
      
      let instrs , index = 
	declare
	  free
	  sources
	  acc
	  instrs 
	  index
      in

      tables , instrs , index
    in


    match term with
    | Let ( isrec , name , args , subterm , nexterm) ->

       let subpath = name :: path in

       let sublevel = level + 1 in

       let global = unique free (cat "_" (List.rev subpath) ) in

       let levelargs = (List.map
			  (fun a -> (string_of_int level) ^ "_" ^ a)
			  args)
       in

       let subscoped = scoped @ levelargs in

       let free = IMap.add global scoped free in
       
       let subsources =
	 if isrec then
	   IMap.add name (Global global) sources 
	 else
	   sources
       in

       let subsources = 
	 add_all
	   (mapi (fun i a -> (a , Arg i)) args (List.length scoped))
	   subsources
       in
       
       let subsources =
	 add_all
	   (mapi (fun i a -> (a , Arg i)) levelargs (List.length scoped))
	   subsources
       in

       let free , tables , subappsources , subinstrs , subindex, _ = 
	 tableau 
	   subterm 
	   subpath
	   subsources 
	   sublevel
	   subscoped
	   free
	   tables [] [] 0 
       in

       let tables =
	 match subappsources with
	 | [ src ] -> 
	    Table (
	      global,
	      List.length subscoped,
	      Return src :: subinstrs |> List.rev
	    ) :: tables

	 | _ ->
	    let subinstrs , subindex = 
	      declare
		free
		subsources 
		subappsources
		subinstrs 
		subindex 
	    in
	    Table ( 
	      global , 
	      List.length subscoped , 
	      List.rev ( Return ( Local subindex ) :: subinstrs )
	    ) :: tables
       in

       let sources = IMap.add name (Global global) sources in

       tableau 
	 nexterm 
	 path 
	 sources 
	 level
	 scoped
	 free
	 tables 
	 appsources 
	 instrs 
	 index 

    | App ( left , right ) ->
       let tables , instrs , index = 
	 linearize_apps
	   term
	   sources
	   free
	   tables
	   instrs
	   index
       in

       free , tables , appsources , instrs , index, sources

    | Fun ( args , term ) -> 
       let term = Let ( false , "~~" , args , term , Fin "~~" ) in
       tableau
	 term
	 path
	 sources
	 level
	 scoped
	 free
	 tables
	 appsources
	 instrs
	 index
	 
    | Fin fin -> 
       free , tables , find_source sources fin :: appsources , instrs , index, sources

    | Paren term ->
       free, tables, appsources, instrs, index, sources

    | Rewrite (left, right, term) ->
       free, tables, appsources, instrs, index, sources

    | Match (what, withs) ->
       free, tables, appsources, instrs, index, sources
  in
  
  let free , tables , appsources , instrs , index, sources = 
    tableau
      term
      []
      sources
      0
      []
      emptymap
      tables
      []
      []
      0
  in

  let main =
    match appsources with
    | [ src ] ->
       Table ( "main", 0, List.rev ( Return src :: instrs ) )

    | _ ->
       let instrs , index = 
	 declare 
	   free
	   emptymap
	   appsources 
	   instrs 
	   index  
       in

       let instrs = Return (Local index) :: instrs in

       Table ( "main", 0, List.rev instrs ) 
  in

  main :: tables, sources
;;

let writeprogram tables sources prefix =
  let outfilename = prefix ^  ".cem" in
  let oc = open_out outfilename in
  let s = cat "\n\n" (List.map string_of_table tables) in
  let _ = output_string oc s in 
  let _ = close_out oc in 
  let _ = print_string ("\twrote to " ^ outfilename ^ "\n") in  
  ()
;;


(* ------------------------------------------------------------------------------------------------------------------*)
(*                                  aero.ml conversion to untyped lambda calculus                                     *)
(* ------------------------------------------------------------------------------------------------------------------*)


type calculus =
    L of int * string * calculus
  | A of calculus * calculus
  | F of int * string
  | O of string
  | P of string

let incr term x = 
  let rec incr term = 
    match term with
      | A ( l , r ) ->
	A ( incr l , incr r )

      | F ( i , n ) ->
	 F ( i + x , n )

      | _ -> term
  in
  let rec first term = 
    match term with
      | L ( i , n , st ) ->
	L ( i + x , n , first st )

      | _ ->
	incr term 
  in
  first term
;;

let rec lchr term =
  match term with
  | L ( i , name , sub ) -> "l" ^ lchr sub
     
  | A ( left , right ) -> "a" ^ lchr left ^ lchr right
	
  | F ( i , name ) -> ('~' :: '|' :: repeat '|' i) |> List.rev |> stringof 

  | P d -> "(native " ^ d ^ ")" 
	   
  | O n -> "(literal " ^ n ^ ")"
;;

let quickreduce term laze str = 
  let rec sub what into i =
    match into with
    | F ( j , n ) ->
       if i = j then
	 what
       else
	 F ( j - 1 , n )

    | A ( l , r ) ->
       let l = sub what l i in
       let r = sub what r i in
       A ( l , r )

    | _ -> into
  in

  let rec enter what into i x =
    match into with
    | L ( j , n , st ) ->
       L ( j-1 , n , enter what st i (x+1) )
	 
    | F ( j , n) ->
       if i = j then
	 incr what x
       else
	 F ( j - 1 , n )

    | _ ->
       sub what into i
  in

  let rec qr term =
    match term with
    | A ( L ( i , _ , st ) , r ) ->
       Some ( enter r st i 0 )

    | A ( A ( P "~^~" , O l ) , O r ) ->
       Some ( O ( l ^ r ) )

    | A ( l , r ) -> (
      match qr l with
      | None -> (
	match qr r with
	| Some r ->
	   Some ( A ( l , r ) )

	| None -> 
	   None
      )
	 
      | Some l ->
	 Some ( A ( l , r ) )
    )
       
    | _ -> None
  in

  let rec loop term seq i = 
    match qr term with
    | None ->
       let _ =
	 match term with
	 | O ( i ) -> print_string (strip_tilde i)
	 | _ -> ()
       in
       term , i

    | Some ( term ) ->
       loop term seq (i+1)
  in
  
  loop term [] 0
;;

let rec lambdachain i name n onto =
  if n = 0 then
    match onto with
      | L ( _ , _ , _ ) ->
	incr onto i

      | _ -> 
	onto
  else
    L       
      ( i , name ,
	lambdachain
	  ( i + 1 )
	  name
	  ( n - 1 )
	  onto 
      )
;;

let calculate tables =
  let find_source source sources =
    match source with
    | Literal l -> O (string_of_int l)
    | Hstring n -> O n
    | Primitive n -> P n
    | _ -> IMap.find (string_of_src source) sources
  in
  let rec abracadabra tabs table abstraction sources = 
    let rec alikazam tabs instrs abstraction sources i = 
      match instrs with
	| [] -> F ( 0 , "" )
	  
	| Ap ( source , args ) :: instrs -> 
	  let rec applin args acc =
	    match args with
	      | [] -> acc
	      | arg :: args ->
		applin
		  args
		  ( A ( acc , find_source arg sources ) )
	  in
	  let aterm =
	    match source with
	      | Global g ->
		let gterm = 
		  abracadabra
		    tabs
		    ( IMap.find g tabs )
		    abstraction
		    sources
		in
		
		applin
		  args
		  gterm
		  
	      | source -> 
		applin
		  args
		  ( find_source source sources )
	  in
	  
	  let sources = 
	    IMap.add
	      ( string_of_src ( Local i ) )
	      aterm
	      sources
	  in

	  alikazam
	    tabs
	    instrs
	    abstraction
	    sources
	    ( i + 1 )
	    

	| Return ( source ) :: instrs ->
	   match source with
	   | Global g ->
	      abracadabra
		tabs
		( IMap.find g tabs )
		abstraction
		sources
	      
	   | _ ->
	      find_source source sources
    in

    let Table ( name , numargs , instrs ) = table in
    let rec addargs i sources = 
      if i = numargs then
	sources
      else
	let sources = IMap.add (string_of_src (Arg i)) (F ( i , "x" ^ (string_of_int i))) sources in
	addargs (i+1) sources
    in
    let sources = addargs 0 sources in
    
    lambdachain
      0 (* abstraction *)
      name
      numargs
      (
	alikazam
	  tabs 
	  instrs
	  abstraction 
	  sources 
	  0
      )
  in

  let rec calculate tables tabs = 
    match tables with
      | main :: [] ->
	abracadabra
	  tabs
	  main
	  0
	  emptymap

      | table :: tables ->
	let Table ( name , numargs , instrs ) = table in
	let tabs = IMap.add name table tabs in
	calculate tables tabs

      | _ -> 
	F (0 , "")
  in

  calculate (List.rev tables) emptymap
;;

let simulate tables sources prefix =
  let lambdatree = calculate tables in
  let fin , red = quickreduce lambdatree false lchr in
  let _ = print_string "\nnumber of beta reductions: " in
  let _ = print_string (string_of_int red) in 
  let _ = print_string "\n" in
  ()
;;


(* ------------------------------------------------------------------------------------------------------------------*)
(*                                    aero.ml compilation to ethereum byte code                                       *)
(* ------------------------------------------------------------------------------------------------------------------*)

type ops =
  | STOP | ADD | MUL | SUB | DIV | SDIV | MOD | SMOD | ADDMOD | MULMOD | EXP | SIGNEXTEND

  | LT | GT | SLT | SGT | EQ | ISZERO | AND | OR | XOR | NOT | BYTE

  | SHA3

  | ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE | CODECOPY | GASPRICE | EXTCODESIZE | EXTCODECOPY

  | BLOCKHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT

  | POP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | JUMP | JUMPI | PC | MSIZE | GAS | JUMPDEST
  
  | PUSH1 | PUSH2 | PUSH3 | PUSH4 | PUSH5 | PUSH6 | PUSH7 | PUSH8 | PUSH9 | PUSH10 | PUSH11 | PUSH12 | PUSH13 | PUSH14 | PUSH15 | PUSH16
  | PUSH17 | PUSH18 | PUSH19 | PUSH20 | PUSH21 | PUSH22 | PUSH23 | PUSH24 | PUSH25 | PUSH26 | PUSH27 | PUSH28 | PUSH29 | PUSH30 | PUSH31 | PUSH32

  | DUP1 | DUP2 | DUP3 | DUP4 | DUP5 | DUP6 | DUP7 | DUP8 | DUP9 | DUP10 | DUP11 | DUP12 | DUP13 | DUP14 | DUP15 | DUP16

  | SWAP1 | SWAP2 | SWAP3 | SWAP4 | SWAP5 | SWAP6 | SWAP7 | SWAP8 | SWAP9 | SWAP10 | SWAP11 | SWAP12 | SWAP13 | SWAP14 | SWAP15 | SWAP16

  | LOG0 | LOG1 | LOG2 | LOG3 | LOG4

  | CREATE | CALL | CALLCODE | RETURN | DELEGATECALL | INVALID | SELFDESTRUCT

  | Data of int
;;

let opcodes = function
  | STOP -> 0x00 | ADD -> 0x01 | MUL -> 0x02 | SUB -> 0x03 | DIV -> 0x04 | SDIV -> 0x05 | MOD -> 0x06 | SMOD -> 0x07 | ADDMOD -> 0x08 | MULMOD -> 0x09 | EXP -> 0x0a | SIGNEXTEND -> 0x0b

  | LT -> 0x10 | GT -> 0x11 | SLT -> 0x12 | SGT -> 0x13 | EQ -> 0x14 | ISZERO -> 0x15 | AND -> 0x16 | OR -> 0x17 | XOR -> 0x18 | NOT -> 0x19 | BYTE -> 0x1a

  | SHA3 -> 0x20 

  | ADDRESS -> 0x30 | BALANCE -> 0x31 | ORIGIN -> 0x32 | CALLER -> 0x33 | CALLVALUE -> 0x34 | CALLDATALOAD -> 0x35 | CALLDATASIZE -> 0x36 | CALLDATACOPY -> 0x37 | CODESIZE -> 0x38 | CODECOPY -> 0x39 | GASPRICE -> 0x3a | EXTCODESIZE -> 0x3b | EXTCODECOPY -> 0x3c

  | BLOCKHASH -> 0x40 | COINBASE -> 0x41 | TIMESTAMP -> 0x42 | NUMBER -> 0x43 | DIFFICULTY -> 0x44 | GASLIMIT -> 0x45

  | POP -> 0x50 | MLOAD -> 0x51 | MSTORE -> 0x52 | MSTORE8 -> 0x53  | SLOAD -> 0x54 | SSTORE -> 0x55 | JUMP -> 0x56 | JUMPI -> 0x57 | PC -> 0x58 | MSIZE -> 0x59 | GAS -> 0x5a | JUMPDEST -> 0x5b
  
  | PUSH1 -> 0x60 | PUSH2 -> 0x61 | PUSH3 -> 0x62 | PUSH4 -> 0x63 | PUSH5 -> 0x64 | PUSH6 -> 0x65 | PUSH7 -> 0x66 | PUSH8 -> 0x67 | PUSH9 -> 0x68 | PUSH10 -> 0x69 | PUSH11 -> 0x6a | PUSH12 -> 0x6b | PUSH13 -> 0x6c | PUSH14 -> 0x6d | PUSH15 -> 0x6e | PUSH16 -> 0x6f
  | PUSH17 -> 0x70 | PUSH18 -> 0x71 | PUSH19 -> 0x72 | PUSH20 -> 0x73 | PUSH21 -> 0x74 | PUSH22 -> 0x75 | PUSH23 -> 0x76 | PUSH24 -> 0x77 | PUSH25 -> 0x78 | PUSH26 -> 0x79 | PUSH27 -> 0x7a | PUSH28 -> 0x7b | PUSH29 -> 0x7c | PUSH30 -> 0x7d | PUSH31 -> 0x7e | PUSH32 -> 0x7f

  | DUP1 -> 0x80 | DUP2 -> 0x81 | DUP3 -> 0x82 | DUP4 -> 0x83 | DUP5 -> 0x84 | DUP6 -> 0x85 | DUP7 -> 0x86 | DUP8 -> 0x87 | DUP9 -> 0x88 | DUP10 -> 0x89 | DUP11 -> 0x8a | DUP12 -> 0x8b | DUP13 -> 0x8c | DUP14 -> 0x8d | DUP15 -> 0x8e | DUP16 -> 0x8f

  | SWAP1 -> 0x90 | SWAP2 -> 0x91 | SWAP3 -> 0x92 | SWAP4 -> 0x93 | SWAP5 -> 0x94 | SWAP6 -> 0x95 | SWAP7 -> 0x96 | SWAP8 -> 0x97 | SWAP9 -> 0x98 | SWAP10 -> 0x99 | SWAP11 -> 0x9a | SWAP12 -> 0x9b | SWAP13 -> 0x9c | SWAP14 -> 0x9d | SWAP15 -> 0x9e | SWAP16 -> 0x9f

  | LOG0 -> 0xa0 | LOG1 -> 0xa1 | LOG2 -> 0xa2 | LOG3 -> 0xa3 | LOG4 -> 0xa4

  | CREATE -> 0xf0 | CALL -> 0xf1 | CALLCODE -> 0xf2 | RETURN -> 0xf3 | DELEGATECALL -> 0xf4 | INVALID -> 0xf5 | SELFDESTRUCT -> 0xf6

  | Data(i) -> i
;;


let hr_hb = function
  | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3" | 4 -> "4" | 5 -> "5" | 6 -> "6" | 7 -> "7"
  | 8 -> "8" | 9 -> "9" | 10 -> "a" | 11 -> "b" | 12 -> "c" | 13 -> "d" | 14 -> "e" | _ -> "f"
;;

let hr_byte i =
  (hr_hb ((i lsr 4) land 0xf)) ^ (hr_hb (i land 0xf))
;;
    

let strop = function
  | STOP -> "STOP"; | ADD -> "ADD" | MUL -> "MUL" | SUB -> "SUB" | DIV -> "DIV" | SDIV -> "SDIV" | MOD -> "MOD" | SMOD -> "SMOD" | ADDMOD -> "ADDMOD" | MULMOD -> "MULMOD" | EXP -> "EXP" | SIGNEXTEND -> "SIGNEXTEND"

  | LT -> "LT" | GT -> "GT" | SLT -> "SLT" | SGT -> "SGT" | EQ -> "EQ" | ISZERO -> "ISZERO" | AND -> "AND" | OR -> "OR" | XOR -> "XOR" | NOT -> "NOT" | BYTE -> "BYTE"

  | SHA3 -> "SHA3"

  | ADDRESS -> "ADDRESS" | BALANCE -> "BALANCE" | ORIGIN -> "ORGIN" | CALLER -> "CALLER" | CALLVALUE -> "CALLVALUE" | CALLDATALOAD -> "CALLDATALOAD" | CALLDATASIZE -> "CALLADATASIZE" | CALLDATACOPY -> "CALLDATACOPY" | CODESIZE -> "CODESIZE" | CODECOPY -> "CODECOPY" | GASPRICE -> "GASPRICE" | EXTCODESIZE -> "EXTCODESIZE" | EXTCODECOPY -> "EXTCODECOPY"

  | BLOCKHASH -> "BLOCKHASH" | COINBASE -> "COINBASE" | TIMESTAMP -> "TIMESTAMP" | NUMBER -> "NUMBER" | DIFFICULTY -> "DIFFICULTY" | GASLIMIT -> "GASLIMIT"

  | POP -> "POP" | MLOAD -> "MLOAD" | MSTORE -> "MSTORE" | MSTORE8 -> "MSTORE8" | SLOAD -> "SLOAD" | SSTORE -> "SSTORE" | JUMP -> "JUMP" | JUMPI -> "JUMPI" | PC -> "PC" | MSIZE -> "MSIZE" | GAS -> "GAS" | JUMPDEST -> "JUMPDEST"
  
  | PUSH1 -> "PUSH1" | PUSH2 -> "PUSH2" | PUSH3 -> "PUSH3" | PUSH4 -> "PUSH4" | PUSH5 -> "PUSH5" | PUSH6 -> "PUSH6" | PUSH7 -> "PUSH7" | PUSH8 -> "PUSH8" | PUSH9 -> "PUSH9" | PUSH10 -> "PUSH10" | PUSH11 -> "PUSH11" | PUSH12 -> "PUSH12" | PUSH13 -> "PUSH13" | PUSH14 -> "PUSH14" | PUSH15 -> "PUSH15" | PUSH16 -> "PUSH16"
  | PUSH17 -> "PUSH17" | PUSH18 -> "PUSH18" | PUSH19 -> "PUSH19" | PUSH20 -> "PUSH20" | PUSH21 -> "PUSH21" | PUSH22 -> "PUSH22" | PUSH23 -> "PUSH23" | PUSH24 -> "PUSH24" | PUSH25 -> "PUSH25" | PUSH26 -> "PUSH26" | PUSH27 -> "PUSH27" | PUSH28 -> "PUSH28" | PUSH29 -> "PUSH29" | PUSH30 -> "PUSH30" | PUSH31 -> "PUSH31" | PUSH32 -> "PUSH32"

  | DUP1 -> "DUP1" | DUP2 -> "DUP2" | DUP3 -> "DUP3" | DUP4 -> "DUP4" | DUP5 -> "DUP5" | DUP6 -> "DUP6" | DUP7 -> "DUP7" | DUP8 -> "DUP8" | DUP9 -> "DUP9" | DUP10 -> "DUP10" | DUP11 -> "DUP11" | DUP12 -> "DUP12" | DUP13 -> "DUP13" | DUP14 -> "DUP14" | DUP15 -> "DUP15" | DUP16 -> "DUP16"

  | SWAP1 -> "SWAP1" | SWAP2 -> "SWAP2" | SWAP3 -> "SWAP3" | SWAP4 -> "SWAP4" | SWAP5 -> "SWAP5" | SWAP6 -> "SWAP6" | SWAP7 -> "SWAP7" | SWAP8 -> "SWAP8" | SWAP9 -> "SWAP9" | SWAP10 -> "SWAP10" | SWAP11 -> "SWAP11" | SWAP12 -> "SWAP12" | SWAP13 -> "SWAP13" | SWAP14 -> "SWAP14" | SWAP15 -> "SWAP15" | SWAP16 -> "SWAP16"

  | LOG0 -> "LOG0" | LOG1 -> "LOG1" | LOG2 -> "LOG1" | LOG3 -> "LOG3" | LOG4 -> "LOG4"

  | CREATE -> "CREATE" | CALL -> "CALL" | CALLCODE -> "CALLCODE" | RETURN -> "RETURN" | DELEGATECALL -> "DELEGATECALL" | INVALID -> "INVALID" | SELFDESTRUCT -> "SELFDESTRUCT"

  | Data i -> hr_byte i
;;


let pushx x =
  match x with
  | 1 -> PUSH1
  | 2 -> PUSH2
  | 3 -> PUSH3
  | 4 -> PUSH4
  | 5 -> PUSH5
  | 6 -> PUSH6
  | 7 -> PUSH7
  | 8 -> PUSH8
  | 9 -> PUSH9
  | 10 -> PUSH10
  | 11 -> PUSH11
  | 12 -> PUSH12
  | 13 -> PUSH13
  | 14 -> PUSH14
  | 15 -> PUSH15
  | 16 -> PUSH16
  | 17 -> PUSH17
  | 18 -> PUSH18
  | 19 -> PUSH19
  | 20 -> PUSH20
  | 21 -> PUSH21
  | 22 -> PUSH22
  | 23 -> PUSH23
  | 24 -> PUSH24
  | 25 -> PUSH25
  | 26 -> PUSH26
  | 27 -> PUSH27
  | 28 -> PUSH28
  | 29 -> PUSH29
  | 30 -> PUSH30
  | 31 -> PUSH31
  | 32 -> PUSH32
  | _ ->
     let _ = print_string "larger than 32byte literals not supported " in
     let _ = flush stdout in
     STOP
;;

let primop prim =
  match prim with
  | "~+~" -> ADD
  | "~*~" -> MUL
  | "~-~" -> SUB
  | "~/~" -> DIV
  | "~%~" -> MOD
  | "~**~" -> EXP
  | "~<~" -> LT
  | "~>~" -> GT
  | "~&&~" -> AND
  | "~||~" -> OR
  | "sha3" -> SHA3
  | "address" -> ADDRESS
  | "balance" -> BALANCE
  | "origin" -> ORIGIN
  | "caller" -> CALLER
  | "callvalue" -> CALLVALUE
  | "gasprice" -> GASPRICE
  | "blockhash" -> BLOCKHASH
  | "coinbase" -> COINBASE
  | "timestamp" -> TIMESTAMP
  | "number" -> NUMBER
  | "difficulty" -> DIFFICULTY
  | "gaslimit" -> GASLIMIT
  | "gas" -> GAS
  | _ -> STOP
;;

let bundle array n pad =
  let rec bundle array acc =
    if Array.length array = 0 then
      List.rev acc
    else if Array.length array < n then
      let array = Array.concat [ array; Array.make (n - Array.length array) pad ] in
      List.rev (array :: acc)
    else
      let word = Array.sub array 0 n in
      let array = Array.sub array n (Array.length array - n) in
      bundle array (word :: acc) 
  in
  bundle array []
;;

let compile tables sources =
  let box i size =
    let rec box i acc =
      if i = 0 then
	acc
      else
	box (i lsr 8) ((Data (i land 0xff)) :: acc)	
    in
    let bytes = box i [] in
    match size with
    | None -> (match bytes with [] -> [| Data 0 |] | _ -> Array.of_list bytes)
    | Some sz -> Array.of_list ((repeat (Data 0) (sz-List.length bytes)) @ bytes)
  in

  let push i =
    let bytes = box i None in
    Array.append [| Array.length bytes |> pushx |] bytes
  in

  let table_uplook =
    List.fold_left
      (fun uplook table ->
	let Table(name, numargs, instrs) = table in
	IMap.add name table uplook)
      emptymap
      tables
  in

  let line_uplook =
    ref emptymap 
  in

  let headersize = 0x33 in
  
  let line_add name line =
    line_uplook := IMap.add name (line + headersize) !line_uplook;
    !line_uplook
  in
  
  let line_lookup name =
      IMap.find name !line_uplook
  in  

  let dependencies_get dep_ops deps compile =
    let rec dg deps dep_ops =
      match deps with
      | [] ->
	 dep_ops
	   
      | dep :: deps ->
	 try
	   let _ = line_lookup dep in
	   dg deps dep_ops
	 with
	   _ ->
	     dg deps (compile dep dep_ops)
    in
    dg deps dep_ops
  in
  
  let rec compile goal dep_ops =
    let dep_ops, ops =
      match goal with
      | "info" ->
	 let ops =
	   [| (* [ ..., :caller, ~ref ] *)
	     JUMPDEST;
	     DUP1;
	     MLOAD;
	     DUP1;
	     PUSH9;
	     Data 0xff;
	     Data 0xff;
	     Data 0xff;
	     Data 0xff;
	     Data 0x0;
	     Data 0x0;
	     Data 0x0;
	     Data 0x0;
	     Data 0x0;
	     AND;
	     PUSH6;
	     Data 1;
	     Data 0;
	     Data 0;
	     Data 0;
	     Data 0;
	     Data 0;
	     SWAP1;
	     DIV;
	     SWAP1;
	     DUP1;
	     PUSH5;
	     Data 0xff;
	     Data 0xff;
	     Data 0xff;
	     Data 0xff;
	     Data 0x0;
	     AND;
	     PUSH2;
	     Data 1;
	     Data 0;
	     SWAP1;
	     DIV;
	     SWAP1;
	     PUSH1;
	     Data 0xff;
	     AND;
	     SWAP3;(* [ ..., :caller, !flags, !sat, !nargs, ~ref ] *)
	     SWAP4;
	     JUMP; (* [ ..., ~ref, !flags, !sat, !nargs ] *)	
	   |]
	 in
	 dep_ops, ops
	   
      | "copy" ->
	 let dep_ops = dependencies_get dep_ops ["info"] compile in
	 let ops = 
	   let _1 =
	     let _1_1 =
	       [|
		 JUMPDEST; (* :caller, !memory, ~ref *)
	       |]
	     in
	     let _1_2 = push (line_lookup "info") in
	     let _1_3 = 
	       [|
		 SWAP1;
		 PC;
		 PUSH1;
		 Data 0x6;
		 ADD;
		 SWAP2;
		 JUMP;
	       |]
	     in
	     Array.concat [ _1_1; _1_2; _1_3 ]
	   in
	   let _2 =
	     [|
	       JUMPDEST; (* [ ... :caller, !memory, ~ref, !flags, !sat, !nargs ] *)
	       PUSH1;
	       Data 0x2;
	       ADD;
	       SWAP2;
	       POP; (* [ ..., :caller, !memory, ~ref, !size, !sat ] *)
	       PUSH1;
	       Data 0x2;
	       ADD;
	       PUSH1;
	       Data 32; 
	       MUL; (* [ ..., :caller, !memory, ~ref, !size, !bytesize ] *)
	       DUP4;
	       ADD;  (* [ ..., :caller, ~copy, ~ref, !size, !memory ] *)
	       SWAP3; (* [ ..., :caller, !memory, ~ref, !size, ~copy ] *)
	       SWAP1; (* [ ..., :caller, !memory, ~ref, ~copy, !size ] *)
	       DUP3; (* ..., :caller, !memory, ~ref, ~copy, !size, ~ref *)
	       DUP3; (* ..., :caller, !memory, ~ref, ~copy, !size, ~ref, ~copy *)
	     |]
	   in
	   let _3 =
	     let _3_1 =
	       [|
		 JUMPDEST; (* [ ..., :caller, !memory, ~ref, ~copy, !size, ~ref_i, ~copy_i ] *) 
		 DUP2; 
		 MLOAD;
		 DUP2;
		 MSTORE;
		 PUSH1;
		 Data 32;
		 ADD;
		 SWAP1;
		 PUSH1;
		 Data 32;
		 ADD; (* [ ..., :caller, !memory, ~ref, ~copy, !size, ~copy_{i+1}, ~ref_{i+1} ] *)
		 SWAP1; (* [ ..., :caller, !memory, ~ref, ~copy, !size, ~ref_{i+1}, ~copy_{i+1} ] *)
		 SWAP2; (* [ ..., :caller, !memory, ~ref, ~copy, ~copy_{i+1}, ~ref_{i+1}, !size ] *)
		 PUSH1;
		 Data 0x1;
		 SWAP1;
		 SUB;
		 SWAP2; (* ..., :caller, !memory, ~ref, ~copy, !size--, ~copy_{i+1}, ~ref_{i+1} *)
		 DUP3;
		 PC;
	       |]
	     in
	     let _3_2 = push (Array.length _3_1 - 1) in
	     let _3_3 =
	       [|
		 SWAP1;
		 SUB; (* [ ..., :caller, !memory, ~ref, ~copy, !size--, ~copy_{i+1}, ~ref_{i+1}, !size--, LOOP_START ] *)
		 JUMPI;
		 POP;
		 POP;
		 POP;
		 SWAP1; 
		 SWAP2;
		 SWAP3;
		 JUMP; (* ..., !memory, ~ref, ~copy *)
	       |]
	     in
	     
	     Array.concat [ _3_1; _3_2; _3_3 ]
	   in
	   Array.concat [ _1; _2; _3 ] 
	 in
	 dep_ops, ops

      | "primeval" ->
	 let dep_ops = dependencies_get dep_ops ["eval"] compile in
	 let ops =
	   let _1 =
	     let _1_1 =
	       [| 
		 JUMPDEST; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !memory, !args, !nargs *)
		 SWAP2;
		 DUP2; 
		 MLOAD; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, !memory, ~arg_i *) 
		 DUP4; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, !memory, ~arg_i, !nargs *)
		 PUSH1;
		 Data 0x0;
		 EQ;
		 ISZERO; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, !memory, ~arg_i, !nargs <> 0 *)
	       |]
	     in
	     let _1_3 =
	       [| 
		 PC;
		 ADD; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, !memory, ~arg_i, !nargs <> 0, :_2 *)
		 JUMPI;
		 POP;
		 POP;
		 POP;
		 POP;
		 JUMP;	     
	       |]
	     in
	     let _1_2 = push (Array.length _1_3) in
	     
	     Array.concat [ _1_1; _1_2; _1_3 ]
	   in
	   let _2 =
	     let _2_1 =
	       [|
		 JUMPDEST; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, !memory, ~arg_i *)
	       |]
	     in
	     let _2_2 = push (line_lookup "eval") in
	     let _2_3 =
	       [|	     
		 SWAP2; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, :eval, ~arg_i, !memory *)
		 SWAP1; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, :eval, !memory, ~arg_i *)
		 DUP1; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, :eval, !memory, ~arg_i, ~arg_i *)
		 MLOAD; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, :eval, !memory, ~arg_i, !arg_i_info *)
		 PUSH1;
		 Data 0x1;
		 AND; 
		 ISZERO; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, :eval, !memory, ~arg_i, not !is_literal*)
		 PC;
	       |]
	     in
	     let _2_4 = push (Array.length _2_3 + Array.length _2_2 + Array.length _2_1 - 1) in
	     let _2_5 =
	       [|
		 SWAP1; 
		 SUB; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, :eval, !memory, ~arg_i, not !is_literal, _2 *)
		 SWAP4;(* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, _2, !memory, ~arg_i, not !is_literal, :eval *)
		 JUMPI; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, _2, !memory, ~arg_i *)
		 PUSH1;
		 Data 32;
		 ADD;
		 MLOAD; (* ..., !lit_0, ..., !lit_{i-1}, :caller, !nargs, !args, _2, !memory, !lit_i *)
		 SWAP5; (* ..., !lit_0, ..., !lit_i, !nargs, !args, _2, !memory, :caller *)
		 SWAP4;
		 PUSH1;
		 Data 1;
		 SWAP1;
		 SUB; (* ..., !lit_0, ..., !lit_i, :caller, !args, _2, !memory, :!nargs-- *)
		 SWAP2; 
		 POP; (* ..., !lit_0, ..., !lit_i, :caller, !args, !nargs--, !memory *)
		 SWAP2;
		 PUSH1;
		 Data 32;
		 ADD; 
		 SWAP1; (* ..., !lit_0, ..., !lit_i, :caller, !memory, !args++, !nargs-- *)
		 PC;
	       |]
	     in
	     Array.concat [ _2_1; _2_2; _2_3; _2_4; _2_5 ]
	   in
	   let _3 =
	     let _3_1 = push (Array.length _1 + Array.length _2 - 1 ) in
	     let _3_2 =
	       [| 
		 SWAP1;
		 SUB; (* ..., !lit_0, ..., !lit_i, :caller, !memory, !args++, !nargs--, :primeval *)
		 JUMP
	       |]
	     in
	     Array.concat [ _3_1; _3_2 ]
	   in
	   Array.concat [ _1; _2; _3 ]
	 in
	 dep_ops, ops

	   
      | "eval" ->
	 let dep_ops = dependencies_get dep_ops ["info"] compile in
	 let ops =
	   let _1 =
	     let _1_1 =
	       [|
		 JUMPDEST; (* :caller, !memory, ~ref *)
	       |]
	     in
	     let _1_2 = push (line_lookup "info") in
	     let _1_3 =
	       [|
		 SWAP1;
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP2;
		 JUMP;
	       |]
	     in
	     Array.concat [ _1_1; _1_2; _1_3 ]
	   in
	   let _2 =
	     let _2_1 =
	       [| 
		 JUMPDEST; (* [ ..., :caller, !memory, ~ref, !flags, !sat, !nargs ] *)
		 EQ;
		 ISZERO;
		 OR;
	       |]
	     in
	     let _2_3 =
	       [|
		 PC;
		 ADD; (* [ ..., :caller, !memory, ~ref, !nargs <> !sat || !flags, _3 ] *)
		 JUMPI; (* ..., :caller, !memory, ~ref *)
		 PUSH1;
		 Data 32;
		 ADD;
		 DUP1;
		 PUSH1;
		 Data 32;
		 ADD; (* ..., :caller, !memory ~ref_head, ~ref_body *)
		 SWAP1;
		 MLOAD;
		 JUMP;
	       |]
	     in
	     let _2_2 = push (Array.length _2_3) in

	     Array.concat [ _2_1; _2_2; _2_3 ]
	   in
	   let _3 =
	     [|
	       JUMPDEST; (* ..., :caller, !memory, ~ref *)
	       SWAP1;
	       SWAP2;
	       JUMP;
	     |]
	   in
	   Array.concat [ _1; _2; _3 ]

	 in
	 dep_ops, ops

      | "app" ->
	 let dep_ops = dependencies_get dep_ops ["info";"copy";"eval"] compile in
	 let ops =
	   let _1 =
	     let _1_1 =
	       [| 
		 JUMPDEST; (* ..., :caller, !memory, ~ref, ~arg *)
		 SWAP1;
	       |]
	     in
	     let _1_2 = push (line_lookup "info") in
	     let _1_3 =
	       [| (* ..., :caller, !memory, ~arg, ~ref, :info *)
		 SWAP1; (* ..., :caller, !memory, ~arg, :info, ~ref *)
		 PC; 
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP2;
		 JUMP; 
	       |]
	     in
	     Array.concat [ _1_1; _1_2; _1_3 ]
	   in

	   let _3 =
	     let _3_1 =
	       [| (* ..., :caller, !memory, ~arg, ~ref *)
		 SWAP1;
		 SWAP2; (* ..., :caller, ~arg, ~ref, !memory *)

	       |]
	     in
	     let _3_2 = push (line_lookup "copy") in
	     let _3_3 =
	       [| 
		 SWAP2; (* ..., :caller, ~arg, :copy, !memory, ~ref *)
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP3;
		 JUMP;
	       |]
	     in
	     let _3_4 =
	       [|
		 JUMPDEST; (* ..., :caller, ~arg, !memory, ~ref, ~copy *)
		 SWAP1;
		 POP; (* ..., :caller, ~arg, !memory, ~copy *)
	       |]		 
	     in
	     let _3_5 = push (line_lookup "eval") in
	     let _3_6 =
	       [|
		 SWAP2;
		 SWAP1; (* ..., :caller, ~arg, :eval, !memory, ~copy *)
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP3;
		 JUMP;
	       |]
	     in
	     Array.concat [ _3_1; _3_2; _3_3; _3_4; _3_5; _3_6 ]
	   in
	   let _4 =
	     [|
	       JUMPDEST; (*. ..., :caller, ~arg, !memory, ~res *)
	       SWAP1;
	       SWAP2; (*. ..., :caller, !memory, ~res, ~arg *)
	       SWAP1;
	     |]
	   in
	   let _2 =
	     let _2_1 =
	       [|
		 JUMPDEST; (* ..., :caller, !memory, ~arg, ~ref, !flags, !sat, !nargs *)
		 SWAP1;
		 GT; 
		 SWAP1;
		 POP; (* ..., :caller, !memory, ~arg, ~ref, !sat > !nargs *)
	       |]
	     in
	     let _2_3 =
	       [|
		 PC;
		 ADD; (* ..., :caller, !memory, ~arg, ~ref, !sat > !nargs, _5 *)
		 JUMPI;
	       |]
	     in
	     let _2_2 = push (Array.length _2_3 + Array.length _3 + Array.length _4) in
	     Array.concat [ _2_1; _2_2; _2_3 ]
	   in
	   let _5 =
	     [|
	       JUMPDEST; (* ..., :caller, !memory, ~arg, ~ref *)
	       SWAP1; (* ..., :caller, !memory, ~ref, ~arg *)
	       DUP2; 
	       DUP1; (* ..., :caller, !memory, ~ref, ~arg, ~ref, ~ref *)
	       MLOAD; (* ..., :caller, !memory, ~ref, ~arg, ~ref, !ref_flags *)
	       PUSH2; 
	       Data 1;
	       Data 0;
	       ADD; (* ..., :caller, !memory, ~ref, ~arg, ~ref, !flags *)
	       DUP1; (* ..., :caller, !memory, ~ref, ~arg, ~ref, !flags, !flags *)
	       DUP3;
	       MSTORE;
	       PUSH5;
	       Data 0xff;
	       Data 0xff;
	       Data 0xff;
	       Data 0xff;
	       Data 0x0;
	       AND; 
	       PUSH2;
	       Data 1;
	       Data 0;
	       SWAP1;
	       DIV; (* ..., :caller, !memory, ~ref, ~arg, ~ref, !nargs *)
	       PUSH1;
	       Data 1;
	       ADD; (* ..., :caller, !memory, ~ref, ~arg, ~ref, !word_size *)
	       PUSH1;
	       Data 32;
	       MUL;
	       ADD; (* ..., :caller, !memory, ~ref, ~arg, !ref_end *)
	       MSTORE;
	       SWAP1;
	       SWAP2;
	       JUMP;
	     |]
	   in
	   Array.concat [ _1; _2; _3; _4; _5 ]
	 in
	 dep_ops, ops
	   
      | "expand" ->
	 let dep_ops = dependencies_get dep_ops ["eval"] compile in
	 let ops =
	   let _1 =
	     let _1_1 =
	       [| 
		 JUMPDEST; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !memory, !args, !nargs, !flag *)
		 SWAP3;
		 DUP3; 
		 MLOAD; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, !memory, ~arg_i *) 
		 DUP3; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, !memory, ~arg_i, !nargs *) 
		 PUSH1;
		 Data 0x0;
		 EQ;
		 ISZERO; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, !memory, ~arg_i, !nargs <> 0 *) 
	       |]
	     in
	     let _1_3 =
	       [| 
		 PC;
		 ADD; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, !memory, ~arg_i, !nargs <> 0, :2 *) 
		 JUMPI;
		 POP; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, !memory  *)
		 SWAP4;
		 SWAP3;
		 POP;
		 POP;
		 POP; (* ..., !flag_0, ..., !flag_{i-1}, !memory, :caller  *)
		 JUMP;	     
	       |]
	     in
	     let _1_2 = push (Array.length _1_3) in
	     
	     Array.concat [ _1_1; _1_2; _1_3 ]
	   in
	   let _2 =
	     let _2_1 =
	       [|
		 JUMPDEST; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, !memory, ~arg_i *) 
	       |]
	     in
	     let _2_2 = push (line_lookup "eval") in
	     let _2_3 =
	       [|	     
		 SWAP2; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, :eval, ~arg_i, !memory *)
		 SWAP1; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, :eval, !memory, ~arg_i *)
		 DUP1; (*  ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, :eval, !memory, ~arg_i, ~arg_i *)
		 MLOAD; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, :eval, !memory, ~arg_i, !arg_i_info *)
		 DUP7;		 
		 AND;
		 ISZERO; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !nargs, !args, :eval, !memory, ~arg_i, not !is_flag*)
		 PC;
	       |]
	     in
	     let _2_4 = push (Array.length _2_3 + Array.length _2_2 + Array.length _2_1 - 1) in
	     let _2_5 =
	       [|
		 SWAP1; 
		 SUB; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, :eval, !memory, ~arg_i, not !is_flag, _2 *)
		 SWAP4;(* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, _2, !memory, ~arg_i, not !is_flag, :eval *)
		 JUMPI; (* ..., !flag_0, ..., !flag_{i-1}, :caller, !flag, !args, !nargs, _2, !memory, ~flag_i *)
		 SWAP6; (* ..., !flag_0, ..., !flag_i, !flag, !args, !nargs, _2, !memory, :caller *)
		 SWAP5; (* ..., !flag_0, ..., !flag_i, :caller, !args, !nargs, _2, !memory, !flag *)
		 SWAP1; (* ..., !flag_0, ..., !flag_i, :caller, !args, !nargs, _2, !flag, !memory *)
		 SWAP4; (* ..., !flag_0, ..., !flag_i, :caller, !memory, !nargs, _2, !flag, !args *)
		 PUSH1;
		 Data 32;
		 ADD;
		 SWAP3; (* ..., !flag_0, ..., !flag_i, :caller, !memory, !args++, _2, !flag, !nargs *)
		 PUSH1;
		 Data 1;
		 SWAP1;
		 SUB; (* ..., !flag_0, ..., !flag_i, :caller, !memory, !args++, _2, !flag, !nargs-- *)
		 SWAP2;
		 POP; (* ..., !flag_0, ..., !flag_i, :caller, !memory, !args++, !nargs--, !flag *)
		 PC;
	       |]
	     in
	     Array.concat [ _2_1; _2_2; _2_3; _2_4; _2_5 ]
	   in
	   let _3 =
	     let _3_1 = push (Array.length _1 + Array.length _2 - 1 ) in
	     let _3_2 =
	       [| 
		 SWAP1;
		 SUB; (* ..., !flag_0, ..., !flag_i, :caller, !memory, !args++, !nargs--, !flag, :expand *)
		 JUMP
	       |]
	     in
	     Array.concat [ _3_1; _3_2 ]
	   in
	   Array.concat [ _1; _2; _3 ]
	 in
	 dep_ops, ops
	   
      | "~=~" ->
	 let dep_ops = dependencies_get dep_ops [ "info"; "true"; "false" ] compile in
	 let ops =
	   let _1 =
	     let _1_1 =
	       [|
		 JUMPDEST; (* ..., :caller, !memory, !args *)
	       |]
	     in
	     let _1_2 = push (line_lookup "eval") in
	     let _1_3 =
	       [|
		 DUP3;
		 DUP3;
		 MLOAD;
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP3;
		 JUMP;
		 JUMPDEST; (* ..., :caller, !memory, !args, !memory', ~arg_1 *)
		 SWAP3;
		 POP;		 
	       |]
	     in
	     let _1_4 = push (line_lookup "eval") in
	     let _1_5 =
	       [|
		 SWAP2;
		 PUSH1;
		 Data 32;
		 ADD;
		 MLOAD;
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP3;
		 JUMP;
		 JUMPDEST; (* ..., :caller, ~arg_i, !memory, ~arg_0 *)
		 SWAP1;
		 SWAP2;
	       |]
	     in
	     let _1_6 = push (line_lookup "info") in
	     let _1_7 =
	       [|
		 SWAP1;
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP2;
		 JUMP;		 
	       |]
	     in
	     let _1_8 =
	       [|
		 JUMPDEST; (* ..., :caller, !memory, ~l, ~r, !r_flags, !r_sat, !r_nargs *)
		 SWAP2;
		 POP;
		 POP;
		 PUSH1;
		 Data 2;
		 ADD; (* ..., :caller, !memory, ~l, ~r, !size *)
	       |]
	     in
	     Array.concat [ _1_1; _1_2; _1_3; _1_4; _1_5; _1_6; _1_7; _1_8 ]	     
	   in
	   let _2 =
	     let _2_1 =
	       [|
		 JUMPDEST; (* ..., :caller, !memory, ~l, ~r, !size *)
		 DUP1;
	       |]
	     in
	     let _2_3 =
	       [|
		 PC;
		 ADD;
		 JUMPI;
		 POP;
		 POP;
		 POP;
		 DUP1;
		 PUSH1;
		 Data 2;
		 PUSH6;
		 Data 1;
		 Data 0;
		 Data 0;
		 Data 0;
		 Data 0;
		 Data 0;
		 MUL;
		 DUP2;
		 MSTORE;
		 PUSH1;
		 Data 32;
		 ADD;
	       |]
	     in
	     let _2_4 = push (line_lookup "true") in
	     let _2_5 =
	       [|
		 DUP2;
		 MSTORE;
		 PUSH1;
		 Data 96;
		 ADD;
		 SWAP2;
		 JUMP;
	       |]
	     in
	     let _2_2 = push (Array.length _2_3 + Array.length _2_4 + Array.length _2_5) in
	     Array.concat [ _2_1; _2_2; _2_3; _2_4; _2_5 ]
	   in
	   let _3 =
	     let _3_1 =
	       [|
		 JUMPDEST; (* ..., :caller, !memory, ~l, ~r, !size *)
		 PUSH1;
		 Data 1;
		 SWAP1;
		 SUB;
		 SWAP1;
		 DUP1;
		 MLOAD;
		 SWAP1;
		 PUSH1;
		 Data 32;
		 ADD; (* ..., :caller, !memory, ~l, !size--, !r_i, ~r++*)
		 SWAP3;
		 DUP1;
		 MLOAD;
		 SWAP1;
		 PUSH1;
		 Data 32;
		 ADD; (* ..., :caller, !memory, ~r++, !size--, !r_i, !l_i, ~l++ *)
		 SWAP3;
		 SWAP2; (* ..., :caller, !memory, ~r++, ~l++, !size--, !l_i, !r_i *)
		 EQ; (* ..., :caller, !memory, ~r++, ~l++, !size--, !l_i = !r_i *)
		 PC;
	       |]
	     in
	     let _3_2 = push (Array.length _3_1 + Array.length _2 - 1) in
	     let _3_3 =
	       [|
		 SWAP1;
		 SUB;
		 JUMPI;
		 POP;
		 POP;
		 POP;
		 DUP1;
		 PUSH1;
		 Data 2;
		 PUSH6;
		 Data 1;
		 Data 0;
		 Data 0;
		 Data 0;
		 Data 0;
		 Data 0;
		 MUL;
		 DUP2;
		 MSTORE;
		 PUSH1;
		 Data 32;
		 ADD;
	       |]
	     in
	     let _3_4 = push (line_lookup "false") in
	     let _3_5 =
	       [|
		 DUP2;
		 MSTORE;
		 PUSH1;
		 Data 96;
		 ADD;
		 SWAP2;
		 JUMP;
	       |]
	     in
	     Array.concat [ _3_1; _3_2; _3_3; _3_4; _3_5 ]
	   in
	   Array.concat [ _1; _2; _3 ]
	 in
	 dep_ops, ops	   

      | "~^~" ->
	 let dep_ops = dependencies_get dep_ops ["copy";"expand"] compile in
	 let ops =
	   let _1 =
	     let _1_1 =
	       [| (* :caller, !memory, !args *)
		 JUMPDEST;
		 DUP2;
	       |]
	     in
	     let _1_2 = push (line_lookup "expand") in
	     let _1_3 =
	       [| (* ..., :caller, !memory, !args, !memory, :expand *)		 
		 SWAP2; (* ..., :caller, !memory, :expand, !memory, !args *)
		 PUSH1;
		 Data 2;
		 DUP1; (* ..., :caller, !memory, :expand, !memory, !nargs, 2, 2 *)
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP5;
		 JUMP;
	       |]
	     in
	     let _1_4 =
	       [|
		 JUMPDEST; (* ..., :caller, !memory, ~arg_0, ~arg_1, !memory' *)
		 SWAP3;
		 POP; (* ..., :caller, !memory, ~arg_0, ~arg_1 *)
		 SWAP1; (* :caller, !memory, ~arg_1, ~arg_0 *)
	       |]
	     in
	     let _1_5 = push (line_lookup "copy") in
	     let _1_6 =
	       [|
		 SWAP2;
		 SWAP3;
		 SWAP1; (* :caller, ~arg_1, :copy, !memory, ~arg_0 *)
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP3;		 
		 JUMP;
	       |]
	     in
	     Array.concat [_1_1; _1_2; _1_3; _1_4; _1_5; _1_6 ]
	   in
	   let _2 =
	     let _2_1 =
	       [|
		 JUMPDEST; (* :caller, ~arg_1, !memory, ~arg_0, ~arg_0_copy *)
		 SWAP1;
		 POP;
		 DUP1;
		 PUSH1;
		 Data 64;
		 ADD;
		 MLOAD; (* :caller, ~arg_1, !memory, ~arg_0_copy, ~arg_0_len *)
		 SWAP2;
		 SWAP3; (* :caller, !memory, !arg_0_len, ~arg_0_copy, ~arg_1 *)
		 DUP1; 
		 PUSH1;
		 Data 64;
		 ADD;
		 MLOAD; (* :caller, !memory, !arg_0_len, ~arg_0_copy, ~arg_1, !arg_1_len *)
		 DUP1;
		 SWAP2;
		 SWAP4; (* :caller, !memory, ~arg_1, ~arg_0_copy, !arg_1_len, !arg_1_len, !arg_0_len *)
		 PUSH1;
		 Data 32;
		 SWAP1;
		 MOD;
		 ADD;
		 PUSH1;
		 Data 32;
		 SWAP1;
		 DIV; (* :caller, !memory, ~arg_1, ~arg_0_copy, !arg_1_len, !wordsize_growth *)
		 DUP1;
		 PUSH6;
		 Data 1;
		 Data 0;
		 Data 0;
		 Data 0;
		 Data 1;
		 Data 0;
		 MUL; (* :caller, !memory, ~arg_1, ~arg_0_copy, !arg_1_len, !wordsize_growth, !flag_growth *)
		 DUP4;
		 MLOAD;
		 ADD;
		 DUP4;
		 MSTORE; (* :caller, !memory, ~arg_1, ~arg_0_copy, !arg_1_len, !wordsize_growth *)
		 SWAP1; 
		 DUP3; (* :caller, !memory, ~arg_1, ~arg_0_copy, !wordsize_growth, !arg_1_len, ~arg_0_copy *)
		 PUSH1;
		 Data 64;
		 ADD;
		 MLOAD; (* :caller, !memory, ~arg_1, ~arg_0_copy, !wordsize_growth, !arg_1_len, !orig_len *)
		 SWAP1;
		 DUP2;
		 ADD; (* :caller, !memory, ~arg_1, ~arg_0_copy, !wordsize_growth, !orig_len, !res_len *)
		 DUP4; 
		 PUSH1;
		 Data 64;
		 ADD;
		 MSTORE; (* :caller, !memory, ~arg_1, ~arg_0_copy, !wordsize_growth, !orig_len *)
		 SWAP1;
		 SWAP3;
		 PUSH1;
		 Data 64;
		 ADD; (* :caller, !memory, !wordsize_growth, ~arg_0_copy, !orig_len, ~arg_1_bytesize *)
		 DUP1;
		 MLOAD;
		 PUSH1;
		 Data 32;
		 SWAP1;
		 DIV;
		 SWAP1;
		 PUSH1;
		 Data 32;
		 ADD; (* ..., :caller, !memory, !wordsize_growth, ~arg_0_copy, !orig_len, !arg_1_wordsize, ~arg_1_data *)
		 SWAP1;
		 SWAP2; (* ..., :caller, !memory, !wordsize_growth, ~arg_0_copy, !arg_1_wordsize, ~arg_1_data, !orig_len *)
		 DUP4;
		 PUSH1;
		 Data 96;
		 ADD;
		 ADD;
	       |]
	     in
	     let _2_2 =
	       [|
		 JUMPDEST; (* :caller, !memory, !wordsize_growth, ~res, !size, ~from, ~into *)
		 DUP2;
		 MLOAD; (* :caller, !memory, !wordsize_growth, ~res, !size, ~from, ~into, !from *)
		 DUP2; (* :caller, !memory, !wordsize_growth, ~res, !size, ~from, ~into, !from, ~into *)
		 MSTORE;
		 PUSH1;
		 Data 32;
		 ADD; (* :caller, !memory, !wordsize_growth, ~res, !size, ~from, ~into++ *)
		 SWAP1;
		 PUSH1;
		 Data 32; 
		 ADD; (* :caller, !memory, !wordsize_growth, ~res, !size, ~into++, ~from++ *)
		 SWAP1; (* :caller, !memory, !wordsize_growth, ~res, !size, ~from++, ~into++ *)
		 DUP3;
		 PUSH1;
		 Data 1;
		 SWAP1;
		 SUB; 
		 SWAP3; (* :caller, !memory, !wordsize_growth, ~res, !size--, ~from++, ~into++, !size *) 
		 PUSH1;
		 Data 0;
		 SWAP1;
		 GT;
		 PC;		 
	       |]
	     in
	     let _2_3 = push (Array.length _2_2 - 1) in
	     let _2_4 =
	       [|
		 SWAP1;
		 SUB; (* :caller, !memory, !wordsize_growth, ~res, !size--, ~from++, ~into++, !size > 0, _2_2 *) 
		 JUMPI; (* :caller, !memory, !wordsize_growth, ~res, !size--, ~from++, ~into++ *)
		 POP;
		 POP;
		 POP;
		 SWAP2;
		 SWAP1;
		 PUSH1;
		 Data 32;
		 MUL;
		 ADD;
		 SWAP2;
		 JUMP;
	       |]
	     in
	     
	     Array.concat [ _2_1; _2_2; _2_3; _2_4 ]
	   in
	   
	   Array.concat [ _1; _2 ]
	 in
	 dep_ops, ops
	 
      | _ when isprimitive goal ->
	 let dep_ops = dependencies_get dep_ops ["primeval"] compile in
	 let ops =
	   let _1 =
	     let _1_1 =
	       [|
		 JUMPDEST; (* ..., :caller, !memory, !args *)
		 DUP2; (* ..., :caller, !memory, !args, !memory *)
	       |]
	     in
	     let _1_2 = push (line_lookup "primeval") in
	     let _1_3 =
	       [| 
		 SWAP2; (* ..., :caller, !memory, :primeval, !memory, !args *)
	       |]
	     in
	     let _1_4 = push (primarity goal) in
	     let _1_5 = 
	       [| (* ..., :caller, !memory, :primeval, !memory, !args, !primarity *)
		 PC;
		 PUSH1;
		 Data 6;
		 ADD;
		 SWAP4;
		 JUMP;
	       |]
	     in
	     Array.concat [ _1_1; _1_2; _1_3; _1_4; _1_5 ]
	   in
	   let _2 =
	     [|
	       JUMPDEST; (* ..., :caller, !memory, !lit_0, ..., !lit_n *)
	       primop goal; (* ..., :caller, !memory, !res *)
	       DUP2; (* ..., :caller, ~res, !res, !memory *)
	       PUSH1;
	       Data 1;
	       DUP2;
	       MSTORE; (* ..., :caller, ~res, !res, !memory *)
	       PUSH1;
	       Data 32;
	       ADD;
	       SWAP1;
	       DUP2;
	       MSTORE; (* ..., :caller, ~res, !memory *)
	       PUSH1;
	       Data 32;
	       ADD;
	       SWAP2;	       
	       JUMP;
	     |]
	   in

	   Array.concat [ _1; _2 ]
	 in
	 dep_ops, ops
	   
      | _ ->
	 let Table(name,numargs,instrs) = IMap.find goal table_uplook in
	 let rec bodyc body =
	   match body with
	   | Local i :: body ->
	      let ops =
		let _1 =
		  [| (* ..., :caller, !memory, !args, !locals, !local_index, ~head *)
		    DUP3
		  |]
		in
		let _2 = push i in
		let _3 =
		  [| (* ..., :caller, !memory, !args, !locals, !local_index, ~head, !locals, i *)
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD;
		    MLOAD
		  |]
		in
		let _4 = push (line_lookup "app") in
		let _5 =
		  [| (* ..., :caller, !memory, !args, !locals, !local_index, ~head, ~local_i, :app *)
		    SWAP3; (* ..., :caller, !memory, !args, !locals, :app, ~head, ~local_i, !local_index *)
		    SWAP6; (* ..., :caller, !local_index, !args, !locals, :app, ~head, ~local_i, !memory *)
		    SWAP2; (* ..., :caller, !local_index, !args, !locals, :app, !memory, ~local_i, ~head *)
		    SWAP1; (* ..., :caller, !local_index, !args, !locals, :app, !memory, ~head, ~local_i *)
		    PC;
		    PUSH1;
		    Data 6;
		    ADD;
		    SWAP4;
		    JUMP;
		    JUMPDEST; (* ..., :caller, !local_index, !args, !locals, !memory, ~head *)
		    SWAP1;
		    SWAP4;
		    SWAP1; (* ..., :caller, !memory, !args, !locals, !local_index, ~head *)
		  |]
		in
		Array.concat [ _1; _2; _3; _4; _5; bodyc body ]
	      in
	      ops
		
	  | Arg i :: body ->
	      let ops =
		let _1 =
		  [| (* ..., :caller, !memory, !args, !locals, !local_index, ~head *)
		    DUP4
		  |]
		in
		let _2 = push i in
		let _3 =
		  [|
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD;
		    MLOAD
		  |]
		in
		let _4 = push (line_lookup "app") in
		let _5 =
		  [| (* ..., :caller, !memory, !args, !locals, !local_index, ~head, ~arg_i, :app *)
		    SWAP3;
		    SWAP6;
		    SWAP2;
		    SWAP1;
		    PC;
		    PUSH1;
		    Data 6;
		    ADD;
		    SWAP4;
		    JUMP;
		    JUMPDEST;
		    SWAP1;
		    SWAP4;
		    SWAP1;
		  |]
		in
		Array.concat [ _1; _2; _3; _4; _5; bodyc body ]
	      in
	      ops

	   | Literal i :: body ->
	      let ops =
		let _1 =
		  [| (* :caller, !memory, !args, !locals, !local_index, ~ref *)
		    DUP5;
		    PUSH1;
		    Data 1;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;		    
		  |]
		in
		let _2 = push i in
		let _3 =
		  [|
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		    SWAP5; (* :caller, !memory, !args, !locals, !local_index, ~ref, ~lit *)
		  |]
		in
		let _4 = push (line_lookup "app") in
		let _5 =
		  [|
		    SWAP3; (* :caller, !memory, !args, !locals, :app, ~ref, ~lit, !local_index *)
		    SWAP6; (* :caller, !local_index, !args, !locals, :app, ~ref, ~lit, !memory *)
		    SWAP2;
		    SWAP1;
		    PC;
		    PUSH1;
		    Data 6;
		    ADD;
		    SWAP4;
		    JUMP;
		    JUMPDEST; (* :caller, !locals_index, !args, !locals, !memory, ~ref *)
		    SWAP1;
		    SWAP4;
		    SWAP1;
		  |]
		in
		Array.concat [ _1; _2; _3; _4; _5; bodyc body ]		
	      in
	      ops

	   | Hstring s :: body ->
	      let data c = Data (int_of_char c) in
	      let bytes =
		unquote s |> charlistof |> List.map data |> Array.of_list
	      in
	      let wordsize = 2 + (Array.length bytes / 32) in
	      let ops =
		let _1 =
		  [| (* ..., :caller, !memory, !args, !locals, !local_index, ~ref *)
		    DUP5;		    
		  |]
		in
		let _2 = push wordsize in
		let _3 =
		  [| (* ..., :caller, !memory, !args, !locals, !local_index, ~ref, !memory, !wordsize *)
		    PUSH6;
		    Data 1;
		    Data 0;
		    Data 0;
		    Data 0;
		    Data 1;
		    Data 0;
		    MUL;
		    PUSH1;
		    Data 2;
		    ADD;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		    PUSH1;
		    Data 32;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;		    
		  |]
		in
		let _4 = push (Array.length bytes) in
		let _5 =
		  [|
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		  |]
		in
		let _6 =
		  let store_word word =
		    let _1 = [| pushx (Array.length word) |] in
		    let _2 = word in
		    let _3 =
		      [|
			DUP2;
			MSTORE;
			PUSH1;
			Data 32;
			ADD
		      |]
		    in
		    Array.concat [ _1; _2; _3 ]
		  in
		  bundle bytes 32 (Data 0) |> List.map store_word |> Array.concat
		in
		let _7 = push (line_lookup "app") in
		let _8 =
		  [| (* ..., :caller, ~string, !args, !locals, !local_index, ~head, !memory, :app *)
		    SWAP3;
		    SWAP6;
		    SWAP2;
		    SWAP1;
		    SWAP2; (* ..., :caller, !local_index, !args, !locals, :app, !memory, ~head, ~string *)
		    PC;
		    PUSH1;
		    Data 6;
		    ADD;
		    SWAP4;
		    JUMP;
		    JUMPDEST; (* ..., :caller, !local_index, !args, !locals, !memory, ~head *)
		    SWAP1;
		    SWAP4;
		    SWAP1;
		  |]
		in
		Array.concat [ _1; _2; _3; _4; _5; _6; _7; _8; bodyc body ]
	      in

	      ops	      

	   | [] ->
	      [| (* ..., :caller, !memory, !args, !locals, !local_index, ~ref *)
		SWAP1;
		PUSH1;
		Data 32;
		MUL;
		DUP3;
		ADD;
		MSTORE;
	      |]

	   | _ ->
	      let _ = print_string "matched primitive or global in body of application (not yet supported)" in
	      let _ = flush stdout in
	      [||]
	 in


	 
	 (* 
	    Each application within a table maintains the following stack invariant:
	    
	    [ ..., :caller, :caller, !args, ~locals ]

	    Where !memory is the location in memory pointing to the next area 
	    of free memory

	 *)
	 let rec subc instrs local_index dep_ops ops = 
	   match instrs with
	   | Ap(Local(i), body) :: instrs ->
	      let deps =
		match body with
		| [] -> ["copy"]
		| _ -> ["app";"copy"]
	      in
	      let dep_ops = dependencies_get dep_ops deps compile in
	      let ops =
		let _1 =
		  [| (* :caller, !memory, !args, !locals *)
		    DUP1
		  |]
		in
		let _2 = push i in
		let _3 =
		  [|
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD;
		    MLOAD; (* :caller, !memory, !args, !locals, ~ref *)
		    SWAP1;
		    SWAP3; (* :caller, !locals, !args, ~ref, !memory *)
		  |]
		in
		let _4 = push (line_lookup "copy") in
		let _5 =
		  [|
		    SWAP2;
		    PC;
		    PUSH1;
		    Data 6;
		    ADD;
		    SWAP3;
		    JUMP;
		    JUMPDEST; (* :caller, !locals, !args, !memory, ~ref, ~ref_copy *)
		    SWAP2;
		    SWAP4;
		    SWAP2;
		  |]
		in
		let _6 = push local_index in
		let _7 =
		  [| (* ..., :caller, !memory, !args, !locals, ~ref, ~ref_copy, !local_index *)
		    SWAP2;
		    POP;
		  |]
		in
		Array.concat [ ops; _1; _2; _3; _4; _5; _6; _7; bodyc body ]
	      in
	      subc instrs (local_index + 1) dep_ops ops  
		
	   | Ap(Arg(i), body) :: instrs ->
	      let deps =
		match body with
		| [] -> ["copy"]
		| _ -> ["app";"copy"]
	      in
	      let dep_ops = dependencies_get dep_ops deps compile in
	      let ops =
		let _1 =
		  [|
		    DUP2
		  |]
		in
		let _2 = push i in
		let _3 =
		  [|
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD;
		    MLOAD;
		    SWAP1;
		    SWAP3;
		  |]
		in
		let _4 = push (line_lookup "copy") in
		let _5 =
		  [|
		    SWAP2;
		    PC;
		    PUSH1;
		    Data 6;
		    ADD;
		    SWAP3;
		    JUMP;
		    JUMPDEST;
		    SWAP2;
		    SWAP4;
		    SWAP2;
		  |]
		in
		let _6 = push local_index in
		let _7 =
		  [|
		    SWAP2;
		    POP;
		  |]
		in

		Array.concat [ ops; _1; _2; _3; _4; _5; _6; _7; bodyc body ]
	      in
	      subc instrs (local_index + 1) dep_ops ops

	   | Ap(Literal(i), body) :: instrs ->
	      let deps =
		match body with
		| [] -> []
		| _ -> ["app"]
	      in
	      let dep_ops = dependencies_get dep_ops deps compile in
	      let ops =
		let _1 =
		  [| (* :caller, !memory, !args, !locals *)
		    DUP3; (* ..., :caller, !memory, !args, !locals, !memory *)
		    PUSH1;
		    Data 1;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		  |]
		in
		let _2 = push i in
		let _3 =
		  [| 
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		    SWAP3; (* ..., :caller, !memory, !args, !locals, ~lit *)
		  |]
		in
		let _4 = push local_index in
		let _5 =
		  [|
		    SWAP1;
		  |]
		in

		Array.concat [ ops; _1; _2; _3; _4; _5; bodyc body ]
	      in
	      subc instrs (local_index + 1) dep_ops ops

	   | Ap(Hstring(n), body) :: instrs ->
	      let deps =
		match body with
		| [] -> []
		| _ -> ["app"]
	      in
	      let dep_ops = dependencies_get dep_ops deps compile in	      
	      let data c = Data (int_of_char c) in
	      let bytes =
		unquote n |> charlistof |> List.map data |> Array.of_list
	      in
	      let wordsize = 2 + (Array.length bytes / 32) in
	      let ops =
		let _1 =
		  [| (* :caller, !memory, !args, !locals *)
		    DUP3; (* :caller, !memory, !args, !locals, !memory *)
		  |]
		in		  
		let _2 = push wordsize in
		let _3 =
		  [| (* :caller, !memory, !args, !locals, !memory, !wordsize *)
		    PUSH6;
		    Data 1;
		    Data 0;
		    Data 0;
		    Data 0;
		    Data 1;
		    Data 0;
		    MUL;
		    PUSH1;
		    Data 2;
		    ADD;
		    DUP2;
		    MSTORE; (* :caller, !memory, !args, !locals, !memory *)
		    PUSH1;
		    Data 32;
		    ADD;
		    PUSH1;
		    Data 32;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		  |]
		in
		let _4 = push (Array.length bytes) in
		let _5 =
		  [|
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		  |]
		in
		let _6 =
		  let store_word word =
		    let _1 = [| pushx (Array.length word) |] in
		    let _2 = word in
		    let _3 =
		      [|
			DUP2;
			MSTORE;
			PUSH1;
			Data 32;
			ADD;
		      |]
		    in
		    Array.concat [ _1; _2; _3 ]
		  in
		  bundle bytes 32 (Data 0) |> List.map store_word |> Array.concat
		in
		let _7 =
		  [| (* :caller, ~string, !args, !locals, !memory *)
		    SWAP3;
		  |]		  
		in
		let _8 = push local_index in
		let _9 =
		  [|
		    SWAP1;
		  |]
		in
		Array.concat [ ops; _1; _2; _3; _4; _5; _6; _7; _8; _9; bodyc body ]
	      in

	      subc instrs (local_index + 1) dep_ops ops
		
	   | Ap(Primitive(p), body) :: instrs ->
	      let deps =
		match body with
		| [] -> [p]
		| _ -> ["app";p]
	      in
	      let dep_ops = dependencies_get dep_ops deps compile in
	      let ops =
		let _1 = [| DUP3 |] in
		let _2 = push (primarity p) in
		let _3 = push 0x10000000000 in
		let _4 =
		  [|
		    MUL;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD
		  |]
		in
		let _5 = push (line_lookup p) in
		let _6 =
		  [|
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD
		  |]
		in
		let _7 = push (primarity p) in
		let _8 =
		  [|
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD;
		    SWAP3;
		  |]
		in
		let _9 = push local_index in
		let _10 =
		  [|
		    SWAP1;
		  |]
		in

		Array.concat [ ops; _1; _2; _3; _4; _5; _6; _7; _8; _9; _10; bodyc body ]
	      in
	      subc instrs (local_index + 1) dep_ops ops
		
	   | Ap(Global(g), body) :: instrs ->
	      let deps =
		match body with
		| [] -> [g]
		| _ -> ["app";g]
	      in
	      let dep_ops = dependencies_get dep_ops deps compile in
	      let Table(_, numargs, _) = IMap.find g table_uplook in
	      let ops =
		let _1 =
		  [|
		    DUP3
		  |]
		in
		let _2 = push numargs in
		let _3 =
		  [|
		    PUSH6;
		    Data 1;
		    Data 0;
		    Data 0;
		    Data 0;
		    Data 0;
		    Data 0;
		    MUL;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD; (* ..., :caller, !memory, !args, !locals, !memory+1 *) 
		  |]
		in
		let _4 = push (line_lookup g) in
		let _5 =
		  [|
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		  |]
		in		
		let _6 = push numargs in
		let _7 =
		  [|
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD; (* ..., :caller, ~ref, !args, !locals, !memory *)
		    SWAP3; (* ..., :caller, !memory, !args, !locals, ~ref *)
		  |]
		in
		let _8 = push local_index in
		let _9 =
		  [|
		    SWAP1;
		  |]
		in

		Array.concat [ ops; _1; _2; _3; _4; _5; _6; _7; _8; _9; bodyc body ] 
	      in
	      subc instrs (local_index + 1) dep_ops ops
		
	   | Return(Local(i)) :: instrs ->
	      let dep_ops = dependencies_get dep_ops ["eval"] compile in
	      let ops =
		let _1 = push i in
		let _2 =
		  [| (* :caller, !memory, ~args, ~locals, !i *)
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD;
		    MLOAD;
		    SWAP1;
		    POP
		  |]
		in
		let _3 = push (line_lookup "eval") in
		let _4 = [| JUMP |] in
		
		Array.concat [ ops; _1; _2; _3; _4 ] 
	      in
	      dep_ops, ops
		
	   | Return(Arg(i)) :: instrs ->
	      
	      let dep_ops = dependencies_get dep_ops ["eval"] compile in
	      let ops =
		let _1 =
		  [| (* ..., :caller, !memory, !args, !locals *)
		    POP;
		  |]
		in
		let _2 = push i in
		let _3 =
		  [|
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD;
		    MLOAD;
		  |]
		in
		let _4 = push (line_lookup "eval") in
		let _5 =
		  [|
		    JUMP
		  |]
		in
		Array.concat [ ops; _1; _2; _3; _4; _5 ]
	      in
	      dep_ops, ops


	   | Return(Literal(i)) :: instrs ->
	      (* ..., :caller, !memory, !args, !locals *)
	      let ops =
		let _1 =
		  [|
		    POP;
		    POP;
		    DUP1;
		    PUSH1; 
		    Data 1; (* ..., :caller, ~lit, !memory, !is_literal *)
		    DUP2; 
		    MSTORE; (* ..., :caller, ~lit, !memory *)
		    PUSH1;
		    Data 32;
		    ADD; (* ..., :caller, ~lit, !memory *)
		  |]
		in
		let _2 = push i in
		let _3 =
		  [|
		    DUP2; 
		    MSTORE; (* ..., :caller, ~lit, !memory *)
		    PUSH1;
		    Data 32;
		    ADD; (* ..., :caller, ~lit, !memory *)
		    SWAP2;
		    JUMP;
		  |]		
		in
		Array.concat [ ops; _1; _2; _3 ]
	      in
	      dep_ops, ops

	   | Return(Hstring s) :: instrs ->
	      let data c = Data (int_of_char c) in
	      let bytes =
		unquote s |> charlistof |> List.map data |> Array.of_list
	      in
	      let wordsize = 2 + (Array.length bytes / 32) in
	      let ops =
		let _1 =
		  [| (* :caller, !memory, !args, !locals *)
		    POP;
		    POP;
		    DUP1; 
		  |]
		in
		let _2 = push wordsize in 
		let _3 =
		  [| (* :caller, !memory, !memory, !wordsize *)
		    PUSH6;
		    Data 1;
		    Data 0;
		    Data 0;
		    Data 0;
		    Data 1;
		    Data 0;
		    MUL;
		    PUSH1;
		    Data 2;
		    ADD;
		    DUP2;
		    MSTORE; 
		    PUSH1;
		    Data 32;
		    ADD; (* :caller, !memory, !memory+1 *)
		    PUSH1;
		    Data 32;
		    DUP2;
		    MSTORE; 
		    PUSH1;
		    Data 32;
		    ADD; (* :caller, !memory, !memory+2 *)
		  |]
		in
		let _4 = push (Array.length bytes) in
		let _5 =
		  [|
		    DUP2;
		    MSTORE;		    
		    PUSH1;
		    Data 32;
		    ADD; (* :caller, !memory, !memory+3 *)
		  |]
		in
		let _6 =
		  let store_word word =
		    let _1 = [| pushx (Array.length word) |] in
		    let _2 = word in
		    let _3 =
		      [|
			DUP2;
			MSTORE;
			PUSH1;
			Data 32;
			ADD;
		      |]
		    in
		    Array.concat [ _1; _2; _3 ]
		  in
		  bundle bytes 32 (Data 0) |> List.map store_word |> Array.concat
		in
		let _7 =
		(* ..., :caller, ~string, !memory *)
		  [|
		    SWAP2;
		    JUMP;
		  |]
		in
		Array.concat [ ops; _1; _2; _3; _4; _5; _6; _7 ]
	      in
	      dep_ops, ops

	   | Return (Global g) :: instrs ->
	      let deps = [ g ] in
	      let dep_ops = dependencies_get dep_ops deps compile in
	      let Table(_, numargs, _) = IMap.find g table_uplook in
	      let ops =
		let _1 =
		  [|
		    DUP3
		  |]
		in
		let _2 = push numargs in
		let _3 =
		  [|
		    PUSH6;
		    Data 1;
		    Data 0;
		    Data 0;
		    Data 0;
		    Data 0;
		    Data 0;
		    MUL;
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		  |]
		in
		let _4 = push (line_lookup g) in
		let _5 =
		  [|
		    DUP2;
		    MSTORE;
		    PUSH1;
		    Data 32;
		    ADD;
		  |]
		in
		let _6 = push numargs in
		let _7 =
		  [|
		    PUSH1;
		    Data 32;
		    MUL;
		    ADD; (* ..., :caller, ~ref, !args, !locals, !memory *)
		    SWAP3;
		    SWAP2;
		    POP;
		    POP;
		  |]
		in
		let _8 = push (line_lookup "eval") in
		let _9 = [| JUMP |] in
		Array.concat [ ops; _1; _2; _3; _4; _5; _6; _7; _8; _9 ]		
	      in
	      dep_ops, ops

	       
	   | [] ->
	      let _ = print_string ("no return statement found in compilation of " ^ goal ^ " --- undefined behavior") in
	      let _ = flush stdout in
	      let ops =
		let _1 = [| POP; POP; POP; JUMP |] in
		Array.concat [ ops; _1 ]
	      in
	      dep_ops, ops

	   | _ ->
	      let _ = print_string "matched a return of a primitive (not yet supported)" in
	      let _ = flush stdout in
	      let ops =
		let _1 = [| POP; POP; POP; JUMP |] in
		Array.concat [ ops; _1 ]
	      in
	      dep_ops, ops
	 in
	 let init =
	   let nlocals = nlocals instrs in
	   let ops =
	     let _1 =
	       [|
		 JUMPDEST;
		 DUP2;
	       |]
	     in
	     let _2 = push nlocals in
	     let _3 =
	       [|
		 PUSH1;
		 Data 32;
		 MUL;
		 ADD;
		 SWAP2;
	       |]
	     in
	     Array.concat [ _1; _2; _3]
	   in
	   ops
	 in
	 subc instrs 0 dep_ops init
    in
    let line = Array.length dep_ops in
    let _ = line_add goal line in
    Array.append dep_ops ops    
  in
  let ops = compile "main" [||] in
  let len = Array.length ops + headersize in
  let load_call =
    Array.concat
      [
	[| PUSH4 |];
	box (len + 6) (Some 4);
	[| JUMP |]
      ]
  in
  let loader =
    Array.concat
      [
	[| JUMPDEST |];
	push len;
	[| DUP1 |];
	push (Array.length load_call);
	[|
	  PUSH1;
	  Data 0;
	  CODECOPY;
	  PUSH1;
	  Data 0;
	  RETURN;
	|]
      ]
  in
  let header =
    let _2 =
	[|
	  PUSH1;
	  Data 0;
	  DUP1;
	  PC;
	  PUSH1;
	  Data 6;
	  ADD;
	  SWAP3;
	  JUMP;
	  JUMPDEST; (* !memory, ~ref *)
	  SWAP1;
	  POP;
	  DUP1;
	  MLOAD;
	  PUSH5;
	  Data 0xff;
	  Data 0xff;
	  Data 0xff;
	  Data 0xff;
	  Data 0;
	  AND; (* ~ref, !nargs *)
	  PUSH2;
	  Data 1;
	  Data 0;
	  SWAP1;
	  DIV;
	  PUSH1;
	  Data 1;
	  ADD;
	  PUSH1;
	  Data 32;
	  MUL;
	  SWAP1;
	  PUSH1;
	  Data 32;
	  ADD; 
	  RETURN;
	|]
    in
    let _1 =
      let fill = headersize - Array.length _2 - 1 in

      let _ =
	let size = box (line_lookup "main") None |> Array.length in
	if fill < size then
	  let _ = print_string ("Header overflowed, needs to be adjusted up by " ^ (string_of_int (size-fill))) in
	  ()
      in
      
      Array.concat
      [
	[| pushx fill |];
	box (line_lookup "main") (Some fill);
      ]
    in
    
    
    Array.concat [ _1; _2 ] 
  in
  
  Array.concat
    [
      load_call;
      header;
      ops;
      loader;
    ]
;;

let ethereum_human_readable_compile tables sources prefix =
  compile tables sources |> Array.to_list |> List.map strop |> cat "\n" |> print_string
;;

let ehrc = ethereum_human_readable_compile ;;

let web3_generate program prefix =
  let str =
    program |>
      Array.to_list |>
      List.map opcodes |>
      List.map hr_byte |>
      (fun bytes -> "0x" :: bytes) |>
      Array.of_list |>
      (fun bytes -> bundle bytes 42 "") |>
      List.map (Array.to_list) |>
      List.map (cat "") |>
      cat "' +\n\t  '"
  in

  let web3 = "
personal.unlockAccount(eth.coinbase, \"\")
var test_contract = web3.eth.contract(
  [
    {
      \"constant\":false,
      \"inputs\":[],
      \"name\":\"run\",
      \"outputs\":[{\"name\":\"\",\"type\":\"string\"}],
      \"payable\":false,
      \"type\":\"function\"
     }
   ]
);

var test = test_contract.new(
  {
    from: web3.eth.accounts[0],
    data: '" ^ str ^ "'
    gas: '4700000'
  },
  function (e, contract){
    console.log(e, contract);
    if (typeof contract.address !== 'undefined') {
      console.log('Contract mined! address: ' + contract.address + ' transactionHash: ' + contract.transactionHash);
  }
})

"
  in

  let outfilename = prefix ^ ".js" in
  let oc = open_out outfilename in
  let _ = output_string oc web3 in 
  let _ = close_out oc in
  let _ = print_string ("\twrote to " ^ outfilename ^ "\n") in
  ()
;;

let web3g tables sources prefix =
  web3_generate (compile tables sources) prefix
;;

let code = function
  | 0x0 -> STOP | 0x1 -> ADD | 0x2 -> MUL | 0x3 -> SUB | 0x4 -> DIV | 0x5 -> SDIV | 0x6 -> MOD | 0x7 -> SMOD | 0x8 -> ADDMOD | 0x9 -> MULMOD | 0xa -> EXP | 0xb -> SIGNEXTEND
  | 0x10 -> LT | 0x11 -> GT | 0x12 -> SLT | 0x13 -> SGT | 0x14 -> EQ | 0x15 -> ISZERO | 0x16 -> AND | 0x17 -> OR | 0x18 -> XOR | 0x19 -> NOT | 0x1a -> BYTE

  | 0x20 -> SHA3

  | 0x30 -> ADDRESS | 0x31 -> BALANCE | 0x32 -> ORIGIN | 0x33 -> CALLER | 0x34 -> CALLVALUE | 0x35 -> CALLDATALOAD | 0x36 -> CALLDATASIZE | 0x37 -> CALLDATACOPY | 0x38 -> CODESIZE | 0x39 -> CODECOPY | 0x3a -> GASPRICE | 0x3b -> EXTCODESIZE | 0x3c -> EXTCODECOPY

  | 0x40 -> BLOCKHASH | 0x41 -> COINBASE | 0x42 -> TIMESTAMP | 0x43 -> NUMBER | 0x44 -> DIFFICULTY | 0x45 -> GASLIMIT

  | 0x50 -> POP | 0x51 -> MLOAD | 0x52 -> MSTORE | 0x53 -> MSTORE8 | 0x54 -> SLOAD | 0x55 -> SSTORE | 0x56 -> JUMP | 0x57 -> JUMPI | 0x58 -> PC | 0x59 -> MSIZE | 0x5a -> GAS | 0x5b -> JUMPDEST
  
  | 0x60 -> PUSH1 | 0x61 -> PUSH2 | 0x62 -> PUSH3 | 0x63 -> PUSH4 | 0x64 -> PUSH5 | 0x65 -> PUSH6 | 0x66 -> PUSH7 | 0x67 -> PUSH8 | 0x68 -> PUSH9 | 0x69 -> PUSH10 | 0x6a -> PUSH11 | 0x6b -> PUSH12 | 0x6c -> PUSH13 | 0x6d -> PUSH14 | 0x6e -> PUSH15 | 0x6f -> PUSH16
  | 0x70 -> PUSH17 | 0x71 -> PUSH18 | 0x72 -> PUSH19 | 0x73 -> PUSH20 | 0x74 -> PUSH21 | 0x75 -> PUSH22 | 0x76 -> PUSH23 | 0x77 -> PUSH24 | 0x78 -> PUSH25 | 0x79 -> PUSH26 | 0x7a -> PUSH27 | 0x7b -> PUSH28 | 0x7c -> PUSH29 | 0x7d -> PUSH30 | 0x7e -> PUSH31 | 0x7f -> PUSH32

  | 0x80 -> DUP1 | 0x81 -> DUP2 | 0x82 -> DUP3 | 0x83 -> DUP4 | 0x84 -> DUP5 | 0x85 -> DUP6 | 0x86 -> DUP7 | 0x87 -> DUP8 | 0x88 -> DUP9 | 0x89 -> DUP10 | 0x8a -> DUP11 | 0x8b -> DUP12 | 0x8c -> DUP13 | 0x8d -> DUP14 | 0x8e -> DUP15 | 0x8f -> DUP16

  | 0x90 -> SWAP1 | 0x91 -> SWAP2 | 0x92 -> SWAP3 | 0x93 -> SWAP4 | 0x94 -> SWAP5 | 0x95 -> SWAP6 | 0x96 -> SWAP7 | 0x97 -> SWAP8 | 0x98 -> SWAP9 | 0x99 -> SWAP10 | 0x9a -> SWAP11 | 0x9b -> SWAP12 | 0x9c -> SWAP13 | 0x9d -> SWAP14 | 0x9e -> SWAP15 | 0x9f -> SWAP16

  | 0xa0 -> LOG0 | 0xa1 -> LOG1 | 0xa2 -> LOG2 | 0xa3 -> LOG3 | 0xa4 -> LOG4

  | 0xf0 -> CREATE | 0xf1 -> CALL | 0xf2 -> CALLCODE | 0xf3 -> RETURN | 0xf4 -> DELEGATECALL | 0xf5 -> INVALID | 0xf6 -> SELFDESTRUCT

  | _ -> STOP

;;

let xpush = function
  | PUSH1 -> 1
  | PUSH2 -> 2
  | PUSH3 -> 3
  | PUSH4 -> 4
  | PUSH5 -> 5
  | PUSH6 -> 6
  | PUSH7 -> 7
  | PUSH8 -> 8
  | PUSH9 -> 9
  | PUSH10 -> 10
  | PUSH11 -> 11
  | PUSH12 -> 12
  | PUSH13 -> 13
  | PUSH14 -> 14
  | PUSH15 -> 15
  | PUSH16 -> 16
  | PUSH17 -> 17
  | PUSH18 -> 18
  | PUSH19 -> 19
  | PUSH20 -> 20
  | PUSH21 -> 21
  | PUSH22 -> 22
  | PUSH23 -> 23
  | PUSH24 -> 24
  | PUSH25 -> 25
  | PUSH26 -> 26
  | PUSH27 -> 27
  | PUSH28 -> 28
  | PUSH29 -> 29
  | PUSH30 -> 30
  | PUSH31 -> 31
  | _ -> 32
;;

let byte_of_hex arr =
  arr.(0) * 16 + arr.(1)
;;

let disassemble hex =
  hex
  |> charlistof |> List.map Char.escaped |> List.map (fun x -> "0x" ^ x) 
  |> List.map int_of_string |> Array.of_list |> (fun arr -> bundle arr 2 0) |> List.map (fun x -> code (byte_of_hex x))
  |> List.map strop |> cat "\n" |> print_string
;;


(* ------------------------------------------------------------------------------------------------------------------*)
(*                                                aero.ml main execution                                              *)
(* ------------------------------------------------------------------------------------------------------------------*)
let dowith x = fun tables sources prefix -> x tables sources prefix ;;

let rec args =
  ("help", "Print out the various available options",
   fun tables sources prefix -> print_help ())
  ::
    ("evm", "Write out human-readable EVM code to .evm file",
     dowith ehrc)
  ::
    ("web3", "Write out EVM contract creation to a .js file",
     dowith web3g)
  ::
    ("cem", "Write out the immediate representation to a .cem file",
     dowith writeprogram)
  ::
    ("calc", "Simulate the computation using lambda calculus",
     dowith simulate)
  (*  ::
      ("--dis", "Dissasemble EVM bytecode",
      fun () -> disassemble hex) *)

  :: []
    
and print_help () =
    List.iter
      (fun (parameter, description, _) ->
	let _ = print_string parameter in
	let _ = print_string "\n\t" in
	let _ = print_string description in
	print_string "\n"
      )
      (List.tl args)

;;

let parameter_map =
  List.fold_left
    (fun map (parameter, description, execution) ->
      IMap.add parameter (description, execution) map)
    emptymap
    args
;;

let process_arg tables sources prefix arg =
  try
    let  _, exe = IMap.find arg parameter_map in
    exe tables sources prefix 
  with
    _ -> ()
;;

let shellify cs =
  cs
  |> List.rev      
  |> rescape
  |> wstrim
  |> stringof
;;

let filename () =
  if Array.length (Sys.argv) > 1 && Sys.file_exists (Sys.argv.(1)) then
    Sys.argv.(1)
  else
    "lib.ml"
;;

let str_file filename = 
  let ic = open_in filename in
  let iclen = in_channel_length ic in
  let contents = Bytes.create iclen in
  let _ = really_input ic contents 0 iclen in
  let _ = close_in ic in
  contents
;;

let ir cs sources tables =
  cs
  |> parse
  |> fst
  |> prep
  |> apply_rewrites []
  |> (tableau sources tables)
;;

let scratch cs =
  ir cs emptymap []  
;;

let scratch_file filename = 
  str_file filename |> charlistof |> scratch
;;
  
let try_parse_singular_let s =
  match parse s |> snd with
  | Token TLet :: Tree(Bool isrec) :: Tree(String name) :: Tree(Term subterm) :: Token In :: [] ->
     Some ( Let (isrec, name, [], subterm, Fin name) )

  | Token TLet :: Tree(Bool isrec) :: Tree(Stringlist sl) :: Tree(Term subterm) :: Token In :: [] ->
     Some ( Let (isrec, List.hd sl, List.tl sl, subterm, Fin (List.hd sl)) )
     
  | _ ->
     None     
;;

let some = function
  | Some x -> x
  | _ -> failwith "is something"

let preamble () = 
  print_string ("
\t~

\taero.ml version " ^ version ^ "

\tgnu licence 2.0

\tREADME.ml for details

\t~

\twelcome to aero

\t~


")
;;

let help () =
  print_string "
help 
create

"
;;

let prompt () =
  print_string "(aero)> "
;;

let optional_cone x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs
;;

let aero () = 
  let _ = preamble () in
  let _ = prompt () in
  let _ = flush stdout in
  
  let rec aero cs tables sources main =
    try
      orea ( input_char stdin :: cs ) tables sources main
    with
      _ ->
	false
	  
  and orea cs tables sources main =
    match shellify cs with
    | x when IMap.mem x parameter_map ->
       let _ = process_arg (optional_cone main tables) sources "~" x in
       let _ = prompt () in
       let _ = flush stdout in
       aero [] tables sources main

    | x when x = itacsuf ->
       let _ = print_string obfuscation in
       let _ = prompt () in
       let _ = flush stdout in
       aero [] tables sources main
       
       
    | "show c" | "show w" | "aero.ml" | "aero" | "aero ()" ->
       let _ = preamble () in
       let _ = prompt () in
       let _ = flush stdout in
       aero [] tables sources None
	 
    | "help" ->
       let _ = help () in
       let _ = prompt () in
       let _ = flush stdout in
       aero [] tables sources main
	 
    | "create" ->
       let _ = print_string "contract creation not yet supported in aero\n" in
       let _ = prompt () in
       let _ = flush stdout in
       aero [] tables sources main

    | "exit" | "quit" ->
       true
	 
    | x ->
       match cs with
       | '\n' :: '\n' :: '\n' :: cs -> (
	 let _ = print_string "\n\theed not .. \n\n" in
	 let _ = prompt () in
	 let _ = flush stdout in
	 aero [] tables sources main
       )

       | '\n' :: _ -> (
	 try
	   let tables, sources = (
	     try
	       ir (List.rev cs) sources tables
	     with 
		 (* BUG --- need to remember declared rewrites *)
	       _ -> (
		 try_parse_singular_let (List.rev cs)
			|> some
			|> (tableau sources tables)
	       ))
	   in
	   let lambda = calculate tables in
	   let _ = print_string ("\n\t: " ^ lchr lambda) in
	   let _ = print_string "\n\n" in
	   let _ = prompt () in
	   let _ = flush stdout in
	   aero [] (List.tl tables) sources (Some (List.hd tables))
	 with
	   _ ->
	     aero cs tables sources main
       )
		
       | _ ->
	  aero cs tables sources main
  in


  match aero [] [] emptymap None with
  | true ->
     ()

  | false ->
     let _ = print_string "aborting\n" in
     let _ = flush stdout in
     ()
;;

match Array.length (Sys.argv) with
| x when x < 2 -> aero ()
| _ ->
   let prefix = () |> filename |> prefix in
   begin
     () |> filename |> str_file |> charlistof |> scratch |>
	 fun (tables, sources) -> 
	   Array.iter
	     (fun arg -> process_arg tables sources prefix arg)
	     Sys.argv
   end
(* 

known bugs:  

if arguments or locals are greater than 32 bit unsigned int max, need to adjust data size

if program size exceeds size addressable by 32 bit unsigned it, load_call needs to be adjusted




todo:

pattern matching

argument pruning

inline parsing

native proof

garbage collection

whitepaper
  
*)
