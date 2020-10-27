,~~~
(*
  
aero.ml

This program, hereby known as "the Program" is free software; 
you can redistribute it and/or modify it under the terms of the 
GNU General Public License Version 2 published in June 1991
  
This program is distributed in the hope that it will be useful, 
but without any warranty; without even the implied warranty of 
merchantability or fitness for a particular purpose.
	    
see LICENSE for more details
	    
contact @yithump
	    
copyleft -- april 2017    
 
*)

  ~~~
    
let [] [] ~::~ = [] in

let ~::~ h t [] ~::~ = h :: t in

let reverse l =
  let rec reverse l acc =
    let [] = acc in
    let ~::~ h t = reverse t ( h :: acc ) in
    l [] ~::~
  in
  reverse l []
in

let rec cat delimiter list =
  let [] = "" in
  let ~::~ h t =
    let [] = h in
    let ~::~ h' t' =
      h ^ delimiter ^ ( cat delimiter t )
    in
    t [] ~::~
  in
  list [] ~::~
in

let pair x y sel = sel x y in

let ~,~ = pair in


 

let aero = "A --- dialectically ML --- Programming Language" in

let supported_platforms = "Ethereum Virtual Machine" :: [] in

let dependencies = "ocaml" :: "git" :: [] in

let recommended_dependencies = "emacs" :: "tuareg" :: [] in

let how_to_install =
  1 , "sudo apt-get install " ^ ( cat " " dependencies ) ^ ( cat " " recommended_dependencies ) ::
    2 , "git clone https://github.com/yithump/aero.ml" ::
      []
in

let how_to_run =
  "static compilation" ,
   "ocaml aero.ml file.ml web3"
  ::
    "interactive",
     "ocaml aero.ml"
  ::
    []
in

aero

~~~~~~
wad.js

let output = Unix.system("ocaml aero.ml ueens.ml web3");
let conf = output == "wrote to ueens.js";
let output' = loadScript("ueens.js");
let conf' = output.includes("Contract mined!");                       
let pub = eth.getTransactionReceipt(test.transactionHash).contractAdress;
test_contract.at(pub).run();



