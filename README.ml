~~~
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



  ~~~
(*

~

example --- running in bash / geth

~
 
(bash)$ ocaml aero.ml example.ml web3  
	wrote to example.js

(geth)$ loadScript example.js

   ...

(geth)$ test.run.call ()

   ...

~
  
example --- running interactively 

~

(aero)> let false ff tt = ff in

        : ll|~


(aero)> let true ff tt = tt in 

	: ll||~


(aero)> let and x y = x false y in

	: llaa|~ll|~||~


(aero)> and false true

	: aallaa|~ll|~||~ll|~ll||~


(aero)> web3
        wrote to ~.js                       

*)
