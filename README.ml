let [] [] cons = [] in

let cons h t [] cons = cons h t in

let reverse l =
  let rec reverse l acc =
    let [] = acc in
    let cons h t = reverse t ( cons h acc ) in
    l [] cons
  in
  reverse l []
in

let rec cat delimiter list =
  let [] = "" in
  let cons h t =
    let nil = h in
    let cons h' t' =
      h ^ delimiter ^ ( cat delimiter t )
    in
    t nil cons
  in
  list [] cons
in

let pair x y sel = sel x y in

let ~,~ = pair in




let ava = "A --- dialectically ML --- Programming Language" in

let supported_platforms = cons "Ethereum Virtual Machine" [] in

let dependencies = cons "ubuntu" ( cons "ocaml" ( cons "git" [] ) ) in

let recommended_dependencies = cons "emacs" ( cons "tuareg" [] ) in

let how_to_install = cons ( 1 , "sudo apt-get install " ^ ( cat " " dependencies ) ^ " " ^ ( cat " " recommended_dependencies ) ) ( cons ( 2 , "git clone https://github.com/yithump/ava" ) [] ) in

let how_to_run = cons ( "Ethereum Backend" , "ocaml ava.ml file.ml --web3" ) [] in

ava
