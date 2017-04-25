let [] n c = n in
let ~::~ h t n c = c h t in

rewrite ( ~ :: ~ ) :: ~ -> ~ :: ( ~ :: ~ ) in

let false f t = f in
let true f t = t in
let str_bool bool =
  bool "false" "true"
in

let rec str_list str list =
  let [] = "[]" in
  let cons h t =
    ( str h ) ^ " :: " ^ ( str_list str t )
  in
  list [] cons
in

let not bool = bool true false in

let rec map f list =
  let [] = [] in
  let cons h t =
    ( f h ) :: ( map f t )
  in
  list [] cons
in

str_list str_bool ( map not ( false :: true :: false :: true :: false :: true :: [] ) )

