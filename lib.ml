let false t f = f in
let true t f = t in

let str_bool bool =
  bool "true" "false"
in

let not bool = bool false true in

let [] ~::~ [] = [] in
let ~::~ h t ~::~ [] = h :: t in
rewrite ( ~ :: ~ ) :: ~ → ~ :: ( ~ :: ~ ) in

let rec str_list str list =
  let [] = "[]" in
  let ~::~ h t =
    ( str h ) ^ " :: " ^ ( str_list str t )
  in
  list ~::~ []
in

let rec map f list =
  let [] = [] in
  let ~::~ h t =
    ( f h ) :: ( map f t )
  in
  list ~::~ []
in

let ~|>~ x y = y x in


str_bool true

~~~~~~

  ( true :: false :: true :: false :: true :: false :: true :: false :: true :: [] )
|> ( map not )
|> ( str_list str_bool )
