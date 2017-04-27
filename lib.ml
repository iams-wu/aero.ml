let false true false = false in
let true true false = true in

let [] :: [] = [] in
let ~::~ h t ~::~ [] = h :: t in

rewrite ( ~ :: ~ ) :: ~ → ~ :: ( ~ :: ~ ) in

true :: false :: true :: []


