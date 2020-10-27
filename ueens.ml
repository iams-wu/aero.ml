let false false true = false in
let true false true = true in
let str_bool bool =
  bool "false" "true"
in
let zero zero suc = zero in

let suc n zero suc =
  suc n
in

let rec gt n m =
  let zero z s = false in
  let suc n =
    let zero z s = true in
    let suc m = gt n m in
    m zero suc
  in
  n zero suc
in

let rec peanoadd x y =
  let zero = y in
  let suc q = peanoadd q ( suc y ) in
  x zero suc
in

let one = suc zero in
let two = suc ( suc zero ) in

let not b =
  b true false
in

let xor b1 b2 =
  b1 true ( not b2 )
in

let id x = x in
let i x = x in
let rec equals a b =
  let zero =
    let zero = true in
    let suc b = false in
    b zero suc
  in
  let suc a =
    let zero = false in
    let suc b = equals a b in
    b zero suc
  in
  a zero suc
in

let pred n = 
  let zero = zero in
  let suc n = n in
  n zero suc
in

let peanomult x y =
  let rec peanomult a b = 
    let zero = b in
    let suc a = 
      peanomult a ( peanoadd y b ) 
    in
    a zero suc
  in
  peanomult x zero
in

let three = suc two in
let four = suc three in
let five = suc four in
let six = suc five in
let seven = suc six in
let eight = suc seven in
let nine = suc eight in
let ten = suc nine in
let eleven = suc ten in
let twelve = suc eleven in
let thirteen = suc twelve in
let fourteen = suc thirteen in
let fifteen = suc fourteen in
let sixteen = suc fifteen in
let seventeen = suc sixteen in
let eighteen = suc seventeen in
let nineteen = suc eighteen in
let twenty = suc nineteen in
let twentyone = suc twenty in
let twentytwo = suc twentyone in
let twentythree = suc twentytwo in
let twentyfour = suc twentythree in
let twentyfive = suc twentyfour in
let twentysix = suc twentyfive in
let twentyseven = suc twentysix in
let twentyeight = suc twentyseven in
let twentynine = suc twentyeight in
let thirty = suc twentynine in
let thirtyone = suc thirty in
let thirtytwo = suc thirtyone in
let thirtythree = suc thirtytwo in
let thirtyfour = suc thirtythree in
let thirtyfive = suc thirtyfour in
let thirtysix = suc thirtyfive in
let thirtyseven = suc thirtysix in
let thirtyeight = suc thirtyseven in
let thirtynine = suc thirtyeight in
let forty = suc thirtynine in
let fortyone = suc forty in
let fortytwo = suc fortyone in

let rec fact n =
  let zero = one in
  let suc nm1 =
    peanomult n ( fact nm1 )
  in
  n zero suc 
in

let rec fib n =
  let zero = zero in
  let suc n =
    let zero = one in
    let suc m = peanoadd ( fib n ) ( fib m ) in
    n zero suc
  in
  n zero suc
in

let nil nil cons = nil in

let cons head tail nil cons = cons head tail in

let choose n k =
  let rec choose m j b =
    let false = 
      let b = choose ( suc m ) ( suc j ) b in
      let b = choose ( suc m ) j b in
      b
    in
    let true = 
      let false = b in
      let true = suc b in
      ( equals k j ) false true
    in
    ( equals n m ) false true
  in
  choose zero zero zero
in

let pred n = 
  let zero = zero in
  let suc n = n in
  n zero suc
in

let choose n k = 
  let rec choose m j b = 
    let zero = 
      let zero = suc b in
      let suc j = b in
      j zero suc
    in
    let suc m =
      let zero = suc b in
      let suc jm = 
	let b = choose m jm b in
	choose m j b      
      in
      j zero suc
    in
    m zero suc
  in
  choose n k zero
in

let rec listinit n defval =
  let zero = nil in
  let suc n = 
    cons defval ( listinit n defval )
  in
  n zero suc
in

let rec set list index value =
  let nil = nil in
  let cons head tail =
    let zero = cons value tail in
    let suc index =
      cons head ( set tail index value )
    in
    index zero suc
  in
  list nil cons
in

let queens n =
  let rec queens rboard sols =
    let queensnil = suc sols in
    let queenscons col rboard = 
      let rec branch col sols index =
	let branchnil = sols in
	let branchcons c col =
	  let branchfalse = 
	    let rec future dltz dec horiz inc rboard =
	      let futurenil = nil in
	      let futurecons col rboard =
		let false =
		  let col = set col dec true in
		  let col = set col horiz true in
		  let col = set col inc true in
		  let dltz = equals zero dec in
		  let rboard = future dltz ( pred dec ) horiz ( suc inc ) rboard in
		  cons col rboard
		in
		let true =
		  let col = set col horiz true in
		  let col = set col inc true in
 		  let rboard = future true ( pred dec ) horiz ( suc inc ) rboard in
		  cons col rboard
		in
		dltz false true
	      in
	      rboard futurenil futurecons	      
	    in
	    let rboard = future ( equals zero index ) ( pred index ) index ( suc index ) rboard in
	    let sols = queens rboard sols in
	    branch col sols ( suc index )
	  in
	  let branchtrue = branch col sols ( suc index ) in
	  c branchfalse branchtrue
	in
	col branchnil branchcons
      in
      branch col sols zero
    in
    rboard queensnil queenscons
  in
  queens ( listinit n ( listinit n false ) ) zero
in

eq (queens five) ten

~~~~~~
$ ocaml aero.ml ueens.ml calc
true
-reductions-18744

~~~~~~
$ calc ( gt ( queens five ) ( queens six ) )
true
-reductions-(ran out of memory)


