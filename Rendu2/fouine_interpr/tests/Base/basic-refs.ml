let x = ref 5
let y = ref x
let _ = x := !x * 2;;
prInt (!x + !(!y))
