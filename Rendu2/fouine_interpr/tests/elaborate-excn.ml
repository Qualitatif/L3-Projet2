let a = ref (E 1) ;;
let f x = if x < 0 then -x else raise !a ;;
try f 1 with E x -> prInt x ;;
