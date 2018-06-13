let f x = raise (E x)
let k = try f 5 with E x -> x + 1 ;;
prInt k
