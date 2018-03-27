open Expr

type valeur = VI of int | VF of (string*expr)

let rec eval_var var env =
  match env with
  | [] -> failwith "Error: variable not declared"
  | (str,value)::r -> if str=var then value else eval_var var r

let rec eval e env =
  match e with
  | Const(k) -> VI k
  | Var(str) -> eval_var str env
  | Add(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> VI (k1+k2)
  				  			   | VF(f,e) -> failwith "TypeError: Add of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Add of functions")
  | Mul(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> VI (k1*k2)
  				  			   | VF(f,e) -> failwith "TypeError: Mul of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Mul of functions")
  | Sub(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> VI (k1-k2)
  				  			   | VF(f,e) -> failwith "TypeError: Sub of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Sub of functions")
  | Let(var,e1,e2) -> (match var with
  					  |Var(str) -> (let v1 = eval e1 env in match v1 with
  					  										| VI(_) -> eval e2 ((str,v1)::env)
  					  										| VF(_) -> VI 0)
  					  |_ -> failwith "Error: Incorrect name of variable")
  | Let_anon(e1,e2) -> eval e2 (eval_anon e1 env)
  | Ite(c,e1,e2) -> if eval_cond c env then eval e1 env else eval e2 env
  | PrInt(e) -> let v = eval e env in (match v with
  									   | VI(k) -> let _ = (print_int(k);print_string(";\n")) in v
  									   | VF(x,e) -> let _ = print_string("fonction...;\n") in v)
  | Semis(e1,e2) -> (eval e1 env ; eval e2 env)
  | Fun(x,e) -> VF (x,e) 
  | App(e1,e2) -> (let v = eval e2 env in  
      let f = eval e1 env in
      match f with
      | VF(x,e) -> eval e ((x,v)::env)
      | VI _ -> failwith "Error: Incorrect name of function")
  
and eval_cond c env =
  match c with
  | True -> true
  | False -> false
  | Le(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> k1<=k2
  				  			   | VF(f,e) -> failwith "TypeError: Comaprison of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Comparison of functions")
  | Lt(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> k1<k2
  				  			   | VF(f,e) -> failwith "TypeError: Comaprison of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Comparison of functions")
  | Ge(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> k1>=k2
  				  			   | VF(f,e) -> failwith "TypeError: Comaprison of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Comparison of functions")
  | Gt(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> k1>k2
  				  			   | VF(f,e) -> failwith "TypeError: Comaprison of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Comparison of functions")
  | Eq(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> k1==k2
  				  			   | VF(f,e) -> failwith "TypeError: Comaprison of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Comparison of functions")
  | Neq(e1,e2) -> (let v1 = (eval e1 env) in 
  				  let v2 = (eval e2 env) in
  				  match v1 with
  				  | VI(k1) -> (match v2 with
  				  			   | VI(k2) -> k1<>k2
  				  			   | VF(f,e) -> failwith "TypeError: Comaprison of int and function")
  				  | VF(f1,e) -> failwith "TypeError: Comparison of functions")
  | And(c1,c2) -> (eval_cond c1 env) && (eval_cond c2 env)
  | Or(c1,c2) -> (eval_cond c1 env) || (eval_cond c2 env)
  | Not(c) -> not (eval_cond c env)
  
and eval_anon e env =
  match e with
  | Const(k) -> env
  | Var(str) -> env
  | Add(e1,e2) -> env
  | Mul(e1,e2) -> env
  | Sub(e1,e2) -> env
  | Let(var,e1,e2) -> (match var with
  					  |Var(str) -> (let v1 = eval e1 env in match v1 with
  					  										| VI(_) -> eval_anon e2 ((str,v1)::env)
  					  										| VF(_) -> eval_anon (App(e1,e2)) [])
  					  |_ -> failwith "Error: Incorrect name of variable")
  | Let_anon(e1,e2) -> eval_anon e2 (eval_anon e1 env)
  | Ite(c,e1,e2) -> if eval_cond c env then eval_anon e1 env else eval_anon e2 env
  | PrInt(e) -> env
  | Semis(e1,e2) -> failwith "Error: unexpected ';;'"
  | Fun(x,e) -> env
  | App(e1,e2) -> (let v = eval e2 env in  
      let f = eval e1 env in
      match f with
      | VF(x,e) -> eval_anon e ((x,v)::env)
      | VI _ -> failwith "Error: Incorrect name of function")

