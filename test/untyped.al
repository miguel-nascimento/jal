let lambda_sum = \x -> \y -> x + y
let sum x y = x + y

let first x y = x
let snd x y = y

type Op = Add | Sub

let calc op x y = 
  match op with
    | Add -> x + y
    | Sub -> x - y

let five = calc Sub (calc Add 5 5) 5
