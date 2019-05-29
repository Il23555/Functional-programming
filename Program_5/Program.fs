// 16.1
let notDivisible (n,m) = m % n = 0

// 16.2
let rec division = function
    | (n,i) when n = i -> true
    | (n,i) when (n % i) = 0 -> false
    | (n,i) -> division(n,i+1)

let  prime = function
    |1 -> false
    |2 -> true
    |n -> division(n,2)