// 49.5.1
let even_seq = Seq.initInfinite(fun i -> (i+1)*2)

// 49.5.2
let factorial x =
    let rec f n res =
        if n <= 1 then res
        else f (n - 1) (res * n)
    f x 1

let fac_seq = Seq.initInfinite(factorial)

// 49.5.3
let rec f = function
    | 0 -> 0
    | i when i % 2 = 0 -> i/2
    | i -> 0 - i + f(i-1)

let seq_seq = Seq.initInfinite(f)
