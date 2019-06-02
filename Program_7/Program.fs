// 20.3.1
let vat n x = x*(1.0 + 0.01*n)

// 20.3.2
let unvat n x = x * 100.0/float(n + 100)

// 20.3.3
let rec iter_min = function
    |(f,n) when f(n) = 0 -> n
    |(f,n) -> iter_min(f,n+1)

let rec min f = iter_min(f,0)

