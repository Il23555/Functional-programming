// 48.4.1
let rec fibo1 n n1 n2 = 
    if n = 0 then n2
             else fibo1 (n-1) (n1 + n2) n1

// 48.4.2
let rec fibo2 c n =
   if n = 0 then c 0
   else
   if n = 1 then c 1
            else fibo2 (fun res1 -> fibo2 (fun res2 -> c(res1 + res2)) (n - 2)) (n - 1)

// 48.4.3            
let rec bigList n k =
    if n = 0 then k []
             else bigList (n-1) (fun res -> k(1 :: res))
