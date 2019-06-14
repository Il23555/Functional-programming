// 41.4.1
let list_filter f xs = List.foldBack (fun head tail -> if f head then head :: tail else tail) xs []

// 41.4.2
let sum (p, xs) = List.foldBack (fun head tail -> if p head then head + tail else tail) xs 0

// 41.4.3
let revrev = fun list -> List.fold (fun head tail -> List.fold(fun head1 tail1 -> tail1 :: head1) [] tail :: head) [] list
