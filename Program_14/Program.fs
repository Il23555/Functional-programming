// 40.1
let rec sum (p, xs) = 
    match xs with
    | [head] when p head -> head
    | head :: tail when p head -> head + sum(p, tail)
    | _ :: tail -> sum(p, tail)
    | _ -> 0

// 40.2.1
let rec count (xs, n) = 
    match xs with
    | head :: tail when head = n -> 1 + count(tail,n)
    | head :: _ when head > n -> 0
    | _ :: tail -> count(tail,n)
    | _ -> 0

// 40.2.2
let rec insert (xs, n) = 
    match xs with
    | head :: tail when head >= n -> n :: head :: tail
    | head :: tail -> head :: insert(tail,n)
    |_ -> [n]

// 40.2.3
let rec intersect (xs1, xs2) = 
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: intersect (tail1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 < head2 -> intersect (tail1, head2 :: tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 > head2 -> intersect (head1 :: tail1, tail2)
    | _ -> []

// 40.2.4
let rec plus (xs1, xs2) = 
     match (xs1,xs2) with
     | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: head2:: plus (tail1, tail2)
     | (head1 :: tail1, head2 :: tail2) when head1 < head2 -> head1 :: plus (tail1, head2 :: tail2)
     | (head1 :: tail1, head2 :: tail2) when head1 > head2 -> head2 :: plus (head1 :: tail1, tail2)
     | (_, []) -> xs1
     | ([], xs2) -> xs2
     |_ -> []

// 40.2.5
let rec minus (xs1, xs2) = 
    match (xs1,xs2) with
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> minus(tail1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 < head2 -> head1 :: minus(tail1, head2 :: tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 > head2 -> minus(head1 :: tail1, tail2)
    | (xs1, []) -> xs1
    | _ -> []

// 40.3.1
let rec smallest = fun xs -> 
    let rec min (xs :int list, m) =
        match xs with
        | head :: tail when head < m -> min(tail,head) 
        | _ :: tail -> min(tail,m)
        | _ -> Some (m)
    match xs with
    | [] -> None
    | head :: _ -> min(xs,head)

// 40.3.2
let rec delete (n, xs) = 
    match xs with
    | head :: tail when head = n -> tail
    | head :: tail -> head :: delete(n, tail)
    | _ -> []
    
// 40.3.3
let rec sort = function
    | [] -> []
    | xs -> Option.get(smallest xs) :: sort(delete(Option.get(smallest xs),xs))

// 40.4
let rec revrev = 
    let rec rev = function
        | head :: tail -> List.rev head :: rev tail
        | _ -> []
    fun xs -> rev (List.rev xs)

let l = [4;5;2;9;10]
printfn "%s" (string (smallest (l)))
printfn "%s" (string (sort (l)))