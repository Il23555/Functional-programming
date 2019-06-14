// 40.1
let rec summa (xs,p,s) = 
    match xs with
        |head :: tail when p head -> summa(tail,p,s + head)
        |head :: tail -> summa(tail,p,s)
        |_ -> s

let rec sum (p, xs) = summa(xs,p,0)

// 40.2.1
let rec count1 (x,xs,i) =
    match xs with
    |head :: tail when head = x -> count1(x,tail,i+1)
    |head :: tail when head > x -> i
    |head :: tail -> count1(x,tail,i)
    |_ -> i

let rec count (xs, n) = count1(n,xs,0)

// 40.2.2
let rec insert (xs, n) = 
    match xs with
    | head :: tail when head = n -> head :: n :: tail
    | head :: tail when head > n -> n :: head :: tail
    | head :: tail -> head :: insert(tail,n)
    |_ -> [n]

// 40.2.3
let rec intersect1 (xs1,xs2,list) =
        match (xs1,xs2) with
        |(head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: intersect1 (tail1, tail2, list)
        |(head1 :: tail1, head2 :: tail2) when head1 < head2 -> intersect1 (tail1, head2 :: tail2, list)
        |(head1 :: tail1, head2 :: tail2) when head1 > head2 -> intersect1 (head1 :: tail1, tail2, list)
        |_ -> list

let rec intersect (xs1, xs2) = intersect1(xs1,xs2,[])

// 40.2.4
let rec plus1 (xs1,xs2,list)=
     match (xs1,xs2) with
        |(head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: plus1 (tail1, tail2, list)
        |(head1 :: tail1, head2 :: tail2) when head1 < head2 -> head1 :: plus1 (tail1, head2 :: tail2, list)
        |(head1 :: tail1, head2 :: tail2) when head1 > head2 -> head2 :: plus1 (head1 :: tail1, tail2, list)
        |_ -> list

let rec plus (xs1, xs2) = plus1(xs1,xs2,[])

// 40.2.5
let rec minus_1 (xs1,xs2,list) =
     match (xs1,xs2) with
        |(head1 :: tail1, head2 :: tail2) when head1 = head2 -> minus_1 (tail1, tail2, list)
        |(head1 :: tail1, head2 :: tail2) when head1 < head2 -> head1 :: minus_1 (tail1, head2 :: tail2, list)
        |_ -> list @ xs1

let rec minus (xs1, xs2) = minus_1(xs1,intersect(xs1,xs2),[])

// 40.3.1
let rec smallest = 
    let rec min (xs,m) =
        match xs with
        |head :: tail when head < m -> min(tail,head) 
        |head :: tail -> min(tail,m)
        |_ -> m
    fun xs -> min(xs,xs.Head)

// 40.3.2
let rec delete (n, xs) = 
     match xs with
    |head :: (head2 :: tail) when head = n -> head2 :: tail
    |head :: (head2 :: tail) -> head :: delete(n,head2 :: tail)
    |[head;head2] when head = n -> [head2]
    |[head;head2] -> head :: delete (n,[head2])
    |[head] when head = n -> []
    |[head] -> [head]
    |_ -> []

// 40.3.3
let rec new_list (xs,list) =
    match xs with
    |head :: tail -> new_list(tail,insert(list,head))
    |_ -> list

let rec sort = fun xs -> new_list(xs,[])

// 40.4
let rec revrev = 
    let rec rev = function
        |head :: tail -> List.rev head ::  rev tail
        |_ -> []
    fun xs  -> rev (List.rev xs)

