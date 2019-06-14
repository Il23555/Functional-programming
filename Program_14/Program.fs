// 40.1
let rec sum (p, xs) = 
    match xs with
    |[head] when p head -> head
    |head :: tail when p head -> head + sum(p, tail)
    |_ :: tail -> sum(p, tail)
    |_ -> 0

// 40.2.1
let rec count (xs, n) =
    match xs with
    |head :: tail when head = n -> 1 + count(tail,n)
    |head :: tail when head > n -> 0
    |head :: tail -> count(tail,n)
    |_ -> 0

// 40.2.2
let rec insert (xs, n) = 
    match xs with
    | head :: tail when head = n -> head :: n :: tail
    | head :: tail when head > n -> n :: head :: tail
    | head :: tail -> head :: insert(tail,n)
    |_ -> [n]

// 40.2.3
let rec intersect (xs1, xs2) =
     match (xs1,xs2) with
    |(head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: intersect (tail1, tail2)
    |(head1 :: tail1, head2 :: tail2) when head1 < head2 -> intersect (tail1, head2 :: tail2)
    |(head1 :: tail1, head2 :: tail2) when head1 > head2 -> intersect (head1 :: tail1, tail2)
    |_ -> [] @ xs2

// 40.2.4
let rec plus (xs1,xs2)=
     match (xs1,xs2) with
     |(head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: head2:: plus (tail1, tail2)
     |(head1 :: tail1, head2 :: tail2) when head1 < head2 -> head1 :: plus (tail1, head2 :: tail2)
     |(head1 :: tail1, head2 :: tail2) when head1 > head2 -> head2 :: plus (head1 :: tail1, tail2)
     | (_, []) -> xs1
     | ([], xs2) -> xs2
     |_ -> []

// 40.2.5
let rec minus_1 (xs1,xs2,list) =
     match (xs1,xs2) with
     |(head1 :: tail1, head2 :: tail2) when head1 = head2 -> minus_1 (tail1, tail2, list)
     |(head1 :: tail1, head2 :: tail2) when head1 < head2 -> head1 :: minus_1 (tail1, head2 :: tail2, list)
     |_ -> list @ xs1

let rec minus(xs1, xs2) = minus_1(xs1,intersect(xs1,xs2),[])

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
    fun xs -> rev (List.rev xs)


(*let f x = x > 5
let l1 = [1;2;3;4;5;6;7]
let l2 = [1]
let l0 = [1]
let l3 = [3;4;5]
let l6 = [3;6;6;7;8]
let l10 = [[3;4;5];[3;5];[4;5]]
let l4 = [10;4;6;3;1]

*)
  
//printf "%s" (string (plus(l2,l0)))
//printf "%s" (string (sum(f,l6)))
//printf "%s" (string (count(l6,6)))
//printf "%s" (string (insert(l3,3)))
//printf "%s" (string (intersect(l1,l3)))
//printf "%s" (string (minus(l1,l3)))
//printf "%s" (string (smallest(l4)))
//printf "%s" (string (delete(6,l6)))
//printf "%s" (string (sort (l4)))
//printf "%s" (string (revrev(l10)))