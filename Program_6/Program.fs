// 17.1
let rec pow = function
    | (s,0) -> ""
    | (s,1) -> s
    | (s,n) -> pow(s,n-1) + s

// 17.2
let rec isIthChar = function
   |(s:string,n,c) when n < s.Length -> s.[n] = c
   |(s,n,c) -> false

// 17.3
let rec count = function
    | (s:string,n,c,i) when n > s.Length -> i
    | (s,n,c,i) when isIthChar(s,n,c) -> count(s,n+1,c,i+1)
    | (s,n,c,i) -> count(s,n+1,c,i)

let rec occFromIth = function
    | (s,n,c) -> count(s,n,c,0)
    
