﻿// 17.1
let rec pow = function
    | (s,0) -> ""
    | (s,1) -> s
    | (s,n) -> pow(s,n-1) + s

// 17.2
let rec isIthChar = function
   |(s:string,n,c) -> s.[n] = c

// 17.3
let rec count = function
    | (s,-1,c,i) -> i
    | (s,n,c,i)  when isIthChar(s,n,c) -> count(s,n-1,c,i+1)
    | (s,n,c,i) -> count(s,n-1,c,i)

let rec occFromIth = function
    | (s,n,c) -> count(s,n,c,0)

