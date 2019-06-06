// 23.4.1
let change (a,b,c) = 
    let sum = a * 240 + b * 12 + c
    let g = sum / 240
    let s = (sum - g * 240) / 12
    let c = sum - g * 240 - s * 12
    (g,s,c)

let (.+.) x y =
    let (a,b,c) = x 
    let (a1,b1,c1) = y 
    change(a + a1, b + b1, c + c1)

let (.-.) x y = 
    let (a,b,c) = x
    let (a1,b1,c1) =y
    change(a - a1, b - b1, c - c1)

// 23.4.2
let (.+) x y = 
    let (a,b) = x 
    let (c,d) = y
    (a + c,b + d)

let (.-) x y = 
    let (a,b) = x 
    let (c,d) = y
    (a - c,b - d)

let (.*) x y = 
    let (a,b) = x
    let (c,d) = y 
    (a * c - b * d, a * d + b * c)

let (./) x y = 
    let (a,b) = x
    let (c,d) = y 
    ((a*c + b*d)/(c*c + d*d),(b*c - a*d)/(c*c + d*d))
