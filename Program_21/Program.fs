// 50.2.1
let fac_seq = seq {
     let mutable p = 1
     yield p
     for i in 1..1000 do 
        p <- p*i
        yield p }
    
// 50.2.2
let seq_seq = seq {
    yield 0
    for i in 1..1000 do
        yield (0-i)
        yield i }
