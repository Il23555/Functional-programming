// 50.2.1
let fac_seq = seq {
    yield 1
    let rec f n res = seq {
        yield res
        yield! f (n + 1)(res * n) }
    yield! seq (f 2 1)}
    
// 50.2.2
let seq_seq = seq {
    yield 0
    let rec f i = seq {
        yield (0-i)
        yield i
        yield! f (i + 1) }
    yield! f 1}

