module Problem1 = 
    let isMultiplier x y = y % x = 0
    let isMultiplier3 = isMultiplier 3
    let isMultiplier5 = isMultiplier 5
    let isMultiplier3Or5 x = (isMultiplier3 x) || (isMultiplier5 x)
    let sum3And5Mulitples x = [0 .. x] |> List.filter isMultiplier3Or5 |> List.sum
    let solution1 = sum3And5Mulitples 999

module Problem2 = 

    let generateFib() =

        let rec inner (x,y) = 
            seq {
                let nextValue = x + y
                yield x + y
                yield! inner (nextValue, x)
            }
        inner (1,0)        
    let isEven x = x % 2 = 0
    let solution2 = generateFib() |> Seq.takeWhile ((>) 4000000) |> Seq.filter isEven |> Seq.sum

module Problem3 = 
    
    let rec findFactors (x:int64) = 
        seq {
            let maxVal = sqrt (float x) |> int64            
            let candidates = seq {for i = 2L to maxVal do yield ((i, x / i), (x <> 0L && x % i = 0L))}
            match x, candidates |> Seq.tryFind snd with
            | 0L, _ -> ()
            | _, Some ((factor, rest), _) -> 
                yield factor
                yield! findFactors rest
            | x, None ->
                yield x
        }

    let findMaxPrimeFactor = findFactors >> Seq.max
   
    let solution3 = findMaxPrimeFactor 600851475143L
