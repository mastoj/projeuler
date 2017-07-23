open System

module Problem1 = 
    let isMultiplier x y = y % x = 0
    let isMultiplier3 = isMultiplier 3
    let isMultiplier5 = isMultiplier 5
    let isMultiplier3Or5 x = (isMultiplier3 x) || (isMultiplier5 x)
    let sum3And5Mulitples x = [0 .. x] |> List.filter isMultiplier3Or5 |> List.sum
    let solution() = sum3And5Mulitples 999

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
    let solution() = generateFib() |> Seq.takeWhile ((>) 4000000) |> Seq.filter isEven |> Seq.sum

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
   
    let solution() = findMaxPrimeFactor 600851475143L
   
module Problem4 = 
    open System

    let isPalindromeNumber x = 
        let xStr = x.ToString()
        xStr = String(Array.rev(xStr.ToCharArray()))

    let palindromeProducts maxNum minNum = 
        seq { for x in maxNum .. -1 .. minNum do
                 for y in x .. -1 .. minNum do
                    if isPalindromeNumber (x*y) then yield x*y
            }

    let solution() = palindromeProducts 999 100 |> Seq.max

module Problem5 = 

    let isDividedByList list x = 
        list |> List.exists (fun i -> x % i <> 0) |> not

    let getMultiples numbers = 
        Seq.initInfinite id
        |> Seq.filter ((<) 0)
        |> Seq.filter (isDividedByList numbers)

    let solution() = getMultiples [1 .. 20] |> Seq.head

module Problem6 = 
    let findSquareDiff maxNum = 
        let rec inner currentNum acc1 acc2 = 
            if currentNum > maxNum then
                (acc2*acc2) - acc1
            else
                inner (currentNum + 1L) (acc1 + currentNum*currentNum) (acc2 + currentNum)
        inner 1L 0L 0L

    let solution() = findSquareDiff 100L

Problem6.solution() |> printfn "Solution %A: "