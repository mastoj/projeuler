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

module Problem7 = 
    let isPrime (x:int64) = 
        let minCandidate = sqrt (float x) |> int64
        seq { for i in 2L .. minCandidate do yield x % i = 0L }
        |> Seq.exists id
        |> not

    let findNthPrime nth =
        Seq.initInfinite id
        |> Seq.map int64
        |> Seq.filter ((<) 1L)
        |> Seq.filter isPrime
        |> Seq.skip (nth - 1)
        |> Seq.head

    let solution() = findNthPrime 10001

module Problem8 = 
    let input = """73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"""
    let inputStr = input.Split('\n') |> (fun i -> String.Join("", i))

    let convertChar (c:char) = Int64.Parse(c.ToString())

    let productStr (charArr:char []) = 
        charArr
        |> Array.map convertChar
        |> Array.reduce (*)

    let findNLengthLarges (str:string) length = 
        str 
        |> Seq.windowed length
        |> Seq.map productStr
        |> Seq.max

    let solution() = findNLengthLarges inputStr 13

module Problem9 = 
    let findPythagoreanTripletsOf x = 
        seq {
            for i in 1 .. x do 
                for j in i .. x do
                    for k in j .. x do 
                        if i + j + k = x && i*i + j*j = k*k then yield (i,j,k)
        }

    let solution() = findPythagoreanTripletsOf 1000 |> Seq.head |> (fun (i,j,k) -> i*j*k)

Problem9.solution() |> printfn "Solution %A: "
