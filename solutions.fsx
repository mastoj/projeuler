open System

[<AutoOpen>]
module Utils = 

    let convertChar (c:char) = Int64.Parse(c.ToString())

    let convertString (str:string) = Int64.Parse(str)


    let isPrimeInt x = 
        let minCandidate = sqrt (float x) |> int
        seq { for i in 2 .. minCandidate do yield x % i = 0 }
        |> Seq.exists id
        |> not

    let isPrimeInt64 (x:int64) = 
        let minCandidate = sqrt (float x) |> int64
        seq { for i in 2L .. minCandidate do yield x % i = 0L }
        |> Seq.exists id
        |> not


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
    let findNthPrime nth =
        Seq.initInfinite id
        |> Seq.map int64
        |> Seq.filter ((<) 1L)
        |> Seq.filter isPrimeInt64
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

module Problem10 =

    let getPrimesLessThan x = 
        seq { for i in 2L .. (x-1L) do if isPrimeInt64 i then yield i}

    let sumPrimeLessThan = getPrimesLessThan >> Seq.sum

    let solution() = sumPrimeLessThan 2000000L

module Problem11 = 
    let input = """
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""

    let diagToRows (grid: 'a list list) = 
        [
            for i in 0 .. (grid.Length - 2) do
                yield [ for j in 0 .. i do yield grid.[i - j].[j]]
                yield [ for j in 0 .. i do yield grid.[grid.Length - 1 - j].[grid.Length - 1 - i + j]]
            yield [ for i in 0 .. (grid.Length - 1) do yield grid.[i].[grid.Length - 1 - i]]
        ]

    let diagToRows2 (grid: 'a list list) = 
        [
            for i in 0 .. (grid.Length - 2) do
                yield [ for j in 0 .. i do yield grid.[j + grid.Length - i - 1 ].[j]]
                yield [ for j in 0 .. i do yield grid.[j].[j + grid.Length - i - 1]]
            yield [ for i in 0 .. (grid.Length - 1) do yield grid.[i].[i]]
        ]

    let splitOn (str:string) (inStr:string) = inStr.Split([|str|], StringSplitOptions.None) |> List.ofArray

    let getGrid str = str |> splitOn "\n" |> List.filter (fun s -> s <> "") |> List.map (splitOn " " >> (List.map convertString))

    let getAdjacentProductOfSize n grid = 
        grid 
        |> List.filter (fun l -> l |> List.length >= n) 
        |> List.map ((List.windowed n) >> List.map (fun l2 -> l2 |> List.reduce (*)) >> List.max)
        |> List.max

    let columnsToRows<'a> (grid: 'a list list) = 
        [ for i in 0 .. (grid.Length - 1) do
            yield [ for j in 0 .. (grid.[0].Length - 1) do yield grid.[j].[i] ]
        ]

    let makeTransformish grid = 
        [
            grid
            grid |> columnsToRows
            grid |> diagToRows
            grid |> diagToRows2
        ]

    let solution() = input |> getGrid |> makeTransformish |> List.map (getAdjacentProductOfSize 4) |> List.max

Problem11.solution() |> printfn "Solution %A: "
