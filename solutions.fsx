module Problem1 = 
    let isMultiplier x y = y % x = 0
    let isMultiplier3 = isMultiplier 3
    let isMultiplier5 = isMultiplier 5
    let isMultiplier3Or5 x = (isMultiplier3 x) || (isMultiplier5 x)
    let sum3And5Mulitples x = [0 .. x] |> List.filter isMultiplier3Or5 |> List.sum
    let solution1 = sum3And5Mulitples 999

 module Problem2 = 
    