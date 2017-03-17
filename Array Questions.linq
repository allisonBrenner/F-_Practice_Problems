<Query Kind="FSharpProgram" />

open System

//1.	In an array 1-100 numbers are stored, one number is missing how do you find it?
//      if we know there should be exactly 100 consecutive numbers we can get the sum of all elements subtracted from the expected sum
let findMissing (arr:int[]) =
    let rec sum (arr:int[]) =
        match arr.Length with
        | n when n=1 -> arr.[0]
        | _ -> arr.[0] + sum arr.[1..arr.Length-1]
    
    let actual = sum arr
    let expected = sum [|1..100|]
    expected - actual
    
let missingArr = [|52..100|] |> Array.append [|1..50|]    
let missingInt = sprintf "1. Given the array \n %A \n the missing int is %d \n" missingArr (findMissing missingArr)
missingInt.Dump()


//2.	In an array 1-100 exactly one number is duplicate how do you find it?
//      this can be found the same way as above, because the sum will be over by the duplicate int
let findDouble (arr:int[]) =
    let rec sum (arr:int[]) =
        match arr.Length with
        | n when n=1 -> arr.[0]
        | _ -> arr.[0] + sum arr.[1..arr.Length-1]
    
    let actual = sum arr
    let expected = sum [|1..100|]
    actual - expected
    
let doubleArr = [|50..100|] |> Array.append [|1..50|]    
let doubleInt = sprintf "2. Given the array \n %A \n the duplicate int is %d \n" doubleArr (findDouble doubleArr)
doubleInt.Dump()


//3.	In an array 1-100 multiple numbers are duplicates, how do you find them all?
//      we can traverse the array while and check if the current items exists in the previous items
//      for each item we can check if there is an equivalaent in the visted portion, if yes add it to dupes
//      if it is not already there
let rec checkVisited (inputList:int list) (i:int)=
    match inputList with
    | [] -> false
    | x::xs -> 
            match i = x with
            | true  -> true
            | false -> checkVisited xs i

let rec findMultiDupes (input:int[]) (visited:int list) (acc:int list) =
    let isDupe = checkVisited visited input.[0]
    let isAcc = checkVisited acc input.[0]
    
    match input.Length with
    | len when len=1  -> acc
    | len when len<>1 ->
        match (isDupe,isAcc) with
        | (false,_)
        | (true,true)  -> findMultiDupes input.[1..] (input.[0]::visited) acc
        | (true,false) -> findMultiDupes input.[1..] (input.[0]::visited) (input.[0]::acc)


let multiDupes = sprintf "3. Find duplicates in an array with multiple duplicates %A -> %A\n" [|11;9;4;7;11;4;2;3;11;4;1|] (findMultiDupes [|11;9;4;7;11;4;2;3;11;4;1|] [] []) |> Dump

//4.	Given two arrays find which numbers are not present in the second array
let rec findMultiMissing (a:int[]) (b:int[]) (acc:int list)=
    
    match a.Length with
    | len when len=1  -> acc
    | len when len<>1 ->
                        match checkVisited (b |> Array.toList) a.[0] with
                        |true  -> findMultiMissing a.[1..] b acc
                        |false -> 
                                  match checkVisited acc a.[0] with
                                  | true  -> findMultiMissing a.[1..] b acc
                                  | false -> findMultiMissing a.[1..] b (a.[0]::acc)
   

let multiMissing = sprintf "4. Given [|11,3,14,7,15,17|] find the ints missing from [|14;3;15;|] -> %A \n" (findMultiMissing [|11;3;14;7;15;17|] [|14;3;15;|] []) |> Dump


//5.	Find second highest number in an integer array
let rec findSecondMax (input:int[]) (max1:int) (max2:int) =
    match input.Length with
    | len when len=1  ->
        match input.[0] > max1 with
        | true  -> max1
        | false -> 
            match input.[0] > max2 with
            | true  -> input.[0]
            | false -> max2
    | len when len<>1 ->
        match input.[0] > max1 with
        | true  -> findSecondMax input.[1..] input.[0] max1
        | false ->
            match input.[0] > max2 with
            | true  -> findSecondMax input.[1..] max1 input.[0]
            | false -> findSecondMax input.[1..] max1 max2

let secondHighest = sprintf "5. Find the second highest int in %A -> %A \n" [|1;2;3;4;5;6|] (findSecondMax [|1;2;3;4;5;6|] 0 0) |> Dump

//6.	Find all pairs of integers whose sum is equal to given number
//      1 2 3 4 5 6 > 9 
//      1 8 input.[0]  find diff.[0] in input.[1..] 
//      2 7 input.[1]  input.[2..]
//      3 6 input.[2]  input.[3..]
let findPairs (arr:int[]) (sum:int) =

    let findIndexOf (i:int) (arr:int[]) =
        arr
        |> Array.mapi (fun index x ->
                match x=i with
                | true  -> Some index
                | false -> None
            )
        |> Array.filter (fun opt ->
                match opt with
                |Some x -> true
                |None -> false
            )
        |> Array.map (fun someInt ->
                match someInt with
                | Some x -> x
                | None -> 0
            )

    let diff = arr |> Array.map (fun x -> sum-x)
    
    arr
    |> Array.mapi (fun index num ->
            match findIndexOf diff.[index] arr with
            | [||] -> []
            | [|x|] -> [(num,arr.[x])]
        )
    |> Array.filter (fun list ->
            match list with
            |[] -> false
            |x::xs -> true
        )
    |> Array.toList
    |> List.concat
    
    
    
let sumPairs = sprintf "6. Find all pairs in %A whose sum is %A -> %A\n" [|1;2;3;4;5;6|] 9 (findPairs [|1;2;3;4;5;6|] 9) |> Dump


//7.	Find the largest and smallest numbers in array
//      assume first element is the max and min and overwrite when a next element is larger or smaller, recursively
let findMaxMin (arr:int[]) =
    let mutable max = arr.[0]
    let mutable min = arr.[0]
    
    arr
    |> Array.map
        (fun x ->
            match x with
            | x when x > max -> max <- x
            | x when x < min -> min <- x
            | _ -> max <- max
        )
        
    (max,min)
    
let maxAndMin = sprintf "7. Given the array %A the max and min are %A \n" [|12;4;15;11;3;6;9|] (findMaxMin [|12;4;15;11;3;6;9|])
maxAndMin.Dump()



//8.	Find top two maximum number in array
//      Like finding one max, we can assume the top two are the first two and overwrite them if the recursive rest of the array has larger ints
let rec findMaxTwo (input:int[]) (max1:int) (max2:int) =
    match input.Length with
    | len when len=1  ->
        match input.[0] with
        | x when x > max1 -> (x,max1)
        | x when x > max2 -> (max1,x)
        | _ -> (max1,max2)
    | len when len<>1 ->
        match input.[0] > max1 with
        | true  -> findMaxTwo input.[1..] input.[0] max1
        | false ->
            match input.[0] > max2 with
            | true  -> findMaxTwo input.[1..] max1 input.[0]
            | false -> findMaxTwo input.[1..] max1 max2
    
    
let maxes = sprintf "8. The two maximum numbers in %A are %A" [|3;11;9;8;4;1|] (findMaxTwo [|3;11;9;8;4;1|] 0 0)
maxes.Dump()


//