<Query Kind="FSharpProgram" />

open System

type Link =
    {
        value: int
        pointer: Link option
    }
    
let example12 = { value = 13; pointer = None}    
let example11 = { value = 12; pointer = Some example12}
let example10 = { value = 11; pointer = Some example11}
let example9 = { value = 10; pointer = Some example10}
let example8 = { value = 9; pointer = Some example9}
let example7 = { value = 8; pointer = Some example8}
let example6 = { value = 7; pointer = Some example7}
let example5 = { value = 6; pointer = Some example6}    
let example4 = { value = 5; pointer = Some example5}
let example3 = { value = 4; pointer = Some example4}
let example2 = { value = 3; pointer = Some example3}
let example1 = { value = 2; pointer = Some example2}
let example0 = { value = 1; pointer = Some example1}

//1.	Find the middle element of a linked list in single pass
//      1 > 2 > 3 > 4 > 5 > 6 > 7 > 8 > 9 > 10 > 11 > 12 > 13 > ()
//      (1,2) > (2,4) > (3,6) > (4,8) > (5,10) > (6,12) > (7,()) => 7
let middleLink (current:Link) =
    let rec travel (currents:(Link*Link) option) acc =
        match currents with
        | Some (current1,current2) ->
            match (current1.pointer,current2.pointer) with
            |(Some x, Some x1) ->
                match x1.pointer with
                | Some x2 -> travel (Some(x,x2)) (current1.value::acc)
                | None -> acc
            |_ -> (current1.value::acc) 
        | None -> acc

    travel (Some(current,current)) [] |> List.head    
    
let middle = sprintf "1. The middle link is %A" (middleLink example0)
middle.Dump()


//2.	Find the 3rd element from last in single pass
//      1 > 2 > 3 > 4 > 5 > 6 > 7 > 8 > 9 > 10 > 11 > 12 > 13 > ()
//     (1,4) > (2,5) > (3,6) > (4,7) > (5,8) > (6,9) > (7,10) > (8,11) > (9,12) > (10,13) > (11,()) => 11
let rec thirdToLast (current:Link) =
    match current.pointer with
    | Some oneAhead ->
        match oneAhead.pointer with
        | Some twoAhead -> 
            match twoAhead.pointer with
            | Some threeAhead -> thirdToLast oneAhead 
            | None -> current.value
        | None -> current.value
    | None -> current.value
    
let third = sprintf "2. The third to last link is %A" (thirdToLast example0)
third.Dump()

//3.	Reverse a singly linked list
let rec reverse (current:Link) acc =
    match current.pointer with
    | Some x -> reverse x (current.value::acc)
    | None -> (current.value::acc)
    
let rev = sprintf "3. Reverse a linked list: %A" (reverse example0 [])
rev.Dump()

