<Query Kind="FSharpProgram" />

open System

//Write a program to find out if two rectangles R1 and R2 are overlapping?
//
//                  (19,5) _________________ (35,5)
//                        |                 |
//   (3,3) _______________|____ (23,3)      |
//        |               |____|____________|
//        |         (19,2)     |             (35,2)
//        |____________________|
//   (3,0)                      (23,0)
//
        
type Rectangle =
    {
        x1: int
        y1: int
        x2: int
        y2: int
    }
    
let r1 = {x1=3; y1=0; x2=23; y2=3}
let r2 = {x1=19; y1=3; x2=35; y2=5}
     
let isOverlap rec1 rec2 =
    let leftright = if rec1.x2 > rec2.x2 then rec1.x1 > rec2.x2 else rec2.x1 > rec1.x2
    let topbottom = if rec1.y2 > rec2.y2 then rec1.y1 > rec2.y2 else rec2.y1 > rec1.y2
    match (leftright,topbottom) with
    | (false,false) -> true
    | _ -> false
    
let overlapping = sprintf "1.  Rectangle 1 overlaps with rectangle2: %b\n" (isOverlap r1 r2) |> Dump


//You need to write a function to climb n steps you can climb either 1 step at a time or 2 steps a time, write a function to return number of ways to climb a ladder with n step.
// when n=3, 2-1, 1-1-1                             n=3 => 2 = 1+1 = (3/2=1) + 1
// when n=4, 2-2, 2-1-1, 1-1-1-1                    n=4 => 3 = 3+0 = (4/2=2) + 1
// when n=5, 2-2-1, 2-1-1-1, 1-1-1-1-1              n=5 => 3 = 2+1 = (5/2=2) + 1
// when n=6, 2-2-2, 2-2-1-1, 2-1-1-1-1, 1-1-1-1-1-1 n=6 => 4 = 4+0 = (6/2=3) + 1
// when n=7, 2-2-2-1, 1-1-2-2-1, 1-1-1-1-2-1, 1-1-1-1-1-1-1 = 4
let waysToClimb n = (n/2) + 1

let ways = sprintf "2.  If you can climb 1 or 2 steps at a time, there are %d ways to climb %d steps\n" (waysToClimb 9) 9 |> Dump

//Write code to generate a random number in a range from min to max 



//Design an algorithm to find the frequency of occurrence of a word in an article
//let article = "An article, in this case, is considered one long string. The string contains words, which are characters separated by spaces. Words may also be separated by puncuation, such as a comma before a space, or a period at the end of a sentence."
let article = "apples apples oranges apples apples ducks and Apples"
let word = "apples"

let fst = (fun (x,y) -> x)
let snd = (fun (x,y) -> y)

let isSpecialChar ch =
    ['!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('; ')'; '_'; '-'; '+'; '['; ']'; '{'; '}'; '`'; ';'; ':'; '''; '"'; '/'; '?'; '>'; '<'; '.'; ',']
    |> List.filter (fun c -> c=ch)
    |> List.length
    
let countWord (input:string) (article:string) =
    let lower = article.ToLower()
    let chars = lower.ToCharArray() |> Array.filter (fun c -> (isSpecialChar c) <= 0)
    
    let rec getIndexes (words:char[]) i indexes =
        match words.Length with
        | len when len <= 1 -> indexes
        | len when len > 1  ->
            match words.[0] with
            | ch when ch = ' '  -> getIndexes words.[1..] (i+1) (i::indexes)
            | ch when ch <> ' ' -> getIndexes words.[1..] (i+1) indexes

    let indexes = getIndexes chars 0 [] |> List.rev |> List.toArray
    
    let wordIndexes =
        indexes
        |> Array.mapi (fun i x ->
                match i with
                | index when index = 0 -> (0,x-1)
                | index when index <>0 -> ((indexes.[i-1]+1),(x-1))
            )
        |> Array.rev
        |> Array.append [|(indexes.[indexes.Length-1]+1,chars.Length-1)|]
        |> Array.rev
        
    let rec wordList (indexes:(int*int)[]) (wordlist:char[] list)=
        match indexes.Length with
        | len when len <= 1 -> ((chars.[(fst indexes.[0])..(snd indexes.[0])])::wordlist)
        | len when len  > 1 -> wordList indexes.[1..] ((chars.[(fst indexes.[0])..(snd indexes.[0])])::wordlist)

    let words =
        wordList wordIndexes []
        |> List.filter (fun x ->
                x=input.ToCharArray()
            )
            
    words.Length
    

let occurrences = sprintf "3.  Given the article \"%s\" the word \"%s\" appeard %d times\n" article word (countWord word article) |> Dump

