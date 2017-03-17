<Query Kind="FSharpProgram" />

open System

//1. remove consecutive duplicates: aaabbaa -> aba
//// base case, if the string is one letter, there are no dupes return as is
//// if the first two letters of a string are the same, 
//// ditch the first letter return the dedupe of the rest
//// otherwise, include the first and the deduped rest of the string
let rec dedupe (string:string) =
    if string.Length = 1 then
        string
    else
        match string.[0] = string.[1] with
        | true -> dedupe string.[1..]
        | false -> string.[0..0] + (dedupe string.[1..]) 
    

let dupes = "aaabbbcdddece"
let deduped = sprintf "1. Remove all consecutive duplicates from %s -> %s" dupes (dedupe dupes)
deduped.Dump()
  
   
//2. Reverse a string
//// base case - if the string is length 1, then return it as is
//// otherwise, take the last letter of the string and append it to the reversed rest
let rec reverse (string:string) =
    match string.Length with
    | 1 -> string 
    | _ -> string.[string.Length-1..string.Length-1] + (reverse string.[0..string.Length-2])
  
let forward = "banana"
let backward = sprintf "2. Reverse the string %s -> %s" forward (reverse forward)
backward.Dump()

//3. Write code to check a String is palindrome or not? 
//// split the string in two halves (if its an odd lenght ditch the middle char)
//// reverse one string, return true if both are equal 
let ispal (string:string) =
    let left = string.[0..(string.Length/2)-1]
    let right = string.[((string.Length/2) + (string.Length%2))..string.Length-1] |> reverse
    left = right

let palindrome = "amanaplanacanalpanama"
let checkPal = sprintf "3. Check if %s is a palindrome -> %A" palindrome (ispal palindrome)
checkPal.Dump()
 
//4. Find a character in an array (finds all indexes where the char occurs)
//// iterate over the string use an accumulator to add indexes of ever matching char
let find ch (str:string) =
    str.ToCharArray()
    |> Array.mapi
        (fun i x ->
            match x with
            | c when c=ch -> i
            | _ -> str.Length
        )
    |> Array.filter (fun x -> x<str.Length)
    

let findExample = "australia"
let found = sprintf "4. Find all indexes where %c occurs in %s -> %A" 'a' findExample (find 'a' findExample)
found.Dump()


//5. Write a method which will remove any given character from a String
//// find all indexes where the char occurs
//// if there are none then return the string
//// otherwise, take the string until the char occurs 
//// and append it to the remaining string where the char is removed
let rec remove (ch:char) (str:string) =
    let index = find ch str

    if index.Length > 0 then
        let i = index.[0]
        let b =
            match i with
            | i when i = 1 -> str.[0..0]
            | i when i > 1 -> str.[0..i-1]
            | _ ->  ""
    
        let a = 
            match i with 
            | i when i = str.Length -> str.[str.Length-1..str.Length-1]
            | _ -> remove ch str.[i+1..str.Length-1]
            
        b + a
    else 
        str
        
let complete = "bananas"
let removed = sprintf "5. Remove all occurances of %c from %s -> %s" 'a' complete (remove 'a' complete)
removed.Dump()

//6. remove char by index
/// concat string up until index with string after index
let removeIndex i (string:string)=
    match i with
    | n when n=0 -> string.[1..string.Length-1]
    | n when n=(string.Length-1) -> string.[0..string.Length-2]
    | _ -> string.[0..i-1] + string.[i+1..string.Length-1] 
    
let removedIndex = sprintf "6. Remove the character is position %d from %s -> %s" 3 complete (removeIndex 3 complete)
removedIndex.Dump()
    
//7. Print all permutation of String
//// base case is a string of length two which returns 2 results, itself and itself swapped
//// otherwise, each letter appended to all permutations of the remaining letters
//// c a t -> c + permute(a t), a + permute(c t), t + permute(c a) -> cat, cta, act, atc, tca, tac
let rec makeStrings (start:string) (chars:char []) =
    match chars with
    | [||] -> [|start|]
    | _ -> 
            chars
            |> Array.mapi (fun i c -> 
                    let start = (start + (string c)) 
                    let remaining = chars.[0..i-1] |> Array.append chars.[i+1..] 
                    makeStrings start remaining
                )
            |> Array.concat
 
let permute (s:string) =
    makeStrings "" (s.ToCharArray())
               
                
let cat = sprintf "7. Find all permutations of %s -> %A" "cat" (permute "cat")    
cat.Dump()


//8. Write a function to find the longest palindrome in a given string - DO IN DYNAMIC PROGRAMMING
// bananas 
// start with the first smallest substring and check if its a palindrome 
// if its not keep going, if it is add it to the accumulater and then keep going 
// keep going like that such that every letter in the original string has a chance to start off the substrings
// b (ananas) -> ba, ban, bana, banan, banana, bananas -> palindromes[]
// a (nanas)  -> an, ana, anan, anana, ananas          -> palindromes[ana, anana]
// n (anas)   -> na, nan, nana, nanas                  -> palindormes[ana, anana, nan, nana]
// a (nas)    -> an, ana, anas                         -> palindromes[ana, anana, nan, nana, ana]
// n (as)     -> na, nas                               -> palindromes[ana, anana, nan, nana, ana]
// a (s)      -> as                                    -> palindromes[ana, anana, nan, nana, ana]
// s ()       ->                                       -> palindromes[ana, anana, nan, nana, ana] |> findLongest    
let longestPal (input:string) =
    let rec loop (acc:string []) (s:string) =
        match s.Length with
        | n when n <= 1 -> acc
        | n when n > 1 -> 
                            let head = s.[0..0]
                            let tail = s.[1..s.Length-1] 
                            
                            let t = tail.ToCharArray()
                            let addedList = t 
                                            |> Array.mapi (fun i c -> 
                                                    match (head + tail.[0..i]) with
                                                    | x when ispal x = true -> [|x|] |> Array.append acc 
                                                    | x when ispal x = false -> acc
                                                ) 
                                            |> Array.concat

                            addedList |> Array.append (loop [||] tail)
                            
    let rec findLongest (longest:string) (pals:string[]) = 
        match pals with
        | [||] -> longest
        | _ ->
            match pals.[0].Length > longest.Length with
            | true  -> findLongest pals.[0] pals.[1..]
            | false -> findLongest longest pals.[1..]

                           
    let allPals = loop [||] input 
    
    match allPals with
    | [||] -> "No pals exist"
    | _ -> findLongest allPals.[0] allPals


let longestPalindrome = sprintf "8. Find the longest palindrome within the string %A -> %A" "bananas" (longestPal "bananas")
longestPalindrome.Dump()


//9. Find first non repeated character of a given String
//// base case, if the string has only 1 char, its single so return it
//// if the first two letters are different, return the first letter
//// otherwise recurse on the string without the first repeated char
let rec firstSingle (string:string) =
    match string.Length with
    | n when n=1 -> string.[0]
    | n when n=2 -> 
        match string.[0] = string.[1] with
        | false -> string.[0]
        | true -> ' '
    | _ -> 
        match string.[0] = string.[1] with
        | false -> string.[0]
        | true -> 
            let rest = remove string.[0] string
            firstSingle rest

let hasDupes = "aaabbbcdddece"
let firstNonDupe = sprintf "9. Find the first non-repeating character in %s -> %c" hasDupes (firstSingle hasDupes)
firstNonDupe.Dump()

//10. Count occurrence of a given character in a String
//// iterate over the string, accumulate a list of all occurences
//// return length of accumulator list
let countOccurrence ch (str:string) =
    str.ToCharArray()
    |> Array.mapi (fun i x -> (i,x))
    |> Array.filter 
        (fun x -> 
            match x with
            | (_,x) when x = ch -> true
            | _ -> false
        )
    |> Array.map (fun (x,_) -> x)
    |> Array.length
    
let count = sprintf "10. Count the occurrences of %c in %s -> %d" 'a' "banana" (countOccurrence 'a' "banana")
count.Dump()
 
//11. Return a string with the chars alphabetized
//merge two sorted lists, if the leftmost item of list a is less
//return item and compare next leftmost, else return leftmost item of list b and compare next
let merge (a:string) (b:string) =
    let mutable acc = ""
    
    let rec loop (i:int) (j:int) =
        if i < a.Length then
            if j < b.Length then
                match a.[i] < b.[j] with
                | true ->
                    acc <-  acc + a.[i..i]
                    loop (i+1) j 
                | false ->
                    acc <-  acc + b.[j..j] 
                    loop i (j+1) 
            else
                acc + a.[i..a.Length-1]
        else
            acc + b.[j..b.Length-1] 
        
    loop 0 0 
            
//divide list in half until there are only two items,
//smallest item leftmost, then merge sorted lists together
let rec mergeSort  (string:string) =
    match string.Length with
    //a string of length 1 is already sorted
    | n when n=1 -> string
    //a string of length 2 should have the smaller char on the left
    | n when n=2 ->
        match string.[0] < string.[1] with
        |true -> string
        |false -> string.[1..1] + string.[0..0]
    //if string is longer than 2 chars, half it again
    |_ ->
        let a = mergeSort string.[0..string.Length/2]
        let b = mergeSort string.[(string.Length/2)+1..string.Length-1]
        merge a b

let alphabetized = sprintf "11. Sort %s into alphabetical order -> %s" "casiopia" (mergeSort "casiopia")
alphabetized.Dump()

//12. Check if two String are Anagram
//sort both strings, if equal they are anagrams
let isAnagram a b =
    (mergeSort a) = (mergeSort b)

    
let anagramCheck = sprintf "12. Check if %s and %s are anagrams -> %A" "angered" "enraged" (isAnagram "angered" "enraged")
anagramCheck.Dump()