<Query Kind="FSharpProgram" />

open System

//1.	Check if a number is power of two
//      A number is a power of two if half of that number is also a power of two
//      The base case is 2, which will return true
//      16/2 -> 8/2 -> 4/2 -> 2 -> true, 20/2 -> 10/2 -> 5 -> false
let rec power2 i =
    match i with
    | i when i%2=1 -> false
    | i when i < 2 -> false
    | i when i = 2 -> true
    | i when i > 2 -> power2 (i/2)

let checkPower = sprintf "1. Check if %d is a power of 2 -> %A" 1024 (power2 1024)
checkPower.Dump()


//2.	Check if an integer is Armstrong number
//      An Armstrong number of three digits is an integer such that the sum of the cubes of its digits is equal to the number itself. 
//      For example, 371 is an Armstrong number since 3**3 + 7**3 + 1**3 = 371.
//      isolate each digit by using %10, get its cube and add it to the sum of the recursed number (base case is a single digit <10)
let cube n = n*n*n

let isArmstrong n =
    let rec sumCubes n =
        match n with
        | n when n<10 -> cube n
        | _ -> (cube (n%10)) + sumCubes(n/10) 
    n = (sumCubes n)

let arm = sprintf "2. Check if %d is an Armstrong number -> %A" 371 (isArmstrong 371)
arm.Dump()


//3.	Find all prime numbers up to a given numbers 
//      A prime number is only divisible by itself and 1
//      if any number from 2 through n-1 returns no remainder, return false
//      this can be done exaustively by trying ever number or we can try selective numbers
//      we start by only checking ever second number 
//      3,5,7,11,13,15,17,19,21 and exclude any that are divisible by any numbers that we already checked
let getPrimes (n:int) =
    let rec isPrime (check:int) (against:int list) =
        match against with
        | [] -> true
        | x::xs -> 
            if check % x = 0 then false
            else isPrime check xs
        
    let rec nextPrime (visited: int list) (primes:int list) (tryInt: int) =
        match tryInt > n with
        | true  -> primes
        | false -> 
            match tryInt with
            | x when isPrime x visited = true  -> 
                nextPrime (x::visited) (x::primes) (tryInt+2)
            | x when isPrime x visited = false -> 
                nextPrime (x::visited) primes (tryInt+2)
            
    match n with
    | max when max <= 2 -> [1]
    | max when max = 3 -> [1;3]
    | max when max > 3 -> nextPrime [3] [3;1] 5
    
let primes = sprintf "3. Find all primes through %A -> %A" 45 (getPrimes 40)    
primes.Dump()

//3b. sieve of eratosthemes!
//    imagine a grid of all the numbers in a grid
//    skip 1, arrive at 2 and add it to your primes then remove all numbers that are divisible by two


//4.	Find the Nth Fibonacci number 
//      base case are the first two fibs, [1;1]
//      nth fib is fib(n-1) + fib(n-2)
//      [1;1;2;3;5;8;13;21;34]
let rec fib n =
    match n with
    | n when n < 2 -> 1
    | _ -> fib(n-1) + fib(n-2)
    
let fibonacci = sprintf "4. Find the %dth Fibonacci number -> %d" 7 (fib 7)
fibonacci.Dump()


//5.	Check if a number is binary (For this question, you need to write a function which will accept an integer and return true if it contains only 0 and 1 e.g. if input is 123 then your function will return false, for 101 it should return true.)let rec isBinary (n:int) =
let rec isBinary (n:int) =
    match n < 10 with
    | true  -> n<=1
    | false ->
        match n%10 with
        | x when x <= 1 -> isBinary (n/10)
        | x when x > 1 -> false

let binaryNum = sprintf "5. Check if %d is a binary number -> %A" 01111 (isBinary 01111)
binaryNum.Dump()

//6.	Reverse an integer
//      isolate digits using %10, last digit would naturally be returned first
//      187 -> 187%10 = 7, 187/10=18 -> 18%10 = 8, 18/10 = 1, 1%10 
let reverse n =
    let rec rev n acc=
        match n with
        | n when n<10 -> [n]
        | _ ->
            let a = n%10 
            let next = rev (n/10) acc
            a :: next
            
    rev 187 []
    //figure out how to return as an int instead of a list of digits
       
let backwards = sprintf "6. Reverse %d -> %A" 187 (reverse 187)
backwards.Dump()
        

//7.	Count the number of set bits in given integer
let rec countBits (acc:int) (n:int) =
    match n < 10 with
    | true  -> 
        match n with
        | x when x = 1 -> acc+1
        | x when x <> 1 -> acc
    | false ->
        match n%10 with
        | x when x = 1 -> countBits (acc+1) (n/10)
        | x when x <> 1 -> countBits acc (n/10)   
        
let setBits = sprintf "7. Count the number of set bits in %A -> %A" 101110 (countBits 0 101110)
setBits.Dump()


//8.	Find sum of all digits using recursion
//      isolate last digit with %10 and add to the recursive sum of the remaining digits
//      base case is a single digit < 10 
let rec sumAll n =
    match n with
    | n when n<10 -> n
    | _ -> (n%10)+sumAll(n/10)
    
let summed = sprintf "8. Find the sum of all digits in %d -> %d" 187 (sumAll 187) 
summed.Dump()



//9.	Find the largest of three integers
//      base case is one int, otherwise return the largest between the first vs. the recursive max of the rest
let rec findLargest (ints:int list) =
    if ints.Length = 1 then
        ints.Head   
    else
        match ints.Head with
        | n when n > findLargest ints.Tail -> n
        | _ -> findLargest ints.Tail
        
let bigInt = sprintf "10. Find the largest int from %A -> %d" [9;18;3;24;1] (findLargest [9;18;3;24;1])
bigInt.Dump()


