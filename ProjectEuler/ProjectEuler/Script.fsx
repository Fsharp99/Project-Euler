﻿#I "..\github\"
#r @"MathNet.Numerics.3.10.0\lib\net40\MathNet.Numerics.dll"

open MathNet.Numerics.Random
open MathNet.Numerics.Distributions

#time "on"
fsi.ShowDeclarationValues <- true


//Problem 1

//If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
//Find the sum of all the multiples of 3 or 5 below 1000.

[|1..999|] |> Array.sumBy (fun x -> if x%3=0 || x%5=0 then x else 0)

//Problem 2

//Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
//1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
// By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
    
let fib1 = Seq.unfold (fun state ->
     if (snd state > 4000000) then None
     else Some(fst state + snd state, (snd state, fst state + snd state))) (0,1) |> Seq.sumBy (fun x -> if x%2=0 then x else 0)

//Problem 3

//The prime factors of 13195 are 5, 7, 13 and 29.
//What is the largest prime factor of the number 600851475143 ?

//sqrt 600851475143L
let naturalNum = [|1..10|] 

let primes3 = naturalNum |> Array.filter (fun x -> if x=1 then ignore else x)

let num = [|1..7|]
let primes2 xs = Array.map (match xs with
                | 1 -> "1"
                | 2 -> "2"
                | 3 -> "3"
                | x when x%2=0 -> "nOT PRIME"
                | x when x%3=0 -> "Not prime"
                | 5 -> "5"
                | x when x%5=0 -> "not prime"
                | 7 -> "7")
            


let primes = naturalNum |> Array.collect (fun x -> naturalNum |> Array.filter(fun y -> x%y<>0))

Array.filter (fun x -> x%x<>0)


[|2..1775146|] 


Array.create (fun x -> for x > 1 && < 1775146) 

Array.
    

// Problem 4

//A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
//Find the largest palindrome made from the product of two 3-digit numbers.

open System 
open System.Globalization

let xs = [|999..-1..100|] //if count down form 999 will find solution faster
let isPalindrome (s:string) = s = String(s.ToCharArray() |> Array.rev)
let isNumericPalindrome x y = isPalindrome (string (x*y))

xs
|> Array.choose (fun x -> xs |> Array.tryPick (fun y -> if isNumericPalindrome x y then Some (x,y) else None))
|> Array.maxBy ( fun (x, y) -> x * y )
||> (*) 
// this last part is a shorthand version of (|> fun (x,y) -> x * y)
// the double pipe takes a tuple and applies an operator



// Problem 5

// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

{1..400000000} |> Seq.find (fun x -> x%11=0 && x%12=0 && x%13=0 && x%14=0 && x%15=0 && x%16=0 && x%17=0 && x%18=0 && x%19=0 && x%20=0)

//This one is faster
{0..20..400000000} |> Seq.skip 1 |> Seq.find (fun x -> x%11=0 && x%12=0 && x%13=0 && x%14=0 && x%15=0 && x%16=0 && x%17=0 && x%18=0 && x%19=0 && x%20=0)



// Problem 6
// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

let array = [|1..100|] |> Array.sum 

let arraySqu = array * array

let squArray = [|1..100|] |> Array.map (fun x-> x*x) |> Array.sum

let diff = arraySqu - squArray


// Problem 7

// 










//Problem of calculating pi using random number generartor  (not part of project euler)


let size3 = 100000000

let random3 n = 
    let x = MersenneTwister()
    Array.init n (fun _ -> x.NextDouble())

let xs3 = random3 size3
let ys3 = random3 size3

let valuesInCircle3 = 
    Array.zip xs3 ys3
    |> Array.filter (fun (x,y) -> x**2. + y**2. < 1.)
    |> Async.Parallel          
    |> Async.RunSynchronously 


let numInCircle3 = valuesInCircle3 |> Array.length
let pi3 = 4. * (float numInCircle3) / (float size3)


// Using a different approach  

let size = 10000000
let scale = 100000000L
let scaleSqrd = scale * scale |> int64

let rnds n seed = 
    let x = System.Random (seed)
    Seq.init n (fun _ -> int64 (x.Next(0, int scale))) 

let xs = rnds size 3
let ys = rnds size 5

let valuesInCircle = 
    Seq.zip xs ys
    |> Seq.filter (fun (x,y) -> x*x + y*y <= scaleSqrd)

let numInCircle = valuesInCircle |> Seq.length
let pi = 4. * (float numInCircle) / (float size)

//1.2

[|1;2;3|] |> Array.length

Array.unfold


x**2 + y**2 = 1

/// Test 2 code

let size2 = 1000000

let rnds2 n seed = 
    let x = System.Random (seed)
    Array.init n (fun _ -> x.NextDouble())


let xs2 = rnds2 size2 3
let ys2 = rnds2 size2 5

let valuesInCircle2 = 
    Array.zip xs2 ys2
    |> Array.filter (fun (x,y) -> x**2. + y**2. < 1.)


let numInCircle2 = valuesInCircle2 |> Array.length
let pi2 = 4. * (float numInCircle2) / (float size2)




let size5 = 1000000
let scale5 = 10000

let rnds5 n seed = 
    let x = System.Random (seed)
    Array.init n (fun _ -> (x.Next(0, int scale5)))

let xs5 = rnds5 size 3
let ys5 = rnds5 size 5

let valuesInCircle5 = 
    Array.zip xs5 ys5
    |> Array.filter (fun (x,y) -> x*x + y*y <= 100000000)

let numInCircle5 = valuesInCircle5 |> Array.length
let pi = 4. * (float numInCircle5) / (float size)

  
let unfold<'T,'State> (f:'State -> ('T*'State) option) (s:'State) =
    let res = ResizeArray<_>()
    let rec loop state =
        match f state with
        | None -> ()
        | Some (x,s') ->
            res.Add(x)
            loop s'
    loop s
    res.ToArray()


fsi.AddPrinter(fun (x : System.DateTime) -> if x.TimeOfDay.TotalSeconds = 0. then x.ToString("yyyy.MM.dd") else x.ToString("yyyy-MM-dd h:mmt")) 

open System
let dt = DateTime.Today

let dts = 
    [|  for i = 1 to 9 do
            yield dt.AddDays((float i)*7.) |]
    |> Array.map (fun d -> d.ToLongDateString())


let f (t:DateTime) = 
    if t.DayOfWeek = DayOfWeek.Thursday then Some (t, t.AddDays(7.0)) else None

let xs =    Seq.unfold f dt
            |> Seq.take 100   
            |> Seq.toArray
DateTime.MaxValue
xs |> Seq.take 2






[|1..100|] |> Array.iter (printfn "Answer is: %s" << function
                | x when x%15=0 -> "CracklePop"
                | x when x%3=0  -> "Crackle"
                | x when x%5=0  -> "Pop"
                | x             -> string x )


[|1..100|] |> Array.map (function
                | x when x%15=0 -> "CracklePop"
                | x when x%3=0 -> "Crackle"
                | x when x%5=0 -> "Pop"
                | x -> string x) 
           |> Array.iter (printfn "%s")  



[|1..100|] |> Array.iter (function
                | x when x%15=0 -> "CracklePop"
                | x when x%3=0 -> "Crackle"
                | x when x%5=0 -> "Pop"
                | x -> string x 
                >> printfn "%s")









