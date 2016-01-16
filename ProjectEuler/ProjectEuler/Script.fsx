
//Problem 1

[|1..999|] |> Array.sumBy (fun x -> if x%3=0 || x%5=0 then x else 0)

//Problem 2
    
let fib1 = Seq.unfold (fun state ->
     if (snd state > 4000000) then None
     else Some(fst state + snd state, (snd state, fst state + snd state))) (0,1) |> Seq.sumBy (fun x -> if x%2=0 then x else 0)

//Problem 3



    


//Problem of calculating pi using random number generartor  (not part of project euler)

#I "..\packages"
#r @"MathNet.Numerics.3.10.0\lib\net40\MathNet.Numerics.dll"

open MathNet.Numerics.Random
open MathNet.Numerics.Distributions

#time "on"
fsi.ShowDeclarationValues <- true

// todo: Ask Karl how to use parallelism  

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









