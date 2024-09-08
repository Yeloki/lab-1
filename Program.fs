open System

let cosDumb x =
    let rec taylorSeries term n sum =
        if abs term < 1e-10 then sum
        else
            let newTerm = term * (-x * x) / float (2 * n * (2 * n - 1))
            taylorSeries newTerm (n + 1) (sum + term)
    taylorSeries 1.0 1 0.0
    
let cosSmart x =
    let rec taylorSeries term n sum =
        if abs term < 1e-10 then (sum, n)
        else
            let newTerm = term * (-x * x) / float (2 * n * (2 * n - 1))
            taylorSeries newTerm (n + 1) (sum + term)
    taylorSeries 1.0 1 0.0

let printTable a b n =
    let step = (b - a) / float (n - 1)
    printfn "%-10s %-20s %-10s %-20s %-10s %-20s" "x" "dumb" "#" "smart" "#" "value"
    [0 .. n-1]
    |> List.map (fun i -> a + step * float i)
    |> List.iter (fun x ->
        let valueDumb = cosDumb x
        let valueSmart, iterationsSmart = cosSmart x
        printfn "%-10f %-20.10f %-10s %-20.10f %-10d %-20.10f" x valueDumb "N/A" valueSmart iterationsSmart (Math.Cos(x))
    )

let func x = 0.4 + Math.Atan(Math.Sqrt(x)) - x

let funcDerivative x = (1.0 / (2.0 * Math.Sqrt(1.0 + x))) - 1.0

let iterativeMethod f x0 tol maxIter =
    let rec iterate x iter =
        let xNew = f x
        if iter >= maxIter || abs (xNew - x) < tol then
            (xNew, iter)
        else
            iterate xNew (iter + 1)
    iterate x0 0

let bisectionMethod f a b tol =
    let rec bisect a b iter =
        let mid = (a + b) / 2.0
        if abs (b - a) < tol then
            (mid, iter)
        else
            if f(mid) * f(a) < 0.0 then
                bisect a mid (iter + 1)
            else
                bisect mid b (iter + 1)
    bisect a b 0

let newtonMethod f fPrime x0 tol maxIter =
    let rec newton x iter =
        let xNew = x - (f x / fPrime x)
        if iter >= maxIter || abs (xNew - x) < tol then
            (xNew, iter)
        else
            newton xNew (iter + 1)
    newton x0 0

[<EntryPoint>]
let main argv =
    let a = 0.0
    let b = 1
    let n = 10

    printfn "\nfirst task:\n"
    printTable a b n

    printfn "\nsecond task:\n"

    let a2 = 1.0
    let b2 = 2.0
    let tol = 1e-6
    let maxIter = 1000

    let phi x = 0.4 + Math.Atan(Math.Sqrt(x))
    let iterResult, iterCount = iterativeMethod phi a2 tol maxIter
    printfn "Iterative method: x = %f, iterations = %d" iterResult iterCount

    let bisectResult, bisectCount = bisectionMethod func a2 b2 tol
    printfn "Bisection method: x = %f, iterations = %d" bisectResult bisectCount

    let newtonResult, newtonCount = newtonMethod func funcDerivative a2 tol maxIter
    printfn "Newton method: x = %f, iterations = %d" newtonResult newtonCount

    0
