type LogginBuilder() = 
    let log p = printfn "expression is %A" p

    member this.Bind(x ,f) =
        log x
        f x
    
    member this.Return(x) = 
        x

let logger = new LogginBuilder()
let loggedWorkflow = 
    logger {
        let! x = 32
        let! y = 12
        let! z = x + y
        return z 
    }

let devideBy bottom top = 
    if bottom = 0
    then None
    else Some (top/bottom)

type MaybeBuilder() = 
    
    member this.Bind(x, f) = 
       x |> Option.bind(f)

    member this.Return(x) =
        Some x

let maybe = new MaybeBuilder()

let devideByWorkflow init x y z = 
    maybe {
        let! a = init |> devideBy x 
        let! b = a |> devideBy y
        let! c = b |> devideBy z
        return c
    }

let good = devideByWorkflow 12 3 2 1
let bad = devideByWorkflow 12 3 0 1

let strToInt (text:string) =
    try
        let result = System.Convert.ToInt32 text
        Ok result
    with
    | _ -> Error "Not a number"

type StrToIntBuilder() = 
    
    member this.Bind(x, f) =
        x |> Result.bind(f)

    member this.Return(x) = 
        Ok x

let converterIntString = new StrToIntBuilder()

let stringAddWorkflow x y z =
    converterIntString{
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z

        return a + b + c
    }

let good1= stringAddWorkflow "12" "3" "2"
let bad1 = stringAddWorkflow "12" "xyz" "2"

let strAdd str i = 
    i |> Result.bind(fun a -> converterIntString {let! c = strToInt str
                                                  return c + a})
let (>>=) m f = m |> f

let good2 = strToInt "1" >>= strAdd "2" >>= strAdd "3"
let bad2 = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"