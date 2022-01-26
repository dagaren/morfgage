module Parsing

open FParsec

let runParser parser str = 
    match run parser str with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error (sprintf "Invalid input: %s" errorMsg)

let tryRunParser parser str = 
    match run  parser str with
    | Success(result, _, _)   -> Some result
    | Failure(errorMsg, _, _) -> 
        printfn "Invalid command: %s" str
        None