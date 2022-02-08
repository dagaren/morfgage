module ParsingCommands

open Commands
open FParsec
open Parsing

let pExit = stringCIReturn "exit" Exit
let pCalculateMortgageArguments = tuple3 (pint64 |>> decimal .>> spaces1) (pfloat .>> spaces1) (pint32)
let pCalculateMortgate = pstringCI "fixedMortgage" >>. spaces1 >>. pCalculateMortgageArguments |>> CalculateMortgage
let pShowAmortizationSchedule = stringCIReturn "AmortizationSchedule" ShowAmortizationSchedule

let pCommand = choice [
    attempt pExit
    attempt pCalculateMortgate
    attempt pShowAmortizationSchedule
]

let tryParseCommand str = tryRunParser pCommand str
