module ParsingCommands

open Commands
open FParsec
open Parsing

let pExit = stringCIReturn "exit" Exit

let pCommand = choice [
    attempt pExit
]

let tryParseCommand str = tryRunParser pCommand str
