open ParsingCommands
open Commands

let readCommandSeq = seq {
    while true do
        yield (System.Console.ReadLine())
}

readCommandSeq 
|> Seq.choose tryParseCommand
|> Seq.takeWhile (not << isExitCommand)
|> Seq.fold handleCommand initialState
|> ignore