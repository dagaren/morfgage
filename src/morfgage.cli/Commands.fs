module Commands

type AppState = {
    text: string
}

type Command = 
| Exit

let initialState = {
    text = ""
}

let isExitCommand command =
    match command with
    | Exit -> true
    | _ -> false

let handleCommand state command =
    match command with
    | _ -> state