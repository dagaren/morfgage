module Commands

open Types
open Functions

type AppState = {
    mortgage: FixedRateMortgage option
}

type Command = 
| Exit
| CalculateMortgage of principal:decimal * interest:float * years:int 
| ShowAmortizationSchedule

let initialState = {
    mortgage = None
}

let isExitCommand command =
    match command with
    | Exit -> true
    | _ -> false

let printMortgage mortgage =
    printfn ""
    printfn "Fixed rate mortgage"
    printfn "-------------------"
    printfn " - Principal: %M " (Amount.value mortgage.Principal)
    printfn " - Interest: %f" (Interest.value mortgage.Interest)
    printfn " - Term (months): %d " (Years.value mortgage.Term)
    printfn ""

let printMonthlyMortgage monthlyMortgage =
    printfn "Total = %M, Amort. = %M, Interest %M" (Amount.value monthlyMortgage.Total) (Amount.value monthlyMortgage.Amortization) (Amount.value monthlyMortgage.Interest)

let unfoldAmortizationRow (principal:Amount, interest, term) = 
    if Amount.value principal <= 0M then
        None
    else
        let monthlyMortgage = calcMonthlyMortgage principal interest term
        let pendingPrincipal = Amount.minus principal monthlyMortgage.Amortization

        let nextTerm = term - 1

        Some (monthlyMortgage, (pendingPrincipal, interest, nextTerm))

let calculateAmortizationTable mortgage =
    let years = Years.value mortgage.Term
    let months = years * 12
    (mortgage.Principal, mortgage.Interest, months)
    |> List.unfold unfoldAmortizationRow

let handleCalculateMortgate principal interest years state = 
    let mortgageOption = FixedRateMortgage.tryCreate principal interest years

    match mortgageOption with
    | Some mortgage ->
        printMortgage mortgage
        let amortizationSchedule = calculateAmortizationTable mortgage
        printfn " * Num quotas: %d" amortizationSchedule.Length
        printfn " * Total interest: %M" (amortizationSchedule |> List.map (fun x -> Amount.value x.Interest) |> List.sum)
        printfn " * Total: %M" (amortizationSchedule |> List.map (fun x -> Amount.value x.Total) |> List.sum)
        printfn " * Total a: %M" (amortizationSchedule |> List.map (fun x -> Amount.value x.Amortization) |> List.sum)
        
        { state with mortgage = (Some mortgage) }
    | None -> 
        printfn "Invalid data for creating mortgage"
        state

let handleShowAmortizationSchedule state =
    match state.mortgage with
    | Some mortgage ->
        let amortizationSchedule = calculateAmortizationTable mortgage
        printfn ""
        printfn " - Amortization schedule" 
        amortizationSchedule |> List.iter printMonthlyMortgage
        printfn " * Num quotas: %d" amortizationSchedule.Length
        printfn " * Total interest: %M" (amortizationSchedule |> List.map (fun x -> Amount.value x.Interest) |> List.sum)
        printfn " * Total: %M" (amortizationSchedule |> List.map (fun x -> Amount.value x.Total) |> List.sum)
        state
    | None ->
        printfn ""
        printfn "There are no mortgage loaded"
        printfn ""
        state

let handleCommand state command =
    match command with
    | CalculateMortgage (principal, interest, years) -> handleCalculateMortgate principal interest years state
    | ShowAmortizationSchedule -> handleShowAmortizationSchedule state
    | _ -> state