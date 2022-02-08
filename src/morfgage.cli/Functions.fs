module Functions

open Types
open System

let calcMonthlyPayment principal interest term =
    let i = Interest.value interest / 12.0
    let principalValue = Amount.value principal

    let numerator = decimal ((1.0 + i)**(float term))
    let denominator = decimal ((1.0 + i)**(float term) - 1.0)

    let monthlyPayment = principalValue * (decimal i) * (numerator / denominator)

    Amount (Math.Round(monthlyPayment, 2))

let calcMonthlyInterest principal interest = 
    let interestValue = Interest.value interest
    let principalValue = Amount.value principal

    let i = decimal(interestValue / 12.0)

    Amount (Math.Round(i * principalValue, 2))

let calcMonthlyMortgage principal interest term =
    let total = calcMonthlyPayment principal interest term
    let interest = calcMonthlyInterest principal interest
    let amortization = Amount.minus total interest

    {
        Total = total
        Amortization = amortization
        Interest = interest
    }

