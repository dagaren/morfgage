module Types

open FsToolkit.ErrorHandling

type Interest = private Interest of float

module Interest = 
    let tryCreate (value:float) : Option<Interest> =
        if value > 1.0 then
            None
        else if value < 0.0 then
            None
        else
            Some (Interest value)
    
    let value (Interest value) = value 

type Years = private Years of int

module Years = 
    let create (value:int) : Option<Years> = 
        if value < 0 then
            None
        else
            Some (Years value)

    let unsafeCreate (value:int) : Years = Years value

    let value (Years value) = value

type Quotas = private Quotas of int

module Quotas = 
    let create (value:int) : Option<Quotas> =
        if value < 0 then
            None
        else
            Some (Quotas value)

    let value (Quotas value) = value

type Amount = Amount of decimal

module Amount = 
    let value (Amount value) = value
    let minus (Amount xValue) (Amount yValue) = Amount(xValue - yValue)

    let unsafeCreate (value:decimal) : Amount = Amount value

    let tryCreate (value:decimal) : Option<Amount> = 
        if value < 0M then
            None
        else
            Some (Amount value)

type FixedRateMortgage = {
    Principal: Amount
    Interest: Interest
    Term: Years
}

module FixedRateMortgage =
    let tryCreate principalVal interestVal termVal = option {
        let! principal = Amount.tryCreate principalVal
        let! interest = Interest.tryCreate interestVal
        let! term = Years.create termVal

        return {
            Principal = principal
            Interest = interest
            Term = term
        }
    }

type MonthlyMortgage = {
    Total: Amount
    Amortization: Amount
    Interest: Amount   
}