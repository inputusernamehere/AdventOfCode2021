module ProblemView

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.Router
open Fss
open Fss.FssTypes
open Browser.Dom

open BaseTypes

let problemView model dispatch =
  let problem =
    model.Problems
    |> Map.tryFind model.Day

  let problemStatement1 =
    match problem with
    | Some p -> p.Part1Problem
    | None -> ""

  let problemStatement2 =
    match problem with
    | Some p -> p.Part2Problem
    | None -> ""

  Html.div [
    prop.style [
      style.whitespace.prewrap
    ]

    prop.children [
      Divider.divider [
        divider.text "Part 1"
      ]

      Html.p problemStatement1

      Divider.divider [
        divider.text "Part 2"
      ]

      Html.p problemStatement2
    ]
  ]