module Index

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
open SyntaxHighlighterWrapper
open DayView
open InputView
open CodeView
open ProblemView
open ExplanationView
open TabView

let init () =
  let problems =
    Map.empty
      .Add(Example.example.Day, Example.example)

  let initialState = {
    Day = 1
    Tab = Code

    Problems = problems
    Problems2 = Map.empty
    Inputs = Map.empty
    Answers = Map.empty
  }

  initialState, Cmd.none

let update msg (model : Model) =
  match msg with
  | ChangeDay day ->
    { model with Day = day }, Cmd.none
  | ChangeTab tab ->
    { model with Tab = tab }, Cmd.none
  | UpdateInput (section, value) ->
    let newInputs =
      model.Inputs
      |> Map.add section value

    { model with Inputs = newInputs }, Cmd.none
  | UpdateAnswer (section, value) ->
    let newAnswers =
      model.Answers
      |> Map.add section value

    { model with Answers = newAnswers }, Cmd.none
  | RunCode (section, input) ->
    let code =
      model.Problems
      |> Map.tryFind section.Day

    match code with
    | Some c ->
      let answer =
        if section.Part = 1
        then c.Part1Code input
        else c.Part2Code input
      
      model, Cmd.ofMsg (UpdateAnswer (section, answer))
    | None ->
      model, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
  Html.div [
    prop.style [
      style.paddingRight (length.rem 3.0)
      style.paddingLeft (length.rem 3.0)
      style.paddingTop (length.rem 1.5)
    ]

    prop.children [
      Html.h1 [
        prop.style [
          style.fontSize (length.rem 3)
        ]

        prop.text "Advent of Code 2021"
      ]

      dayView model dispatch

      tabView model dispatch

      match model.Tab with
      | Code -> codeView model dispatch
      | Problem -> problemView model dispatch
      | Explanation -> explanationView model dispatch
    ]
  ]