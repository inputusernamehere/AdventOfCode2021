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
      //.Add(Example.example.Day, Example.example)
      .Add(Day1.data.Day, Day1.data)
      .Add(Day2.data.Day, Day2.data)
      .Add(Day3.data.Day, Day3.data)
      .Add(Day4.data.Day, Day4.data)
      .Add(Day5.data.Day, Day5.data)
      .Add(Day6.data.Day, Day6.data)
      .Add(Day7.data.Day, Day7.data)
      .Add(Day8.data.Day, Day8.data)
      .Add(Day9.data.Day, Day9.data)
      .Add(Day10.data.Day, Day10.data)
      .Add(Day11.data.Day, Day11.data)
      .Add(Day12.data.Day, Day12.data)
      .Add(Day13.data.Day, Day13.data)
      .Add(Day14.data.Day, Day14.data)
      .Add(Day15.data.Day, Day15.data)
      .Add(Day16.data.Day, Day16.data)
      .Add(Day17.data.Day, Day17.data)
      .Add(Day18.data.Day, Day18.data)
      .Add(Day19.data.Day, Day19.data)
      .Add(Day20.data.Day, Day20.data)
      .Add(Day21.data.Day, Day21.data)
      .Add(Day22.data.Day, Day22.data)
      .Add(Day23.data.Day, Day23.data)
      .Add(Day24.data.Day, Day24.data)
      .Add(Day25.data.Day, Day25.data)

  let initialState = {
    Day = 1
    Tab = Code

    Problems = problems
    Problems2 = Map.empty
    Inputs = Map.empty
    Answers = Map.empty
  }

  //problems |> Map.iter (fun k v -> printfn "Problem: %i" k)

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
        then c.Part1Code (input.Trim())
        else c.Part2Code (input.Trim())
      
      model, Cmd.ofMsg (UpdateAnswer (section, answer))
    | None ->
      model, Cmd.none

let lights () =
  Html.div [
    prop.className "lights"

    prop.children [
      Html.ul [
        prop.className "line"

        prop.children [
          Html.li [ prop.classes [ "line-item"; "red" ] ]
          Html.li [ prop.classes [ "line-item"; "yellow" ] ]
          Html.li [ prop.classes [ "line-item"; "blue" ] ]
          Html.li [ prop.classes [ "line-item"; "pink" ] ]
          Html.li [ prop.classes [ "line-item"; "red" ] ]
          Html.li [ prop.classes [ "line-item"; "green" ] ]
          Html.li [ prop.classes [ "line-item"; "blue" ] ]
          Html.li [ prop.classes [ "line-item"; "yellow" ] ]
          Html.li [ prop.classes [ "line-item"; "red" ] ]
          Html.li [ prop.classes [ "line-item"; "pink" ] ]
          Html.li [ prop.classes [ "line-item"; "green" ] ]
          Html.li [ prop.classes [ "line-item"; "yellow" ] ]
          Html.li [ prop.classes [ "line-item"; "blue" ] ]
          Html.li [ prop.classes [ "line-item"; "red" ] ]
          Html.li [ prop.classes [ "line-item"; "pink" ] ]
          Html.li [ prop.classes [ "line-item"; "yellow" ] ]
          Html.li [ prop.classes [ "line-item"; "pink" ] ]
          Html.li [ prop.classes [ "line-item"; "blue" ] ]
          Html.li [ prop.classes [ "line-item"; "red" ] ]
          Html.li [ prop.classes [ "line-item"; "green" ] ]
          Html.li [ prop.classes [ "line-item"; "blue" ] ]
        ]
      ]
    ]
  ]

let view (model : Model) (dispatch : Msg -> unit) =
  let snowflake = Html.div [ prop.className "snowflake"; prop.text "❅" ]

  Html.div [
    prop.style [
      style.paddingRight (length.rem 3.0)
      style.paddingLeft (length.rem 3.0)
      style.paddingTop (length.rem 1.5)
      style.paddingBottom (length.rem 1.5)
    ]

    prop.children [
      lights ()

      Html.h1 [
        prop.style [
          style.fontFamily "ThePerfectChristmas"
          style.fontWeight 500
          style.fontSize (length.em 6)
        ]

        prop.text "Advent of Code 2021"
      ]

      dayView model dispatch

      tabView model dispatch

      match model.Tab with
      | Code -> codeView model dispatch
      | Problem -> problemView model dispatch
      //| Explanation -> explanationView model dispatch

      for _ in [ 1 .. 50 ] do snowflake

      Html.div [ prop.id "snow-blur" ]
    ]
  ]