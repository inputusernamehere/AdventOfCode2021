module App

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

open Problem
open SyntaxHighlighterWrapper

type Tab =
  | Code
  | Problem
  | Explanation

let dayBoxStyle = fss [
  Height.value (px 40)
  Width.value (px 40)
]

let dayView (changeDayFn : int -> unit) (currentDay : int) =
  let dayBox (i : int) =
    let color () =
      if currentDay = i
      then color.isInfo
      else color.isWhite
    Bulma.button.button [
      prop.className dayBoxStyle
      color ()

      prop.text (string i)

      prop.onClick <| fun _ -> changeDayFn i
    ]

  let dayBoxes () =
    [ 1 .. 25 ]
    |> List.map dayBox

  Html.div [
    prop.style [
      style.display.flex
      style.flexWrap.wrap
    ]
    prop.children [
      Bulma.subtitle [
        prop.style [
          style.width (length.percent 100)
        ]
        prop.text "Choose a day:"
      ]

      Html.div [
        prop.children (dayBoxes ())
      ]
    ]
  ]

let inputView
  (code : string -> string)
  (changeInput : string -> unit)
  (currentInput : string)
  (changeResult : string -> unit)
  (currentResult : string) =

  Html.div [
    prop.children [
      Html.form [
        Bulma.field.div [
          Bulma.label "Run code"
          Bulma.control.div [
            Bulma.input.text [
              prop.required true
              prop.placeholder "Your input"
              prop.value currentInput
              prop.onTextChange <| fun v -> changeInput v
            ]
          ]
        ]

        Bulma.field.div [
          field.isGrouped
          field.isGroupedCentered
          prop.children [
            Bulma.control.div [
              Bulma.button.button [
                color.isLink
                prop.text "Submit"
                prop.onClick <| fun e ->
                  e.preventDefault()
                  let result = (code currentInput)
                  changeResult result
              ]
            ]
          ]
        ]
      ]

      Html.p currentResult
    ]
  ]

let codeView
  (problem : Problem option)
  (changePart1Input : string -> unit)
  (currentPart1Input : string)
  (changePart2Input : string -> unit)
  (currentPart2Input : string)
  (changePart1Result : string -> unit)
  (currentPart1Result : string)
  (changePart2Result : string -> unit)
  (currentPart2Result : string) = 
  let partContent
    (codeSnippet : string)
    (code : string -> string)
    (part : int) =
    Html.div [
      prop.style [ style.display.flex ]
      prop.children [
        Html.div [
          prop.style [
            style.flexBasis (length.em 20)
            style.flexShrink 0
          ]

          prop.children [
            if part = 1
            then
              inputView
                code
                changePart1Input
                currentPart1Input
                changePart1Result
                currentPart1Result
            else
              inputView
                code
                changePart2Input
                currentPart2Input
                changePart2Result
                currentPart2Result
          ]
        ]
        Html.div [
          prop.style [
            style.height 200
            style.flexGrow 1
          ]

          prop.children [
            fsSnippet codeSnippet
          ]
        ]
      ]
    ]

  Html.div [
    prop.children [
      Divider.divider [
        divider.text "Part 1"
      ]

      match problem with
      | Some p -> partContent (p.Part1Code) (p.Part1Solution) 1
      | None -> ()

      Divider.divider [
        divider.text "Part 2"
      ]

      match problem with
      | Some p -> partContent (p.Part2Code) (p.Part2Solution) 2
      | None -> ()
    ]
  ]

let problemView (problem : Problem option) =
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

let explanationView (problem : Problem option) =
  let explanation1 =
    match problem with
    | Some p -> p.Part1Explanation
    | None -> ""

  let explanation2 =
    match problem with
    | Some p -> p.Part2Explanation
    | None -> ""

  Html.div [
    prop.style [
      style.whitespace.prewrap
    ]

    prop.children [
      Divider.divider [
        divider.text "Part 1"
      ]

      Html.p explanation1

      Divider.divider [
        divider.text "Part 2"
      ]

      Html.p explanation2
    ]
  ]

let tabView
  (changeTab : Tab -> unit)
  (currentTab : Tab) =
  Bulma.tabs [
    tabs.isBoxed
    prop.children [
      Html.ul [
        Bulma.tab [
          if currentTab = Code then tab.isActive
          prop.children [
            Html.a [
              prop.text "Code"
              prop.onClick <| fun _ -> (changeTab Code)
            ]
          ]
        ]

        Bulma.tab [
          if currentTab = Problem then tab.isActive
          prop.children [
            Html.a [
              prop.text "Problem"
              prop.onClick <| fun _ -> (changeTab Problem)
            ]
          ]
        ]

        Bulma.tab [
          if currentTab = Explanation then tab.isActive
          prop.children [
            Html.a [
              prop.text "Explanation"
              prop.onClick <| fun _ -> (changeTab Explanation)
            ]
          ]
        ]
      ]
    ]
  ]

type AppState = {
  Day : int
  Tab : Tab
  Part1Input : string
  Part2Input : string
  Part1Result : string
  Part2Result : string
}

let App = FunctionComponent.Of<AppState> (fun model ->
  let state = Hooks.useState model

  let changeDay n =
    let newValue =
      match n with
      | x when x < 0 -> 0
      | x when x > 25 -> 25
      | x -> x

    state.update { state.current with Day = newValue; Part1Result = ""; Part2Result = "" }

  let changeTab t =
    state.update { state.current with Tab = t }

  let changePart1Input i =
    state.update { state.current with Part1Input = i }

  let changePart2Input i =
    state.update { state.current with Part2Input = i }

  let changePart1Result r =
    state.update { state.current with Part1Result = r }

  let changePart2Result r =
    state.update { state.current with Part2Result = r }

  let problems =
    Map.empty
      .Add (Example.example.Day, Example.example)

  let currentProblem () =
    problems
    |> Map.tryFind state.current.Day

  Bulma.section [
    prop.children [
      Bulma.container [
        prop.children [
          Bulma.title "Advent of Code 2021"

          dayView changeDay state.current.Day

          tabView changeTab state.current.Tab

          match state.current.Tab with
          | Code ->
            codeView
              (currentProblem ())
              changePart1Input
              state.current.Part1Input
              changePart2Input
              state.current.Part2Input
              changePart1Result
              state.current.Part1Result
              changePart2Result
              state.current.Part2Result
          | Problem -> problemView (currentProblem ())
          | Explanation -> explanationView (currentProblem ())
        ]
      ]
    ]
  ]
)

let render() =
  ReactDom.render(
    App {
      Day = 1
      Tab = Code
      Part1Input = ""
      Part2Input = ""
      Part1Result = ""
      Part2Result = ""
    },
    document.getElementById("ReactEntryPoint"))

render()