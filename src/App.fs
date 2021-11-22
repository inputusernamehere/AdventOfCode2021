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

let inputView () =
  Html.form [
    Bulma.field.div [
      Bulma.label "Run code"
      Bulma.control.div [
        Bulma.input.text [
          prop.required true
          prop.placeholder "Your input"
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
          ]
        ]
      ]
    ]
  ]

let codeView (problem : Problem option) =
  let partContent (codeSnippet : string) =
    Html.div [
      prop.style [ style.display.flex ]
      prop.children [
        Html.div [
          prop.style [
            style.flexBasis (length.em 20)
            style.flexShrink 0
          ]

          prop.children [
            inputView ()
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
      | Some p -> partContent (p.Part1Code)
      | None -> ()

      Divider.divider [
        divider.text "Part 2"
      ]

      match problem with
      | Some p -> partContent (p.Part2Code)
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

let tabView (changeTab : Tab -> unit) (currentTab : Tab) =
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
}

let App = FunctionComponent.Of<AppState> (fun model ->
  let state = Hooks.useState model

  let changeDay n =
    let newValue =
      match n with
      | x when x < 0 -> 0
      | x when x > 25 -> 25
      | x -> x

    state.update { state.current with Day = newValue }

  let changeTab t =
    state.update { state.current with Tab = t }

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
          | Code -> codeView (currentProblem ())
          | Problem -> problemView (currentProblem ())
          | Explanation -> explanationView (currentProblem ())
        ]
      ]
    ]
  ]
)

let render() =
  ReactDom.render(
    App { Day = 1; Tab = Code },
    document.getElementById("ReactEntryPoint"))

render()