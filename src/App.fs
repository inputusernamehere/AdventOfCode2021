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

open SyntaxHighlighterWrapper

type Tab =
  | Code
  | Problem
  | Explanation

let problem1 =
  """
--- Day 1: Report Repair ---

After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

To play, please identify yourself via one of these services:
  """

let codeSnippet =
  """
type Temp =
    | DegreesC of float
    | DegreesF of float

// Use one of the cases to create one
let temp1 = DegreesF 98.6
let temp2 = DegreesC 37.0

// Pattern match on all cases to unpack
let printTemp = function
   | DegreesC t -> printfn "%f degC" t
   | DegreesF t -> printfn "%f degF" t

let longLine = 126734926138632316461268123868123423486123468123468123462738462183746723468723461283462134
  """.Trim()

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
      Bulma.label "Run your code"
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

let codeView () =
  Html.div [
    prop.style [ style.display.flex ]
    prop.children [
      Html.div [
        prop.style [
          style.height 200
          style.backgroundColor "#f4f5f8"
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

  Bulma.section [
    prop.children [
      Bulma.container [
        prop.children [
          Bulma.title "Advent of Code 2021"

          dayView changeDay state.current.Day

          tabView changeTab state.current.Tab

          match state.current.Tab with
          | Code -> codeView ()
          | Problem ->
            Html.div [
              prop.style [
                style.whitespace.prewrap
                style.backgroundColor "#f4f5f8"
              ]
              prop.text problem1
            ]
          | Explanation -> ()
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