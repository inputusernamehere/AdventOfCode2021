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
      Bulma.label "Label"
      Bulma.control.div [
        Bulma.input.text [
          prop.required true
          prop.placeholder "Placeholder"
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

let tabbedView () =
  Bulma.tabs [
    tabs.isBoxed
    prop.children [
      Html.ul [
        Bulma.tab [
          tab.isActive
          prop.children [
            Html.a [
              prop.text "Run Code"
              prop.href "#"
            ]
          ]
        ]

        Bulma.tab [
          Html.a [
            prop.text "View Code"
            prop.href "#"
          ]
        ]
      ]
    ]
  ]

type AppState = {
  Day : int
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

  Bulma.section [
    prop.children [
      Bulma.container [
        prop.children [
          Bulma.title "Advent of Code 2021"

          dayView changeDay state.current.Day

          fsSnippet codeSnippet

          tabbedView ()

          inputView ()
        ]
      ]
    ]
  ]
)

let render() =
  ReactDom.render(
    App { Day = 1 },
    document.getElementById("ReactEntryPoint"))

render()