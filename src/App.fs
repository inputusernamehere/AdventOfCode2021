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

let dayBox (i : string) =
  Html.div [
    Html.p i
  ]

let dayBoxes () =
  [ 1 .. 25 ]
  |> List.map (string >> dayBox)

let dayView () =
  Html.div [
    prop.children (dayBoxes())
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

let App = FunctionComponent.Of<unit> (fun model ->
  let state = Hooks.useState model

  Bulma.section [
    prop.children [
      Bulma.container [
        prop.children [
          Bulma.title "Advent of Code 2021"

          SyntaxHighlighter.input [
            SyntaxHighlighter.language "fsharp"
            SyntaxHighlighter.showLineNumbers true

            SyntaxHighlighter.style SyntaxHighlighterWrapper.vs2015

            prop.children [
              Html.text codeSnippet
            ]
          ]

          tabbedView ()

          inputView ()

          dayView ()
        ]
      ]
    ]
  ]
)

let render() =
  ReactDom.render(
    App (),
    document.getElementById("ReactEntryPoint"))

render()