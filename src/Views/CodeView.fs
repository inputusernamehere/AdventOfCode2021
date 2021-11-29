module CodeView

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
open BaseTypes
open InputView

let codeView model dispatch =
  let problem =
    model.Problems
    |> Map.tryFind model.Day

  let partContent
    (codeSnippet : string)
    (part : int) =
    if codeSnippet = ""
    then
      Html.div []
    else
      Html.div [
        prop.style [ style.display.flex ]
        prop.children [
          Html.div [
            prop.style [
              style.flexBasis (length.em 20)
              style.flexShrink 0
            ]

            prop.children [
              inputView part model dispatch
            ]
          ]
          Html.div [
            prop.style [
              style.minHeight 200
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
      | Some p -> partContent (p.Part1CodeString) 1
      | None -> ()

      Divider.divider [
        divider.text "Part 2"
      ]

      match problem with
      | Some p -> partContent (p.Part2CodeString) 2
      | None -> ()
    ]
  ]