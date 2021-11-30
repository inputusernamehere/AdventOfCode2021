module InputView

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

open BaseTypes

let inputView part model dispatch =
  let section = { Section.Day = model.Day; Part = part }
  let input =
    model.Inputs
    |> Map.tryFind section
    |> Option.defaultValue ""

  let answer =
    model.Answers
    |> Map.tryFind section
    |> Option.defaultValue ""

  let code =
    model.Problems
    |> Map.tryFind model.Day

  Html.div [
    prop.style [
      style.marginRight (length.em 1)
    ]
    prop.children [
      Html.form [
        Html.div [
          Html.p [
            prop.style [
              style.paddingBottom (length.rem 1)
            ]
            prop.text "Run code"
          ]

          Html.textarea [
            prop.style [
              style.fontFamily "Source Code Pro"
              style.backgroundColor "rgb(30, 30, 30)"
              style.color.white
              style.height (length.rem 8)
              style.width (length.percent 100)
              style.borderRadius (length.px 4)
              style.resize.vertical
            ]

            prop.required true
            prop.placeholder "Your input"

            prop.value input
            prop.onTextChange <| fun value -> 
              dispatch (UpdateInput (section, value))
          ]
        ]

        Html.div [
          prop.style [
            style.display.flex
            style.justifyContent.center
          ]
          prop.children [
            Html.div [
              Html.button [
                prop.style [
                  style.backgroundColor "#485fc7"
                  style.borderWidth 0
                  style.borderRadius 4
                  style.padding (length.em 1)
                  style.color.white
                ]
                prop.text "Submit"
                prop.onClick <| fun e ->
                  e.preventDefault()

                  dispatch (RunCode (section, input))
              ]
            ]
          ]
        ]
      ]

      Html.p [
        if answer <> ""
        then prop.text ("Answer: " + answer)
      ]
    ]
  ]
