module TabView

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

let tabButtonStyle = fss [
  Height.value (px 40)
  PaddingLeft.value (em 1.0)
  PaddingRight.value (em 1.0)
  BorderWidth.value (px 0)
  BorderRadius.value (px 4)
  Color.white
]

let tabView model dispatch =
  Html.div [
    prop.children [
      Html.ul [
        prop.style [
          style.display.flex
          style.listStyleType.none
          style.padding 0
        ]

        prop.children [
          Html.li [
            Html.button [
              prop.className tabButtonStyle

              prop.style [
                if model.Tab = Code
                then
                  style.backgroundColor "#485fc7"
                else
                  style.backgroundColor "#0f0f23"
              ]
              prop.text "Code"
              prop.onClick <| fun _ -> dispatch (ChangeTab Code)
            ]
          ]

          Html.li [
            Html.button [
              prop.className tabButtonStyle

              prop.style [
                if model.Tab = Problem
                then
                  style.backgroundColor "#485fc7"
                else
                  style.backgroundColor "#0f0f23"
              ]
              prop.text "Problem"
              prop.onClick <| fun _ -> dispatch (ChangeTab Problem)
            ]
          ]

          Html.li [
            Html.button [
              prop.className tabButtonStyle

              prop.style [
                if model.Tab = Explanation
                then
                  style.backgroundColor "#485fc7"
                else
                  style.backgroundColor "#0f0f23"
              ]
              prop.text "Explanation"
              prop.onClick <| fun _ -> dispatch (ChangeTab Explanation)
            ]
          ]
        ]
      ]
    ]
  ]