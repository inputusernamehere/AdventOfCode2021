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
              if model.Tab = Code
              then prop.className "tab-button is-active"
              else prop.className "tab-button"

              prop.text "Code"
              prop.onClick <| fun _ -> dispatch (ChangeTab Code)
            ]
          ]

          Html.li [
            Html.button [
              if model.Tab = Problem
              then prop.className "tab-button is-active"
              else prop.className "tab-button"

              prop.text "Problem"
              prop.onClick <| fun _ -> dispatch (ChangeTab Problem)
            ]
          ]

          (*
          Html.li [
            Html.button [
              if model.Tab = Explanation
              then prop.className "tab-button is-active"
              else prop.className "tab-button"

              prop.text "Explanation"
              prop.onClick <| fun _ -> dispatch (ChangeTab Explanation)
            ]
          ]
          *)
        ]
      ]
    ]
  ]