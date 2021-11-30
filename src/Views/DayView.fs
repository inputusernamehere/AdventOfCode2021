module DayView

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

let dayView (model : Model) dispatch =
  let dayBox (i : int) =
    Html.button [
      if model.Day = i
      then prop.className "day-button is-active"
      else prop.className "day-button"

      prop.text (string i)

      prop.onClick <| fun _ -> dispatch (ChangeDay i)
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
      Html.h2 [
        prop.style [
          style.width (length.percent 100)
          style.fontSize (length.rem 2)
        ]
        prop.text "Choose a day:"
      ]

      Html.div [
        prop.children (dayBoxes ())
      ]
    ]
  ]
