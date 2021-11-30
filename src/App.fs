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

open BaseTypes
open SyntaxHighlighterWrapper
open DayView
open InputView
open CodeView
open ProblemView
open ExplanationView
open TabView

#if DEBUG
open Elmish.Debug
open Elmish.HMR
open Fable.Core.JsInterop
#endif

importAll "../public/snowflake.scss"

Program.mkProgram Index.init Index.update Index.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "ReactEntryPoint"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
