module SyntaxHighlighterWrapper

open System
open Fable.Core.JsInterop
open Feliz

// https://github.com/react-syntax-highlighter/react-syntax-highlighter
// https://www.compositional-it.com/news-blog/working-with-react-components-in-fsharp/

let syntaxHighlighter : obj = import "default" "react-syntax-highlighter"

let docco : obj = import "docco" "react-syntax-highlighter/dist/esm/styles/hljs"
let vs2015 : obj = import "vs2015" "react-syntax-highlighter/dist/esm/styles/hljs"

type SyntaxHighlighter =
  static member inline value (code : string) = prop.custom ("value", code)
  static member inline showLineNumbers (lineNumbers : bool) = prop.custom ("showLineNumbers", lineNumbers)
  static member inline language (lang : string) = prop.custom ("language", lang)
  static member inline style (s : obj) = prop.custom ("style", s)
  static member inline input props = Interop.reactApi.createElement (syntaxHighlighter, createObj !!props)

type Language =
  | OCaml
  | FSharp

let snippet (language : Language) (snippet : string) =
  let languageString =
    match language with
    | OCaml -> "ocaml"
    | FSharp -> "fsharp"

  SyntaxHighlighter.input [
    SyntaxHighlighter.language languageString
    SyntaxHighlighter.showLineNumbers true

    SyntaxHighlighter.style vs2015

    prop.children [
      Html.text snippet
    ]
  ]

let fsSnippet = snippet FSharp
let ocamlSnipper = snippet OCaml