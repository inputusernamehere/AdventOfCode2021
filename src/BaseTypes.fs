module BaseTypes

type Tab =
  | Code
  | Problem
  //| Explanation

type Section = {
  Day : int
  Part : int
}

type SectionData = {
  Code : string -> string
  CodeString : string
  Problem : string
  Explanation : string
}

type Problem = {
  Day : int

  Part1Code : string -> string
  Part2Code : string -> string

  Part1CodeString : string
  Part2CodeString : string

  Part1Problem : string
  Part2Problem : string

  Part1Explanation : string
  Part2Explanation : string

  Part1Language : SyntaxHighlighterWrapper.Language
  Part2Language : SyntaxHighlighterWrapper.Language
}

type Model = {
  Day : int
  Tab : Tab
  Problems : Map<int, Problem>
  Problems2 : Map<Section, Problem>
  Inputs : Map<Section, string>
  Answers : Map<Section, string>
}

type Msg =
  | ChangeDay of int
  | ChangeTab of Tab
  | UpdateInput of Section * string
  | UpdateAnswer of Section * string
  | RunCode of Section * string

