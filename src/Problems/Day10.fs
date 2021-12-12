module Day10

open System
open System.Collections.Generic
open SyntaxHighlighterWrapper

let part1Code (input : string) =
  let left = [|'['; '('; '{'; '<'|]
  let right = [|']'; ')'; '}'; '>'|]

  let rec scoreLine (stack : Stack<char>) sum line =
    match line with
    | [] -> sum
    | x :: xs ->
      match x with
      | l when (Array.contains l left) ->
        stack.Push(l)
        scoreLine stack sum xs
      | r when (Array.contains r right) ->
        let top = stack.Pop()
        let topIndex = Array.IndexOf(left, top)
        let rIndex = Array.IndexOf(right, r)

        if topIndex = rIndex
        then scoreLine stack sum xs
        else
          let score =
            match r with
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | '>' -> 25137
          scoreLine stack (sum + score) xs

  (input.Split(Environment.NewLine))
  |> Array.map Seq.toList
  |> Array.map (scoreLine (new Stack<char>()) 0)
  |> Array.sum
  |> string
  
let part1CodeString =
  """
let part1Code (input : string) =
  let left = [|'['; '('; '{'; '<'|]
  let right = [|']'; ')'; '}'; '>'|]

  let rec scoreLine (stack : Stack<char>) sum line =
    match line with
    | [] -> sum
    | x :: xs ->
      match x with
      | l when (Array.contains l left) ->
        stack.Push(l)
        scoreLine stack sum xs
      | r when (Array.contains r right) ->
        let top = stack.Pop()
        let topIndex = Array.IndexOf(left, top)
        let rIndex = Array.IndexOf(right, r)

        if topIndex = rIndex
        then scoreLine stack sum xs
        else
          let score =
            match r with
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | '>' -> 25137
          scoreLine stack (sum + score) xs

  (input.Split(Environment.NewLine))
  |> Array.map Seq.toList
  |> Array.map (scoreLine (new Stack<char>()) 0)
  |> Array.sum
  |> string
  """
    .Trim()

let part1Problem =
  """
You ask the submarine to determine the best route out of the deep-sea cave, but it only replies:

Syntax error in navigation subsystem on line: all of them

All of them?! The damage is worse than you thought. You bring up a copy of the navigation subsystem (your puzzle input).

The navigation subsystem syntax is made of several lines containing chunks. There are one or more chunks on each line, and chunks contain zero or more other chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops, the next chunk (if any) can immediately start. Every chunk must open and close with one of four legal pairs of matching characters:

    If a chunk opens with (, it must close with ).
    If a chunk opens with [, it must close with ].
    If a chunk opens with {, it must close with }.
    If a chunk opens with <, it must close with >.

So, () is a legal chunk that contains no other chunks, as is []. More complex but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and even (((((((((()))))))))).

Some lines are incomplete, but others are corrupted. Find and discard the corrupted lines first.

A corrupted line is one where a chunk closes with the wrong character - that is, where the characters it opens and closes with do not form one of the four legal pairs listed above.

Examples of corrupted chunks include (], {()()()>, (((()))}, and <([]){()}[{}]). Such a chunk can appear anywhere within a line, and its presence causes the whole line to be considered corrupted.

For example, consider the following navigation subsystem:

[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]

Some of the lines aren't corrupted, just incomplete; you can ignore these lines for now. The remaining five lines are corrupted:

    {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
    [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
    [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
    [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
    <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

Stop at the first incorrect closing character on each corrupted line.

Did you know that syntax checkers actually have contests to see who can get the high score for syntax errors in a file? It's true! To calculate the syntax error score for a line, take the first illegal character on the line and look it up in the following table:

    ): 3 points.
    ]: 57 points.
    }: 1197 points.
    >: 25137 points.

In the above example, an illegal ) was found twice (2*3 = 6 points), an illegal ] was found once (57 points), an illegal } was found once (1197 points), and an illegal > was found once (25137 points). So, the total syntax error score for this file is 6+57+1197+25137 = 26397 points!

Find the first illegal character in each corrupted line of the navigation subsystem. What is the total syntax error score for those errors?

  """
    .Trim()

let part1Explanation =
  """
  """
    .Trim()

let part2Code (input : string) =
  let left = [|'['; '('; '{'; '<'|]
  let right = [|']'; ')'; '}'; '>'|]

  let rec calculateScore (stack : Stack<char>) sum =
    match stack.Count with
    | 0 ->
      sum
    | n ->
      let top = stack.Pop()
      let score =
        match top with
        | '(' -> 1I
        | '[' -> 2I
        | '{' -> 3I
        | '<' -> 4I

      calculateScore stack ((sum * 5I) + score)

  let rec scoreLine (stack : Stack<char>) line =
    match line with
    | [] ->
      calculateScore stack 0I
    | x :: xs ->
      match x with
      | l when (Array.contains l left) ->
        stack.Push(l)
        scoreLine stack xs
      | r when (Array.contains r right) ->
        let top = stack.Pop()
        let topIndex = Array.IndexOf(left, top)
        let rIndex = Array.IndexOf(right, r)

        if topIndex = rIndex
        then scoreLine stack xs
        else 0I

  (input.Split(Environment.NewLine))
  |> Array.map Seq.toList
  |> Array.map (fun x -> scoreLine (new Stack<char>()) x)
  |> Array.filter ((<>) 0I)
  |> Array.sort
  |> (fun arr ->
    let mid = ((Array.length arr) / 2)

    arr.[mid])
  |> string

let part2CodeString =
  """
let part2Code (input : string) =
  let left = [|'['; '('; '{'; '<'|]
  let right = [|']'; ')'; '}'; '>'|]

  let rec calculateScore (stack : Stack<char>) sum =
    match stack.Count with
    | 0 ->
      sum
    | n ->
      let top = stack.Pop()
      let score =
        match top with
        | '(' -> 1I
        | '[' -> 2I
        | '{' -> 3I
        | '<' -> 4I

      calculateScore stack ((sum * 5I) + score)

  let rec scoreLine (stack : Stack<char>) line =
    match line with
    | [] ->
      calculateScore stack 0I
    | x :: xs ->
      match x with
      | l when (Array.contains l left) ->
        stack.Push(l)
        scoreLine stack xs
      | r when (Array.contains r right) ->
        let top = stack.Pop()
        let topIndex = Array.IndexOf(left, top)
        let rIndex = Array.IndexOf(right, r)

        if topIndex = rIndex
        then scoreLine stack xs
        else 0I

  (input.Split(Environment.NewLine))
  |> Array.map Seq.toList
  |> Array.map (fun x -> scoreLine (new Stack<char>()) x)
  |> Array.filter ((<>) 0I)
  |> Array.sort
  |> (fun arr ->
    let mid = ((Array.length arr) / 2)

    arr.[mid])
  |> string
  """
    .Trim()

let part2Problem =
  """
Now, discard the corrupted lines. The remaining lines are incomplete.

Incomplete lines don't have any incorrect characters - instead, they're missing some closing characters at the end of the line. To repair the navigation subsystem, you just need to figure out the sequence of closing characters that complete all open chunks in the line.

You can only use closing characters (), ], }, or >), and you must add them in the correct order so that only legal pairs are formed and all chunks end up closed.

In the example above, there are five incomplete lines:

    [({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
    [(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
    (((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
    {<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
    <{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.

Did you know that autocomplete tools also have contests? It's true! The score is determined by considering the completion string character-by-character. Start with a total score of 0. Then, for each character, multiply the total score by 5 and then increase the total score by the point value given for the character in the following table:

    ): 1 point.
    ]: 2 points.
    }: 3 points.
    >: 4 points.

So, the last completion string above - ])}> - would be scored as follows:

    Start with a total score of 0.
    Multiply the total score by 5 to get 0, then add the value of ] (2) to get a new total score of 2.
    Multiply the total score by 5 to get 10, then add the value of ) (1) to get a new total score of 11.
    Multiply the total score by 5 to get 55, then add the value of } (3) to get a new total score of 58.
    Multiply the total score by 5 to get 290, then add the value of > (4) to get a new total score of 294.

The five lines' completion strings have total scores as follows:

    }}]])})] - 288957 total points.
    )}>]}) - 5566 total points.
    }}>}>)))) - 1480781 total points.
    ]]}}]}]}> - 995444 total points.
    ])}> - 294 total points.

Autocomplete tools are an odd bunch: the winner is found by sorting all of the scores and then taking the middle score. (There will always be an odd number of scores to consider.) In this example, the middle score is 288957 because there are the same number of scores smaller and larger than it.

Find the completion string for each incomplete line, score the completion strings, and sort the scores. What is the middle score?
  """
    .Trim()

let part2Explanation =
  """
  """
    .Trim()

open BaseTypes

let data = {
  Day = 10

  Part1Code = part1Code
  Part2Code = part2Code

  Part1CodeString = part1CodeString
  Part2CodeString = part2CodeString

  Part1Problem = part1Problem
  Part2Problem = part2Problem

  Part1Explanation = part1Explanation
  Part2Explanation = part2Explanation

  Part1Language = OCaml
  Part2Language = OCaml
}