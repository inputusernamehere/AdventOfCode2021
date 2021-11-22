module Example

open System

open Problem

let part1Problem =
  """
Add two numbers together.
  """
    .Trim()

let part1Explanation =
  """
1 + 1 = 2
  """
    .Trim()

let part1Solution (input : string) =
  input.Split(' ')
  |> Array.map int
  |> fun a -> (a.[0] + a.[1])
  |> string

let part1Code =
  """
let part1Solution (input : string) =
  input.Split(' ')
  |> Array.map int
  |> fun a -> (a.[0] + a.[1])
  |> string
  """
    .Trim()

let part2Problem =
  """
Multiply two numbers together.
  """
    .Trim()

let part2Explanation =
  """
2 * 2 = 4
  """
    .Trim()

let part2Solution (input : string) =
  input.Split(' ')
  |> Array.map int
  |> fun a -> (a.[0] * a.[1])
  |> string

let part2Code =
  """
let part2Solution (input : string) =
  input.Split(' ')
  |> Array.map int
  |> fun a -> (a.[0] * a.[1])
  |> string
  """
    .Trim()

let example = {
  Day = 1

  Part1Solution = part1Solution
  Part2Solution = part2Solution

  Part1Code = part1Code
  Part2Code = part2Code

  Part1Problem = part1Problem
  Part2Problem = part2Problem

  Part1Explanation = part1Explanation
  Part2Explanation = part2Explanation
}