module Day5

open System
open SyntaxHighlighterWrapper

let part1Code (input : string) =
  let parsedInput =
    (input.Split(Environment.NewLine))
    |> Array.map (fun x -> x.Split(" -> "))
    |> Array.map (Array.map (fun x -> x.Split(',')))
    |> Array.map (Array.map (fun x -> (int x.[0], int x.[1])))
    |> Array.map (fun x ->
      {|
        MinX = Math.Min(fst x.[0], fst x.[1])
        MaxX = Math.Max(fst x.[0], fst x.[1])
        MinY = Math.Min(snd x.[0], snd x.[1])
        MaxY = Math.Max(snd x.[0], snd x.[1])
      |})
    |> Array.map (fun x ->
      {|
        x with
          Horizontal = (x.MinX = x.MaxX)
          Vertical = (x.MinY = x.MaxY)
          Diagonal = not (x.MinX = x.MaxX) && not (x.MinY = x.MaxY)
      |})

  let vents =
    parsedInput
    |> Array.filter (fun x -> x.Diagonal = false)
    |> Array.collect (fun item ->
      if item.Vertical
      then [| for i in item.MinX .. item.MaxX do (i, item.MinY) |]
      elif item.Horizontal
      then [| for i in item.MinY .. item.MaxY do (item.MinX, i) |]
      else [||])
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> (k, Array.length v))

  vents
  |> Array.filter (fun (_, v) -> v >= 2)
  |> Array.length
  |> string

let part1CodeString =
  """
let part1Code (input : string) =
  let parsedInput =
    (input.Split(Environment.NewLine))
    |> Array.map (fun x -> x.Split(" -> "))
    |> Array.map (Array.map (fun x -> x.Split(',')))
    |> Array.map (Array.map (fun x -> (int x.[0], int x.[1])))
    |> Array.map (fun x ->
      {|
        MinX = Math.Min(fst x.[0], fst x.[1])
        MaxX = Math.Max(fst x.[0], fst x.[1])
        MinY = Math.Min(snd x.[0], snd x.[1])
        MaxY = Math.Max(snd x.[0], snd x.[1])
      |})
    |> Array.map (fun x ->
      {|
        x with
          Horizontal = (x.MinX = x.MaxX)
          Vertical = (x.MinY = x.MaxY)
          Diagonal = not (x.MinX = x.MaxX) && not (x.MinY = x.MaxY)
      |})

  let vents =
    parsedInput
    |> Array.filter (fun x -> x.Diagonal = false)
    |> Array.collect (fun item ->
      if item.Vertical
      then [| for i in item.MinX .. item.MaxX do (i, item.MinY) |]
      elif item.Horizontal
      then [| for i in item.MinY .. item.MaxY do (item.MinX, i) |]
      else [||])
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> (k, Array.length v))

  vents
  |> Array.filter (fun (_, v) -> v >= 2)
  |> Array.length
  |> string
  """
    .Trim()

let part1Problem =
  """
You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:

0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2

Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:

    An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
    An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the following diagram:

.......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111....

In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
  """
    .Trim()

let part1Explanation =
  """
  """
    .Trim()

let part2Code (input : string) =
  let parsedInput =
    (input.Split(Environment.NewLine))
    |> Array.map (fun x -> x.Split(" -> "))
    |> Array.map (Array.map (fun x -> x.Split(',')))
    |> Array.map (Array.map (fun x -> (int x.[0], int x.[1])))
    |> Array.map (fun x ->
      {|
        MinX = Math.Min(fst x.[0], fst x.[1])
        MaxX = Math.Max(fst x.[0], fst x.[1])
        MinY = Math.Min(snd x.[0], snd x.[1])
        MaxY = Math.Max(snd x.[0], snd x.[1])

        XIncr = fst x.[0] < fst x.[1]
        YIncr = snd x.[0] < snd x.[1]
        X1 = fst x.[0]
        Y1 = snd x.[0]
        X2 = fst x.[1]
        Y2 = snd x.[1]
      |})
    |> Array.map (fun x ->
      {|
        x with
          Horizontal = (x.MinX = x.MaxX)
          Vertical = (x.MinY = x.MaxY)
          Diagonal = not (x.MinX = x.MaxX) && not (x.MinY = x.MaxY)
      |})

  let vents =
    parsedInput
    |> Array.collect (fun item ->
      if item.Vertical
      then [| for i in item.MinX .. item.MaxX do (i, item.MinY) |]
      elif item.Horizontal
      then [| for i in item.MinY .. item.MaxY do (item.MinX, i) |]
      else
        let dist =
          Math.Min(
            (item.MaxX - item.MinX),
            (item.MaxY - item.MinY))
        [|
          for d in 0 .. dist do
            let x =
              if item.XIncr
              then item.X1 + d
              else item.X1 - d
            let y =
              if item.YIncr
              then item.Y1 + d
              else item.Y1 - d
            (x, y)
        |])
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> (k, Array.length v))

  vents
  |> Array.filter (fun (_, v) -> v >= 2)
  |> Array.length
  |> string

let part2CodeString =
  """
let part2Code (input : string) =
  let parsedInput =
    (input.Split(Environment.NewLine))
    |> Array.map (fun x -> x.Split(" -> "))
    |> Array.map (Array.map (fun x -> x.Split(',')))
    |> Array.map (Array.map (fun x -> (int x.[0], int x.[1])))
    |> Array.map (fun x ->
      {|
        MinX = Math.Min(fst x.[0], fst x.[1])
        MaxX = Math.Max(fst x.[0], fst x.[1])
        MinY = Math.Min(snd x.[0], snd x.[1])
        MaxY = Math.Max(snd x.[0], snd x.[1])

        XIncr = fst x.[0] < fst x.[1]
        YIncr = snd x.[0] < snd x.[1]
        X1 = fst x.[0]
        Y1 = snd x.[0]
        X2 = fst x.[1]
        Y2 = snd x.[1]
      |})
    |> Array.map (fun x ->
      {|
        x with
          Horizontal = (x.MinX = x.MaxX)
          Vertical = (x.MinY = x.MaxY)
          Diagonal = not (x.MinX = x.MaxX) && not (x.MinY = x.MaxY)
      |})

  let vents =
    parsedInput
    |> Array.collect (fun item ->
      if item.Vertical
      then [| for i in item.MinX .. item.MaxX do (i, item.MinY) |]
      elif item.Horizontal
      then [| for i in item.MinY .. item.MaxY do (item.MinX, i) |]
      else
        let dist =
          Math.Min(
            (item.MaxX - item.MinX),
            (item.MaxY - item.MinY))
        [|
          for d in 0 .. dist do
            let x =
              if item.XIncr
              then item.X1 + d
              else item.X1 - d
            let y =
              if item.YIncr
              then item.Y1 + d
              else item.Y1 - d
            (x, y)
        |])
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> (k, Array.length v))

  vents
  |> Array.filter (fun (_, v) -> v >= 2)
  |> Array.length
  |> string
  """
    .Trim()

let part2Problem =
  """
Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:

    An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
    An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.

Considering all lines from the above example would now produce the following diagram:

1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....

You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.

Consider all of the lines. At how many points do at least two lines overlap?
  """
    .Trim()

let part2Explanation =
  """
  """
    .Trim()

open BaseTypes

let data = {
  Day = 5

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