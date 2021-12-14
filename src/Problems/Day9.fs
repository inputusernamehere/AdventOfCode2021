module Day9

open System
open SyntaxHighlighterWrapper

let part1Code (input : string) =
  let grid =
    (input.Split(Environment.NewLine))
    |> Array.map (Seq.toArray)
    |> Array.map (Array.map string)
    |> Array.map (Array.map int)

  let yLimit = (Array.length grid) - 1
  let xLimit = (Array.length grid.[0]) - 1

  let risk x y =
    let neighbors = [
      if x > 0 then grid.[y].[x - 1]
      if x < xLimit then grid.[y].[x + 1]
      if y > 0 then grid.[y - 1].[x]
      if y < yLimit then grid.[y + 1].[x]
    ]

    let isLowPoint =
      neighbors
      |> List.forall ((<) grid.[y].[x])

    if isLowPoint
    then 1 + (grid.[y].[x])
    else 0

  [ for y in 0 .. yLimit do
    for x in 0 .. xLimit do
    risk x y ]
  |> List.sum
  |> string

let part1CodeString =
  """
let part1Code (input : string) =
  let grid =
    (input.Split(Environment.NewLine))
    |> Array.map (Seq.toArray)
    |> Array.map (Array.map string)
    |> Array.map (Array.map int)

  let yLimit = (Array.length grid) - 1
  let xLimit = (Array.length grid.[0]) - 1

  let risk x y =
    let neighbors = [
      if x > 0 then grid.[y].[x - 1]
      if x < xLimit then grid.[y].[x + 1]
      if y > 0 then grid.[y - 1].[x]
      if y < yLimit then grid.[y + 1].[x]
    ]

    let isLowPoint =
      neighbors
      |> List.forall ((<) grid.[y].[x])

    if isLowPoint
    then 1 + (grid.[y].[x])
    else 0

  [ for y in 0 .. yLimit do
    for x in 0 .. xLimit do
    risk x y ]
  |> List.sum
  |> string
  """
    .Trim()

let part1Problem =
  """
These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678

Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?
  """
    .Trim()

let part1Explanation =
  """
  """
    .Trim()

let part2Code (input : string) =
  let grid =
    (input.Split(Environment.NewLine))
    |> Array.map (Seq.toArray)
    |> Array.map (Array.map string)
    |> Array.map (Array.map int)

  let yLimit = (Array.length grid) - 1
  let xLimit = (Array.length grid.[0]) - 1

  let isLowPoint x y =
    [
      if x > 0 then grid.[y].[x - 1]
      if x < xLimit then grid.[y].[x + 1]
      if y > 0 then grid.[y - 1].[x]
      if y < yLimit then grid.[y + 1].[x]
    ]
    |> List.forall ((<) grid.[y].[x])

  let markableGrid =
    grid
    |> Array.map (Array.map (fun x -> (false, x)))

  let rec makeBasin (basinSize : int) (x, y) =
    let marked, current = markableGrid.[y].[x]

    if current = 9 then 0
    elif marked then 0
    else

      Array.set markableGrid.[y] x (true, current)

      let neighbors = [
        if x > 0 then (x - 1, y)
        if x < xLimit then (x + 1, y)
        if y > 0 then (x, y - 1)
        if y < yLimit then (x, y + 1)
      ]

      neighbors
      |> List.map (makeBasin basinSize)
      |> List.sum
      |> ((+) 1)

  let lowPoints = [
    for y in 0 .. yLimit do
    for x in 0 .. xLimit do
      if (isLowPoint x y) then yield (x, y)
  ]

  lowPoints
  |> List.map (makeBasin 0)
  |> List.sortDescending
  |> List.take 3
  |> List.reduce (*)
  |> string

let part2CodeString =
  """
let part2Code (input : string) =
  let grid =
    (input.Split(Environment.NewLine))
    |> Array.map (Seq.toArray)
    |> Array.map (Array.map string)
    |> Array.map (Array.map int)

  let yLimit = (Array.length grid) - 1
  let xLimit = (Array.length grid.[0]) - 1

  let isLowPoint x y =
    [
      if x > 0 then grid.[y].[x - 1]
      if x < xLimit then grid.[y].[x + 1]
      if y > 0 then grid.[y - 1].[x]
      if y < yLimit then grid.[y + 1].[x]
    ]
    |> List.forall ((<) grid.[y].[x])

  let markableGrid =
    grid
    |> Array.map (Array.map (fun x -> (false, x)))

  let rec makeBasin (basinSize : int) (x, y) =
    let marked, current = markableGrid.[y].[x]

    if current = 9 then 0
    elif marked then 0
    else

      Array.set markableGrid.[y] x (true, current)

      let neighbors = [
        if x > 0 then (x - 1, y)
        if x < xLimit then (x + 1, y)
        if y > 0 then (x, y - 1)
        if y < yLimit then (x, y + 1)
      ]

      neighbors
      |> List.map (makeBasin basinSize)
      |> List.sum
      |> ((+) 1)

  let lowPoints = [
    for y in 0 .. yLimit do
    for x in 0 .. xLimit do
      if (isLowPoint x y) then yield (x, y)
  ]

  lowPoints
  |> List.map (makeBasin 0)
  |> List.sortDescending
  |> List.take 3
  |> List.reduce (*)
  |> string
  """
    .Trim()

let part2Problem =
  """
Next, you need to find the largest basins so you know what areas are most important to avoid.

A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

The top-left basin, size 3:

2199943210
3987894921
9856789892
8767896789
9899965678

The top-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

The middle basin, size 14:

2199943210
3987894921
9856789892
8767896789
9899965678

The bottom-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.

What do you get if you multiply together the sizes of the three largest basins?
  """
    .Trim()

let part2Explanation =
  """
  """
    .Trim()

open BaseTypes

let data = {
  Day = 9

  Part1Code = part1Code
  Part2Code = part2Code

  Part1CodeString = part1CodeString
  Part2CodeString = part2CodeString

  Part1Problem = part1Problem
  Part2Problem = part2Problem

  Part1Explanation = part1Explanation
  Part2Explanation = part2Explanation

  Part1Language = OCaml
  Part2Language = FSharp
}