module Example

open System

open BaseTypes

let part1Problem =
  """
--- Day 1: Report Repair ---

After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
  """
    .Trim()

let part1Explanation =
  """
1 + 1 = 2
  """
    .Trim()

let part1Code (input : string) =
    let split (s : string) =
        s.Split(Environment.NewLine)
        |> List.ofArray

    let fixedInput =
        input
        |> split
        |> List.filter (not << String.IsNullOrWhiteSpace)
        |> List.map int

    let map =
        fixedInput
        |> List.fold (fun s i -> Map.add (2020 - i) i s) Map.empty
    
    fixedInput
    |> List.map (fun i -> Map.tryFind i map)
    |> List.choose id
    |> List.reduce (*)
    |> string

let part1CodeString =
  """
let part1Solution (input : string) =
    let split (s : string) =
        s.Split(Environment.NewLine)
        |> List.ofArray

    let fixedInput =
        input
        |> split
        |> List.filter (not << String.IsNullOrWhiteSpace)
        |> List.map int

    let map =
        fixedInput
        |> List.fold (fun s i -> Map.add (2020 - i) i s) Map.empty
    
    fixedInput
    |> List.map (fun i -> Map.tryFind i map)
    |> List.choose id
    |> List.reduce (*)
    |> string
  """
    .Trim()

let part2Problem =
  """
The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?
  """
    .Trim()

let part2Explanation =
  """
2 * 2 = 4
  """
    .Trim()

let part2Code (input : string) =
    let split (s : string) =
        s.Split(Environment.NewLine)
        |> List.ofArray

    let fixedInput =
        input
        |> split
        |> List.filter (not << String.IsNullOrWhiteSpace)
        |> List.map int

    let rec pairs lst =
        match lst with
        | [] -> []
        | h::t -> List.map (fun elem -> (h, elem)) t @ pairs t

    let solution i =
        pairs fixedInput
        |> List.map (fun (a, b) ->
            match (a + b + i = 2020) with
            | true -> Some i
            | false -> None)
        |> List.choose id

    fixedInput
    |> List.map solution
    |> List.filter (not << List.isEmpty)
    |> List.concat
    |> List.reduce (*)
    |> string

let part2CodeString =
  """
let part2Solution (input : string) =
    let split (s : string) =
        s.Split(Environment.NewLine)
        |> List.ofArray

    let fixedInput =
        input
        |> split
        |> List.filter (not << String.IsNullOrWhiteSpace)
        |> List.map int

    let rec pairs lst =
        match lst with
        | [] -> []
        | h::t -> List.map (fun elem -> (h, elem)) t @ pairs t

    let solution i =
        pairs fixedInput
        |> List.map (fun (a, b) ->
            match (a + b + i = 2020) with
            | true -> Some i
            | false -> None)
        |> List.choose id

    fixedInput
    |> List.map solution
    |> List.filter (not << List.isEmpty)
    |> List.concat
    |> List.reduce (*)
    |> string
  """
    .Trim()

let example = {
  Day = 1

  Part1Code = part1Code
  Part2Code = part2Code

  Part1CodeString = part1CodeString
  Part2CodeString = part2CodeString

  Part1Problem = part1Problem
  Part2Problem = part2Problem

  Part1Explanation = part1Explanation
  Part2Explanation = part2Explanation
}