namespace Day1

open System
open System.IO

type Turn = Left | Right

type Direction = North | East | West | South

type Instruction = {
    Direction: Turn;
    Distance: int;
}

type Coordinate = {
    X : int;
    Y : int;
}

type State = {
    Position: Coordinate;
    Facing: Direction;
}


module Directions =

    let parseDirection (c:char) =
        match c with
        | 'L' -> Left
        | 'R' -> Right
        | _ -> raise <| new System.FormatException(sprintf "Could not parse %c" c)

    let parseInstruction (s:string) =
        {
            Direction = parseDirection s.[0]
            Distance = Int32.Parse(s.Substring(1))
        }

    let left facing =
        match facing with
        | North -> West
        | West -> South
        | South -> East
        | East -> North

    let right facing =
        match facing with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

    let move direction distance position =
        match direction with
        | North -> { position with Y = position.Y + distance }
        | East ->  { position with X = position.X + distance }
        | South -> { position with Y = position.Y - distance }
        | West ->  { position with X = position.X - distance }

    let turnAndMove fTurner distance state =
        let direction = fTurner state.Facing
        let moved = move  direction distance state.Position
        { Facing = direction; Position = moved }

    // reducer
    let follow state instruction =
        match instruction.Direction with
        | Left -> turnAndMove left instruction.Distance state
        | Right -> turnAndMove right instruction.Distance state

    let parseAll (s:string) =
            s.Split([|','|])
            |> Seq.map (fun s -> s.Trim())
            |> Seq.map parseInstruction

module PuzzleA =

    let initialState = { Position = { X = 0; Y = 0}; Facing = North }

    let followAll s =
        let finalPostion =
            Directions.parseAll s
            |> Seq.fold Directions.follow initialState

        let totalDistance = Math.Abs(finalPostion.Position.X) + Math.Abs(finalPostion.Position.Y);

        printfn "Wound up at (%d, %d), which is %d blocks from target" finalPostion.Position.X finalPostion.Position.Y totalDistance

    let followSaved filename =
        followAll (File.ReadAllText filename)
