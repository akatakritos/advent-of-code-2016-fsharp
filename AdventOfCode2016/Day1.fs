module Day1

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

let move position direction distance =
    match direction with
    | North -> { position with Y = position.Y + distance }
    | East ->  { position with X = position.X + distance }
    | South -> { position with Y = position.Y - distance }
    | West ->  { position with X = position.X - distance }

let turnAndMove fTurner state distance =
    let direction = fTurner state.Facing
    let moved = move state.Position direction distance
    { Facing = direction; Position = moved }


let initialState = { Position = { X = 0; Y = 0}; Facing = North }

// reducer
let followInstruction state instruction =
    match instruction.Direction with
    | Left -> turnAndMove left state instruction.Distance
    | Right -> turnAndMove right state instruction.Distance

let parseString (s:string) =
    let finalPostion =
        s.Split([|','|])
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map parseInstruction
        |> Seq.fold followInstruction initialState

    let totalDistance = Math.Abs(finalPostion.Position.X) + Math.Abs(finalPostion.Position.Y);

    printfn "Wound up at (%d, %d), which is %d blocks from target" finalPostion.Position.X finalPostion.Position.Y totalDistance

let parseFile filename =
    parseString (File.ReadAllText filename)