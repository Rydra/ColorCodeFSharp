namespace ColorCodeSharp

module Domain =

    open System
    open System.Text
    open System.Linq
    open Chessie.ErrorHandling

    type Color =
    | Yellow
    | Red
    | Blue
    | White
    | Black
    | Green
    | Orange
    | Purple
    | Turquoise
    | Pink

    type Slot =
    | Occupied of Color
    | Empty

    type Completed =
    | Won
    | Lost

    type GameStatus =
    | InProgress
    | Terminated of Completed

    type Feedback = { MatchedPositions: int; MatchedColors: int }

    type Guess = (int * Color list * Feedback)

    type GameState = {
            Solution: Color list
            NSlots: int
            Slots: Slot[]
            AvailableColors: Color list
            CompletedGuesses: Guess list
            MaxGuesses: int
            CurrentTry: int
            Status: GameStatus
        }

    let allcolors = [| Yellow; Red; Blue; White; Black; Green; Orange; Purple; Turquoise; Pink |]

    let toString color =
        match color with
        | Yellow    -> "Yellow"
        | Red       -> "Red"
        | Blue      -> "Blue"
        | White     -> "White"
        | Black     -> "Black"
        | Green     -> "Green"
        | Orange    -> "Orange"
        | Purple    -> "Purple"
        | Turquoise -> "Turquoise"
        | Pink      -> "Pink"

    let toColor (colorstring:string) =
        match colorstring.ToLowerInvariant() with
        | "yellow" -> ok Yellow
        | "red" -> ok Red
        | "blue" -> ok Blue
        | "white" -> ok White
        | "black" -> ok Black
        | "green" -> ok Green
        | "orange" -> ok Orange
        | "purple" -> ok Purple
        | "turquoise" -> ok Turquoise
        | "pink" -> ok Pink
        | _ -> fail "unrecognized color"

    let rnd = Random()

    // Generates a combination of n colors that the user will require to deduce
    let newRandomColors n (availableColors:Color list) = 
        Seq.unfold (fun x -> Some(availableColors.[rnd.Next(availableColors.Length)], x)) 1 
        |> Seq.take n
        |> List.ofSeq

    let putColor color position gameState =
        if position > gameState.NSlots || position < 1 then
            fail "Invalid position"
        else 
            gameState.Slots.[position - 1] <- Occupied color
            ok ignore

    let join (innerSequence : 'b seq) (outerSequence : 'a seq) outerKeySelector innerKeySelector =
        outerSequence.Join(innerSequence,
            Func<_, _>(outerKeySelector), Func<_, _>(innerKeySelector),
            fun outer inner -> (outer, inner))

    let toNum color =
        allcolors |> Array.findIndex (fun c -> c = color)

    let colorCheckerImperative guess solution =
        let mutable correctColor = 0
        let mutable correctPositionAndColor = 0

        let codeColorCount = Array.init (allcolors |> Array.length) (fun _ -> 0)
        let guessColorCount = Array.init (allcolors |> Array.length) (fun _ -> 0)

        for i in [0 .. (solution |> List.length) - 1] do
            let c = solution |> List.item(i)
            let g = guess |> List.item(i)

            codeColorCount.[c |> toNum] <- codeColorCount.[c |> toNum] + 1
            guessColorCount.[g |> toNum] <- codeColorCount.[g |> toNum] + 1

            if c = g then correctPositionAndColor <- correctPositionAndColor + 1

        for i in [0 .. (codeColorCount |> Array.length) - 1] do
            correctColor <- correctColor + min codeColorCount.[i] guessColorCount.[i]

        { MatchedPositions = correctPositionAndColor; MatchedColors = correctColor - correctPositionAndColor }

    let colorChecker guess solution =
        
        let correctPositionAndColor =
            List.zip guess solution 
            |> Seq.filter (fun (colorFromGuess, colorFromSolution) -> colorFromGuess = colorFromSolution) 
            |> Seq.length

        let countColorsForGuess : seq<Color * int> = 
            guess
            |> Seq.groupBy id 
            |> Seq.map (fun (color, list) -> (color, list |> Seq.length))

        let countColorsForSolution : seq<Color * int> = 
            solution
            |> Seq.groupBy id 
            |> Seq.map (fun (color, list) -> (color, list |> Seq.length))

        let countCorrectColors =
            (join countColorsForGuess countColorsForSolution (fun (color, _) -> color) (fun (color, _) -> color)
            |> Seq.map (fun ((_, countGuess), (_, countSol)) -> min countGuess countSol)
            |> Seq.sum)

        let correctColor = countCorrectColors - correctPositionAndColor

        { MatchedPositions = correctPositionAndColor; MatchedColors = correctColor }

    let startNewGame nslots ncolors =
        let availableColors = allcolors |> Seq.take ncolors |> List.ofSeq

        {
            Solution = newRandomColors nslots availableColors
            NSlots = nslots
            Slots = Array.init nslots (fun i -> Empty)
            AvailableColors = availableColors
            CompletedGuesses = List.Empty
            MaxGuesses = 10
            CurrentTry = 1
            Status = InProgress
        }

    let updateStatus game guess =
        if game.Solution = guess then
            Terminated Won
        elif game.CurrentTry = game.MaxGuesses then
            Terminated Lost
        else
            InProgress

    let addGuess guess feedback game =
        { game with 
            Slots = Array.init game.NSlots (fun i -> Empty)
            CompletedGuesses = (game.CurrentTry, guess, feedback) :: game.CompletedGuesses
            CurrentTry = game.CurrentTry + 1
            Status = updateStatus game guess
        }

    let confirm gameState =
        let guess = 
            gameState.Slots 
            |> Array.choose(fun slot ->
                match slot with
                | Occupied(color) -> Some(color)
                | Empty -> None)
            |> List.ofArray

        let feedback =
            if guess |> List.length <> gameState.NSlots then
                fail "Fill all slots first before confirming"
            else
                colorChecker guess gameState.Solution |> ok
                
        feedback >>= (fun fb -> gameState |> addGuess guess fb |> ok)
