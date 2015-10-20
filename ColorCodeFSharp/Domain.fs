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
    type GameState = {
            Solution: Color list
            NSlots: int
            Slots: Slot[]
            AvailableColors: Color list
            CompletedGuesses: (int * Color list * Feedback) list
            MaxGuesses: int
            CurrentTry: int
            Status: GameStatus
        }

    let allcolors = [| Yellow; Red; Blue; White; Black; Green; Orange; Purple; Turquoise; Pink |]

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

    let join (innerSequence : 'b seq) outerKeySelector innerKeySelector (outerSequence : 'a seq) =
        outerSequence.Join(innerSequence,
            Func<_, _>(outerKeySelector), Func<_, _>(innerKeySelector),
            fun outer inner -> (outer, inner))

    let colorChecker combinationToCheck solution =
        // Provides feedback for the combination against the real solution
        let groupedColorsChk = 
            combinationToCheck
            |> Seq.groupBy id 
            |> Seq.map (fun (c, colors) -> (c, colors |> Seq.length))
        let groupedColorsSol = 
            solution
            |> Seq.groupBy id 
            |> Seq.map (fun (c, colors) -> (c, colors |> Seq.length))

        // Get the matches by color and position
        let matchPos =
            List.zip combinationToCheck solution |> Seq.filter (fun (chk, sol) -> chk = sol) |> Seq.length

        (* The number of colors that match are the minimum between the number of colors
         of the solution and the number of colors in the guess. For example, if the guess is
         Red, Red, Red, Blue, Green and the solution is Blue, Blue, Red, Green, White the
         Feedback should give you 1 match for Reds since you've matched 1 per color.
         Inversely, if your guess was Red, Green, Purple, Green, Blue for the same solution
         it should tell you that you have one color match for red. You get the idea... the pattern
         is to pick the minimum amount per group.
         Sum all the colors matches and substract the color and position matches to get
         The color and not position matches.*)
        let matchColor = 
            (groupedColorsChk |> join groupedColorsSol (fun (c, _) -> c) (fun (c, _) -> c)
            |> Seq.map (fun ((_, countGuess), (_, countSol)) -> min countGuess countSol)
            |> Seq.sum)
            - matchPos

        { MatchedPositions = matchPos; MatchedColors = matchColor }

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
                
        (fun fb ->
            let newStatus =
                if fb.MatchedPositions = gameState.NSlots then
                    Terminated Won
                elif gameState.CurrentTry = gameState.MaxGuesses then
                    Terminated Lost
                else
                    InProgress

            { gameState with 
                Slots = Array.init gameState.NSlots (fun i -> Empty)
                CompletedGuesses = (gameState.CurrentTry, guess, fb) :: gameState.CompletedGuesses
                CurrentTry = gameState.CurrentTry + 1
                Status = newStatus
            }) <!> feedback
