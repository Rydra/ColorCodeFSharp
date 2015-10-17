namespace ColorCodeSharp

module Main = 
    open System
    open System.Linq
    open System.Text
    open Domain
    open Chessie.ErrorHandling
    open ConsoleRenderer
        
    [<EntryPoint>]
    let Main(args) = 
            
        let nslots = 4
        let ncolors = 8
        let game = startNewGame nslots ncolors
        let presentOptions =
            sprintf """The available colors are %A, with %i slots\n
                        1) Write 1 -> confirm combination\n
                        2) color position -> put color in slot position""" game.AvailableColors game.NSlots

        let confirmSlots game =
            let newGameStateRes = game |> confirm

            match newGameStateRes with
            | Pass newGameState ->

                let _, guess, feedback = newGameState.CompletedGuesses |> List.head

                printfn "trial %A" guess
                printfn "feedback %i matchPos, %i matchColor" feedback.MatchedPositions feedback.MatchedColors

                let state =
                    match newGameState.Status with
                    | Terminated x ->
                        match x with
                        | Won -> "Yeaaaah! You win!" + (sprintf "solution %A" newGameState.Solution)
                        | Lost -> "Meeehhhh... keep going dood!" + (sprintf "solution %A" newGameState.Solution)
                    | InProgress ->
                        "Still in progress, try again!"

                printfn "%s" state
                newGameState
            | Fail msg -> 
                printfn "%A" msg
                game
            | _ -> game

        // Recursive function that loops the game till it ends
        let rec processState game = 
           match game.Status with 
           | Terminated _ -> ignore
           | InProgress ->
                Console.WriteLine presentOptions
                printSlots game.Slots

                let gameState =
                    match toOption (Console.ReadLine()) with
                    | Confirm -> confirmSlots game
                    | PrintHistory -> printSlotStack game; game
                    | ColorSlot (color, slot) ->
                        putColor color slot game |> failureTee (fun errs -> printfn "%A" errs) |> ignore
                        game
                    | ColorSlots colorSlots ->
                        colorSlots 
                        |> List.iter (fun (c, pos) ->
                                putColor c pos game 
                                |> failureTee (fun errs -> printfn "%A" errs) |> ignore)
                                
                        game
                    | Unrecognized ->
                        printfn "Unrecognized command, try again."
                        game

                processState gameState

        processState game |> ignore
        0

