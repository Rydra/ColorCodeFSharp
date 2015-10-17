namespace ColorCodeSharp

module ConsoleRenderer =

    open System
    open Domain
    open Chessie.ErrorHandling

    let toConsoleColor color =
            match color with
            | Yellow    -> ConsoleColor.Yellow
            | Red       -> ConsoleColor.Red
            | Blue      -> ConsoleColor.Blue
            | White     -> ConsoleColor.White
            | Black     -> ConsoleColor.Gray
            | Green     -> ConsoleColor.Green
            | Orange    -> ConsoleColor.DarkYellow
            | Purple    -> ConsoleColor.DarkMagenta
            | Turquoise -> ConsoleColor.Cyan
            | Pink      -> ConsoleColor.Magenta

    
    let printSlots slots =
        Console.Write("Slots: [")
        let string = 
            slots 
            |> Array.iter (fun s ->
                            let colorname, consolecolor = 
                                match s with
                                | Occupied color    -> (color |> toString, color |> toConsoleColor)
                                | Empty             -> ("*", ConsoleColor.Gray)
                            Console.ForegroundColor <- consolecolor
                            Console.Write(colorname + "\t")
                                )

        Console.ResetColor()
        Console.WriteLine("]")

    let printFeedback feedback = 
        printfn "feedback %i matchPos, %i matchColor" feedback.MatchedPositions feedback.MatchedColors

    let printSlotStack gameState =
        gameState.CompletedGuesses
        |> List.iter (fun (_, colors, feedback) ->
            printSlots (colors |> List.map Occupied |> Array.ofList)
            printFeedback feedback
            Console.WriteLine())


    type Options =
    | Confirm
    | ColorSlot of Color * int
    | ColorSlots of (Color * int) list
    | PrintHistory
    | Unrecognized
    
    let (|Pair|Colorset|Unknown|) (p:string) =
        let orders = p.Trim().Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
        if orders.Length = 2 then 
            Pair (orders.[0], orders.[1])
        elif orders.Length = 4 then
            Colorset (orders |> Array.map toColor |> collect |> returnOrFail)
        else 
            Unknown
            

    let toOption =
        function
        | "1" -> Confirm
        | "2" -> PrintHistory
        | Pair (a, b) -> 
            let color = a |> toColor
            let pos =
                try
                    int b |> ok
                with
                | _ -> fail "failed to cast position!"

            (fun col pos -> ColorSlot(col, pos)) <!> color <*> pos
            |> either (fun (cs, _) -> cs) (fun _ -> Unrecognized)
        | Colorset colorlist -> ColorSlots (colorlist |> List.mapi (fun i c -> (c, i + 1)))
        | Unknown -> Unrecognized