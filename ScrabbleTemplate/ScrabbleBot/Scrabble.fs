namespace assCRacK

open System.Xml.Schema
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open Dictionary

open System.IO
open AuxMethods

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        printfn ""
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardState    : Map<coord, char * int>
        squaresUsed   : Map<coord, uint32>
        lastTilePlaced : coord
    }

    let mkState b d pn h bs used lastTile = {
        board = b; 
        dict = d;  
        playerNumber = pn; 
        hand = h; 
        boardState = bs; 
        squaresUsed = used; 
        lastTilePlaced = lastTile;
    } 
    

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand


    
module Scrabble =
    open System.Threading
    // Figure out what move to make:
    //let makeMove hand board =
    (*var msg = buildword
    match msg with
    - some -> send SMPlayWord
    - none -> change tiles
    match change tiles with
    - some -> send SMChangeTiles
    - none -> pass
    match pass with
    - some -> send SMPass
    - none -> SMForfeit
    *)

    
 

    let tryBuildWord (pieces: Map<uint32,tile>) (st : State.state) = 
        // let hand = HandToChar st.hand pieces 
        let word : char list = []
        let charDicts : Dict list = []
        let mutable traverseCount = 0
        
        // BDFNUYY
        let hand = ['B';'D';'F';'N';'U';'Y';'Y']
        printfn "Our Hand: %s" (charListToString hand)
        printfn ""
        
        let rec traverse (word : char list) (hand: char list) (dict: Dict) (charDicts: Dict list) handIndex stepIndex =
            printfn "%d" traverseCount
            traverseCount <- traverseCount + 1
            match step hand[stepIndex] dict with
            | None ->
                printfn "None -> Index and hand.Length at top: %d %d" handIndex (hand.Length)
                if handIndex < hand.Length
                then traverse word hand dict charDicts (handIndex+1) stepIndex
                else 
                        printfn ""
                        printfn "None -> Word before remove: %s" (charListToString word)
                        let newWord = List.removeAt (word.Length-1) word
                        
                        printfn "None -> newWord after remove: %s" (charListToString newWord)
                        printfn "None -> Hand before append: %s" (charListToString hand)
                        let newHand = appendChar hand word (word.Length-1)
                        printfn "None -> newHand after append: %s" (charListToString newHand)
                        
                        
                        let charDictLength = charDicts.Length-1
                        let newCharDicts = List.removeAt charDictLength charDicts
                        let newCharDictLength = newCharDicts.Length-1

                        
                        printfn "None -> Index: %d" handIndex
                        printfn ""
                        
                        if newWord.Length = 0
                            then
                                traverse newWord newHand st.dict newCharDicts (handIndex % hand.Length) (stepIndex + 1)
                            else
                                let newDict = newCharDicts[newCharDictLength]
                                traverse newWord newHand newDict newCharDicts (handIndex % hand.Length) stepIndex
                    
            | Some (_,newDict) ->
                
                printfn "Some -> Index at top %d" handIndex
                printfn ""
                printfn "Some -> Word before append: %s" (charListToString word)
                let newWord = appendChar word hand handIndex
                printfn "Some -> newWord after append: %s" (charListToString newWord)
                
                printfn "Some -> Hand before remove: %s" (charListToString hand)
                let newHand = List.removeAt handIndex hand
                printfn "Some -> newHand after remove: %s" (charListToString newHand)
                
                let newCharDicts = appendDict charDicts newDict

                
                
                if lookup (charListToString newWord) st.dict
                then
                    printfn "Finished Word: %s" (charListToString newWord)
                    newWord                
                else traverse newWord newHand newDict newCharDicts handIndex stepIndex
        
        printfn "Index at beginning: %d" 0
        traverse word hand st.dict charDicts 0 0

        
   
    let firstLetter (st: State.state) =
        match st.boardState.TryFind st.lastTilePlaced with //st.lastTilePlaced
       | Some s -> fst s
       | None -> ' '
    
    let isHorizontal (st: State.state) =
        let anchor = st.lastTilePlaced
        match (st.boardState.TryFind ((fst anchor)-1, snd anchor)) with
        | Some s -> true
        | None -> false
        
    let removeHand (st: State.state) = List.fold (fun acc elem -> MultiSet.removeSingle elem acc) st.hand (MultiSet.toList st.hand)   
    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) =
            printfn ""
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input =  System.Console.ReadLine()

            let x = tryBuildWord pieces st

            
            let move = SMForfeit
            
            //make move
            
            // trybuildword
            // match svar
            // makeMove(svar)
            
            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream move

            let msg = recv cstream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                let removeFromHand = List.fold (fun acc elem -> MultiSet.removeSingle (fst(snd (elem))) acc) st.hand ms
                let addToHand = List.fold (fun acc elem -> MultiSet.add (fst elem) (snd elem) acc) removeFromHand newPieces
        
                let newBoardState = Map.add (0,0) ('a',0)  st.boardState
                let lastTile = firstLetter st
                let st' = st // This state needs to be updated mkstate -> newLastTile
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //newBoardState
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
    
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty Map.empty ((0,0): coord))
        
        
