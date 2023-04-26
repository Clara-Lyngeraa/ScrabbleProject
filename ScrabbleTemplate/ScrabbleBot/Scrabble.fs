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
        anchor        : uint32 * uint32
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h; anchor = (0u,0u) }

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
        let fuckedCounter = -1
        
        let hand = ['P';'L';'X';'A';'Z';'Y']
        
        let rec traverse (word : char list) (hand: char list) (dict: Dict) (charDicts: Dict list) index =
            
            printfn "%c" hand[index]
            printfn "%d" hand.Length
            match step hand[index] dict with
            | None ->
                if index < hand.Length-1
                then traverse word hand dict charDicts (index+1)
                else
                    
                    let newWord = List.removeAt (word.Length-1) word
                    let z = charListToString newWord
                    printfn " New word in loop: %s" z
                    let newHand = appendChar hand word (word.Length-1)
                    let m = charListToString newHand
                    printfn " New Hand-- in loop: %s" m
                    let newCharDicts = List.removeAt (charDicts.Length-1) charDicts
                    let newDict = charDicts[newHand.Length-1]
                    traverse newWord newHand newDict newCharDicts (fuckedCounter+1)
                    
            | Some (isWord,newDict) ->
                let newWord = appendChar word hand index
                let x = charListToString newWord
                printfn " New word: %s" x
                
                let newHand = List.removeAt index hand
                let y = charListToString newHand
                printfn " New Hand--: %s" y
                
                let newCharDicts = appendDict charDicts newDict
                
                
                if lookup "HI" st.dict
                then
                    printfn "Is word %b" isWord
                    printfn "Finished Word: skrrrt %s" (charListToString newWord)
                    newWord                
                else traverse newWord newHand newDict newCharDicts 0
        
        traverse word hand st.dict charDicts 0

        
    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input =  System.Console.ReadLine()
            
            let x = tryBuildWord pieces st
            printfn "%s" (charListToString x)

            
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
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        