namespace assCRacK

open System.Threading
open System.Xml.Schema
open Parser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open Dictionary

open System.IO
open AuxMethods

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

 module Print =

    let printHand pieces hand =
        printfn ""
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : board
        dict          : Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardState    : Map<coord, char * int>
        pieces        : Map<uint32, tile>
        squaresUsed   : Map<coord, uint32>
        nextWordIsHorizontal : bool
        thisIsTheVeryFirstWord : bool
        tilesLeft : int
        anchorPoint : coord
    }

    let mkState b d pn h bs used pieces  isHoriz fw tiles anchor = {
        board = b; 
        dict = d;  
        playerNumber = pn; 
        hand = h; 
        boardState = bs; 
        squaresUsed = used
        pieces = pieces
        nextWordIsHorizontal = isHoriz
        thisIsTheVeryFirstWord = fw
        tilesLeft = tiles
        anchorPoint = anchor
    } 
    

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    

    
module Scrabble =

    let isHorizontal (st: State.state) (anchor) =
        match
            st.boardState.TryFind ((fst anchor)-1, snd anchor),
            st.boardState.TryFind ((fst anchor)+1, snd anchor)
            with
        | Some s, _ ->  false
        | _, Some s -> false
        | _ ->  true
        
    let rec isValidGoingDownAnchorPoint ((x,y): coord) (squaresUsed: Map<coord, uint32>) (wordLength: int) (first: bool) =
        match
            Map.tryFind (x-1, y+1) squaresUsed, // down and left
            Map.tryFind (x+1, y+1) squaresUsed, // down and right
            Map.tryFind (x, y+1) squaresUsed, // down
            Map.tryFind (x, y-1) squaresUsed // up (previous)
        with
        | Some _, _, _, _ -> false
        | _, Some _, _, _ -> false
        | _, _, Some _, _ -> false
        | _, _, _, Some _ -> if first then false else isValidGoingDownAnchorPoint (x, y+1) squaresUsed (wordLength-1) false
        | _ when wordLength = 0 -> true
        | _ -> isValidGoingDownAnchorPoint (x, y+1) squaresUsed (wordLength-1) false
    
    let rec isValidGoingRightAnchorPoint ((x,y): coord) (squaresUsed: Map<coord, uint32>) (wordLength: int) (first:bool) =
        match
            Map.tryFind (x+1, y-1) squaresUsed, // right and up
            Map.tryFind (x+1, y+1) squaresUsed, // right and down
            Map.tryFind (x+1, y) squaresUsed, // right
            Map.tryFind (x-1, y) squaresUsed // left (previous)
        with
        | Some _, _, _, _ -> false
        | _, Some _, _, _ -> false
        | _, _, Some _, _ -> false
        | _, _, _, Some _ -> if first then false else isValidGoingRightAnchorPoint (x+1, y) squaresUsed (wordLength-1) false
        | _ when wordLength = 0 -> true
        | _ -> isValidGoingRightAnchorPoint (x+1, y) squaresUsed (wordLength-1) false       
        
    let findAnchorPoints (squaresUsed: Map<coord, uint32>) =
        Seq.fold (fun acc ele ->
                                if isValidGoingDownAnchorPoint ele squaresUsed 6 true
                                then (ele, false)::acc
                                elif isValidGoingRightAnchorPoint ele squaresUsed 6 true
                                then (ele, true)::acc
                                else acc
                            ) (List.Empty: List<coord * bool>) squaresUsed.Keys
        
    
    let tryBuildWord (pieces: Map<uint32,tile>) (st : State.state) : (coord * (uint32 * (char * int))) list =    
    
        let anchorPoints = findAnchorPoints st.squaresUsed
        printfn "ANCHORPOINTS: \n%A" anchorPoints
        let hand = handToIDList st.hand
        let currentWord : uint32 list = []
        let words : uint32 list list = []
        
        let foundWords = 
            if st.thisIsTheVeryFirstWord
                then
                    [((0,0),WordBuilder.playTheVeryFirstWord currentWord words hand st.dict)]
                else
                    let possibleMoves = List.fold (fun acc ele ->
                        let beginWith = Map.find (fst ele) st.squaresUsed
                        let wordList = WordBuilder.stepChar beginWith currentWord words hand st.dict
                        
                        
                        if List.isEmpty wordList then
                            acc
                        else (fst ele,wordList)::acc) (List.Empty: List<coord * List<List<uint32>>>) anchorPoints
                    
                    possibleMoves
                    
        let bestMove = findLongestWord2 foundWords
        if List.isEmpty (snd bestMove)
            then []
        else
            let horiz =isHorizontal st (fst bestMove)
            let x = convertUIntList (snd bestMove) pieces (horiz) (fst bestMove) st.thisIsTheVeryFirstWord
            printfn "st.thisIsTheVeryFirstWord: %b" st.thisIsTheVeryFirstWord
            printfn "x: %A" x
            
            x

    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            let word = tryBuildWord pieces st
            let move =
               
                match word with
                | [] ->
                        if st.tilesLeft = 0 then
                            SMPass
                        else
                            let tilesToChange = min (MultiSet.size st.hand) (uint32 st.tilesLeft)
                            let handLst = handToIDList st.hand
                            let oldTiles = (handLst |> (Seq.take (int tilesToChange)) |> Seq.toList)
                            SMChange oldTiles
                | _ -> SMPlay word
               
            
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream move
            
            let msg = recv cstream
            let x = msg.ToString()

            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->

                let removeFromHand = List.fold (fun acc elem -> MultiSet.removeSingle (fst(snd elem)) acc) st.hand ms
                let addedToHand = List.fold (fun acc elem -> MultiSet.add (fst elem) (snd elem) acc) removeFromHand newPieces

                let newBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardState ms //Map.add (0,0) ('a',0)  st.boardState
                let newSquaresUsed = List.fold (fun acc (coord,(int, _)) -> Map.add coord int acc) st.squaresUsed ms

                let lastTile =
                    match fst (List.last ms) with
                    | (x,y) when x <> fst  st.anchorPoint -> ((x,y): coord), false
                    | (x,y) when y <> snd  st.anchorPoint -> ((x,y): coord), true 
                  
                let st' = State.mkState st.board st.dict st.playerNumber addedToHand newBoardState newSquaresUsed st.pieces (snd lastTile) false  (st.tilesLeft - List.length newPieces) st.anchorPoint// This state needs to be updated mkstate -> newLastTile
                
                aux st'
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //newBoardState
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
           
                let newBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardState ms
                let st' = State.mkState st.board st.dict st.playerNumber st.hand newBoardState st.squaresUsed st.pieces st.nextWordIsHorizontal false  st.tilesLeft st.anchorPoint
                aux st'
            | RCM (CMChangeSuccess(newTiles)) ->
               match move with
                | SMChange changedTiles ->
                       let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) st.hand newTiles
                       let removeFromHand = List.fold (fun acc elem -> MultiSet.removeSingle elem acc) handSet changedTiles
                       let st' = State.mkState st.board st.dict st.playerNumber removeFromHand st.boardState st.squaresUsed st.pieces  st.nextWordIsHorizontal false st.tilesLeft st.anchorPoint
                       aux st'
                       
            | RCM (CMPassed _) ->
               let st' = State.mkState st.board st.dict st.playerNumber st.hand st.boardState st.squaresUsed st.pieces st.nextWordIsHorizontal false st.tilesLeft st.anchorPoint
               aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                let tilesLEft = List.fold (fun acc elem ->
                                    match elem with
                                    | GPENotEnoughPieces (_,lft) -> (int lft)
                                    | _ -> acc
                                    ) st.tilesLeft err
                let st' = State.mkState st.board st.dict st.playerNumber st.hand st.boardState st.squaresUsed st.pieces  st.nextWordIsHorizontal st.thisIsTheVeryFirstWord tilesLEft st.anchorPoint

                aux st'
           
            


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
        let board = mkBoard boardP
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty Map.empty tiles true true (99 - (List.length hand)) (0,0))
        
        
