namespace assCRacK

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
        board         : Parser.board
        dict          : Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardState    : Map<coord, char * int>
        squaresUsed   : Map<coord, uint32>
        anchorPoint : coord
        nextWordIsHorizontal : bool
        thisIsTheVeryFirstWord : bool
    }

    let mkState b d pn h bs used lastTile isHoriz fw = {
        board = b; 
        dict = d;  
        playerNumber = pn; 
        hand = h; 
        boardState = bs; 
        squaresUsed = used; 
        anchorPoint = lastTile
        nextWordIsHorizontal = isHoriz
        thisIsTheVeryFirstWord = fw
    } 
    

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    
    
    
    


    
module Scrabble =
          
    let firstLetter (st: State.state) =
        match st.boardState.TryFind (st.anchorPoint) with //st.lastTilePlaced
        | Some s -> fst s
        | None -> ' '
    
    let isHorizontal (st: State.state) =
        let anchor = st.anchorPoint
        match (st.boardState.TryFind ((fst anchor)-1, snd anchor)) with
        | Some s ->  true
        | None ->  false
        
    let removeHand (st: State.state) = List.fold (fun acc elem -> MultiSet.removeSingle elem acc) st.hand (MultiSet.toList st.hand)   
    
    let tryBuildWord (pieces: Map<uint32,tile>) (st : State.state) =
        
    //let tryBuildWord (pieces: Map<uint32,tile>) (st : State.state) (anchorPoint: (int * int)) = 
        let hand = HandToChar st.hand pieces 
        let currentWord : char list = []
        let words : char list list = []
        // let hand = ['F';'H';'O';'R';'S';'T']
        
        let foundWords = 
            if st.thisIsTheVeryFirstWord
                then
                    st.thisIsTheVeryFirstWord = false // This wouldn't work, need mkState
                    WordBuilder.playTheVeryFirstWord currentWord words hand st.dict
                else
                    printfn "Our Hand: %s" (charListToString hand)
                    let charToBeginWith = fst(Map.find st.anchorPoint st.boardState) //should be able to 
                    WordBuilder.stepChar charToBeginWith currentWord words hand st.dict // Fold over list of anchorpoints instead of D
        
        
        printfn ""
        printfn "The first in the list of longest words: %s" (charListToString ((findLongestWord foundWords 8)[0]))
        printfn ""
        
        convertCharList ((findLongestWord foundWords 8)[0]) pieces st.nextWordIsHorizontal st.anchorPoint st.thisIsTheVeryFirstWord// Debug line, outcomment
        
        //(findLongestWord foundWords 8)[0]
        //smus tester coordinate prints
        //smus stopper igen nu skrrrt skrrrt
        

        
        
    
    
    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) =
            printfn ""
            Print.printHand pieces (State.hand st)

            let wordListToSend = tryBuildWord pieces st
            let move = SMPlay wordListToSend
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            
            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream move

            let msg = recv cstream
            let x = msg.ToString()
            printfn "starts here"  
            printfn "%s" x 
            printfn "skrrrrrrt response message ends here" 
            
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            (*
                i have made a method "getNewANchorPoints" that retunrs the coordinate of the last letter formerly played
                we need to update the state here since we dont player multiplayer
            *)
            
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->

                let removeFromHand = List.fold (fun acc elem -> MultiSet.removeSingle (fst(snd (elem))) acc) st.hand ms
                let addedToHand = List.fold (fun acc elem -> MultiSet.add (fst elem) (snd elem) acc) removeFromHand newPieces
                    
                let newBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardState ms //Map.add (0,0) ('a',0)  st.boardState
                let newSquaresUsed = List.fold (fun acc (coord,(int, _)) -> Map.add coord int acc) st.squaresUsed ms
                
                let newAnchorPoint = getNewAnchorPoint wordListToSend 
                
                let lastTile =
                    match fst (List.last ms) with
                    | (x,y) when x <> fst  st.anchorPoint -> ((x,y): coord), false
                    | (x,y) when y <> snd  st.anchorPoint -> ((x,y): coord), true
                   // | (x,y) when x = fst st.anchorPoint && y = snd st.anchorPoint -> ((x,y): coord), false
                

                let st' = State.mkState st.board st.dict st.playerNumber addedToHand newBoardState newSquaresUsed newAnchorPoint (snd lastTile) false // This state needs to be updated mkstate -> newLastTile
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //newBoardState
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let newBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardState ms
                let st' = State.mkState st.board st.dict st.playerNumber st.hand newBoardState st.squaresUsed st.anchorPoint st.nextWordIsHorizontal false 
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err; aux st
                (*
                    Her skal vin håndtere når vores ord fejler
                    burde ikke være et problem vi får eftersom det kun er hvis det ikke er muligt at skrive ord
                *)
            


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
    
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty Map.empty ((0,0): coord) true true)
        
        
