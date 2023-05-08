module internal AuxMethods
    open System
    open MultiSet
    open ScrabbleUtil.Dictionary
    open ScrabbleUtil
      
    let idToCharMap =
        Map.empty.
            Add(0u, 'A').
            Add(1u, 'A').
            Add(2u, 'B').
            Add(3u, 'C').
            Add(4u, 'D').
            Add(5u, 'E').
            Add(6u, 'F').
            Add(7u, 'G').
            Add(8u, 'H').
            Add(9u, 'I').
            Add(10u, 'J').
            Add(11u, 'K').
            Add(12u, 'L').
            Add(13u, 'M').
            Add(14u, 'N').
            Add(15u, 'O').
            Add(16u, 'P').
            Add(17u, 'Q').
            Add(18u, 'R').
            Add(19u, 'S').
            Add(20u, 'T').
            Add(21u, 'U').
            Add(22u, 'V').
            Add(23u, 'W').
            Add(24u, 'X').
            Add(25u, 'Y').
            Add(26u, 'Z')
    
    let translate (u: uint32) =
        idToCharMap[u]
    
    let handToIDList (hand: MultiSet<uint32>) =
        toList hand

    let uintAsList (u: uint32) =
        u :: []
        

    let appendUIntToWord word uint =
        word @ (uintAsList uint)
    

    
    let removeUintHand (hand: uint32 list) uint =
        let uintToRemove = uintAsList uint
        List.except uintToRemove hand
    

    let rec findLongestWord2 wordlist =
        List.fold (fun acc ele ->
            let longestMoveAtAnchor = List.maxBy (fun l -> List.length l) (snd ele)
            if List.length longestMoveAtAnchor > List.length (snd acc)
                then (fst ele, longestMoveAtAnchor)
            else acc
            ) ((0,0), List.empty) wordlist 
    

   
    let uintListToUInt_Char_Int list (pieces: Map<uint32,tile>) =
        List.map (fun u -> (u,((translate u), (snd (Set.toList (pieces[u])).[0])) )) list
    
    //just a debug method
    let printTest (temp : ((int*int) * (uint32 * (char * int))) list ) =
        printfn ""
        List.iter (fun ((i1:int, i2:int), (u:uint32, (c:char, i3:int))) ->
            printfn "(%d,%d),(%d, (%c, %d))" i1 i2 u c i3) temp
    
    let matchCoord (startCoord : int * int) (word : uint32 * (char * int)) =
        startCoord, word
    
    let findIndex arr elem = arr |> List.findIndex ((=) elem)
    
    let setCoordsForHorizontal (word : (uint32 * (char * int)) list ) (x:int) (y : int)  (isFirstWord : bool)=
        if isFirstWord
        then List.fold (fun acc a ->  acc  @ [matchCoord ( ((findIndex word a) + x) , y) a ]) List.Empty word
        else 
            let fullWord = List.fold (fun acc a ->  acc  @ [matchCoord ( ((findIndex word a) + x) , y) a ]) List.Empty word
            fullWord.Tail
    
    let setCoordsForVertical (word : (uint32 * (char * int)) list ) (x:int) (y : int)  =
        let fullWord =  List.fold (fun acc a ->  acc  @ [matchCoord (x , (y + findIndex word a)) a ]) List.Empty word
        fullWord.Tail
    
    
        
    // (boardState: Map<coord, char * int>) (squaresUsed: Map<coord, uint32>)
    let convertUIntList list  (pieces: Map<uint32,tile>) (isHorizontal : bool) (coordinate : coord) (isFirstWord : bool) =
        
        let temp = uintListToUInt_Char_Int list pieces
        
        if isHorizontal
        then
            printTest (setCoordsForHorizontal temp (fst coordinate) (snd coordinate) isFirstWord)  
            setCoordsForHorizontal temp (fst coordinate) (snd coordinate) isFirstWord
        else
            printTest (setCoordsForVertical temp (fst coordinate) (snd coordinate))  
            setCoordsForVertical temp (fst coordinate) (snd coordinate)
        
        


    

    
        
    





            
