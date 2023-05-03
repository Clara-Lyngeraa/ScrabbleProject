module internal AuxMethods
    open System
    open MultiSet
    open ScrabbleUtil.Dictionary
    open ScrabbleUtil
    
    let buildCharToScoreValueMap (pieces: Map<uint32,tile>) =
        Map.empty.
            Add('_',(snd (Set.toList (pieces[0u])).[0])).
            Add('A',(snd (Set.toList (pieces[1u])).[0])).
            Add('B',(snd (Set.toList (pieces[2u])).[0])).
            Add('C',(snd (Set.toList (pieces[3u])).[0])).
            Add('D',(snd (Set.toList (pieces[4u])).[0])).
            Add('E',(snd (Set.toList (pieces[5u])).[0])).
            Add('F',(snd (Set.toList (pieces[6u])).[0])).
            Add('G',(snd (Set.toList (pieces[7u])).[0])).
            Add('H',(snd (Set.toList (pieces[8u])).[0])).
            Add('I',(snd (Set.toList (pieces[9u])).[0])).
            Add('J',(snd (Set.toList (pieces[10u])).[0])).
            Add('K',(snd (Set.toList (pieces[11u])).[0])).
            Add('L',(snd (Set.toList (pieces[12u])).[0])).
            Add('M',(snd (Set.toList (pieces[13u])).[0])).
            Add('N',(snd (Set.toList (pieces[14u])).[0])).
            Add('O',(snd (Set.toList (pieces[15u])).[0])).
            Add('P',(snd (Set.toList (pieces[16u])).[0])).
            Add('Q',(snd (Set.toList (pieces[17u])).[0])).
            Add('R',(snd (Set.toList (pieces[18u])).[0])).
            Add('S',(snd (Set.toList (pieces[19u])).[0])).
            Add('T',(snd (Set.toList (pieces[20u])).[0])).
            Add('U',(snd (Set.toList (pieces[21u])).[0])).
            Add('V',(snd (Set.toList (pieces[22u])).[0])).
            Add('W',(snd (Set.toList (pieces[23u])).[0])).
            Add('X',(snd (Set.toList (pieces[24u])).[0])).
            Add('Y',(snd (Set.toList (pieces[25u])).[0])).
            Add('Z',(snd (Set.toList (pieces[26u])).[0]))
        
    let buildIDToScoreValueMap (pieces: Map<uint32,tile>) =
        Map.empty.
            Add(0u,(snd (Set.toList (pieces[0u])).[0])).
            Add(1u,(snd (Set.toList (pieces[1u])).[0])).
            Add(2u,(snd (Set.toList (pieces[2u])).[0])).
            Add(3u,(snd (Set.toList (pieces[3u])).[0])).
            Add(4u,(snd (Set.toList (pieces[4u])).[0])).
            Add(5u,(snd (Set.toList (pieces[5u])).[0])).
            Add(6u,(snd (Set.toList (pieces[6u])).[0])).
            Add(7u,(snd (Set.toList (pieces[7u])).[0])).
            Add(8u,(snd (Set.toList (pieces[8u])).[0])).
            Add(9u,(snd (Set.toList (pieces[9u])).[0])).
            Add(10u,(snd (Set.toList (pieces[10u])).[0])).
            Add(11u,(snd (Set.toList (pieces[11u])).[0])).
            Add(12u,(snd (Set.toList (pieces[12u])).[0])).
            Add(13u,(snd (Set.toList (pieces[13u])).[0])).
            Add(14u,(snd (Set.toList (pieces[14u])).[0])).
            Add(15u,(snd (Set.toList (pieces[15u])).[0])).
            Add(16u,(snd (Set.toList (pieces[16u])).[0])).
            Add(17u,(snd (Set.toList (pieces[17u])).[0])).
            Add(18u,(snd (Set.toList (pieces[18u])).[0])).
            Add(19u,(snd (Set.toList (pieces[19u])).[0])).
            Add(20u,(snd (Set.toList (pieces[20u])).[0])).
            Add(21u,(snd (Set.toList (pieces[21u])).[0])).
            Add(22u,(snd (Set.toList (pieces[22u])).[0])).
            Add(23u,(snd (Set.toList (pieces[23u])).[0])).
            Add(24u,(snd (Set.toList (pieces[24u])).[0])).
            Add(25u,(snd (Set.toList (pieces[25u])).[0])).
            Add(26u,(snd (Set.toList (pieces[26u])).[0]))
    
    // Returns the ID from the char value
    let charToIDMap =
        Map.empty.
            Add('_', 0u).
            Add('A', 1u).
            Add('B', 2u).
            Add('C', 3u).
            Add('D', 4u).
            Add('E', 5u).
            Add('F', 6u).
            Add('G', 7u).
            Add('H', 8u).
            Add('I', 9u).
            Add('J', 10u).
            Add('K', 11u).
            Add('L', 12u).
            Add('M', 13u).
            Add('N', 14u).
            Add('O', 15u).
            Add('P', 16u).
            Add('Q', 17u).
            Add('R', 18u).
            Add('S', 19u).
            Add('T', 20u).
            Add('U', 21u).
            Add('V', 22u).
            Add('W', 23u).
            Add('X', 24u).
            Add('Y', 25u).
            Add('Z', 26u)
            
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
    
    // Converts a list of uint32s to a list of ints (List.map uses int)
    let IntFromUint (list:uint32 list) =
        list |> List.map int
        
    // Reverse method:
    let UintFromInt (list:int list) =
        list |> List.map uint32
    
    let charListToString (cl : char list) =
        String.Concat(Array.ofList(cl))
    
    let uintListToString (cl : uint32 list) =
        
        String.Concat(Array.ofList(cl))
    // Convert list of ids to list of chars:
    let HandToChar (hand: MultiSet<uint32>) (pieces: Map<uint32,tile>) =
        let handIDList = toList hand
        let getChar tile = fst (Set.toList (pieces[tile])).[0]
        let test = List.map getChar handIDList
        printfn "Our hand: %s" (charListToString test)
        test
    
    let handToIDList (hand: MultiSet<uint32>) =
        toList hand
    
    let charAsList (c:char)=
        c :: []
   
    let dictAsList (d:Dict) =
        d :: []
    
    let stringAsList (s: string) =
        s :: []
    
    let uintAsList (u: uint32) =
        u :: []
        
     
    (*let appendCharToWord word char =
        word @ (charAsList char)*)
        
    let appendUIntToWord word uint =
        word @ (uintAsList uint)
    
    
    (*let removeCharHand (hand: char list) char =
        let charToRemove = charAsList char
        List.except charToRemove hand*)
    
    let removeUintHand (hand: uint32 list) uint =
        let uintToRemove = uintAsList uint
        List.except uintToRemove hand
    
    let appendCharToHand hand (word: char list) index =
        hand @ (charAsList word[index])
    
    let appendDict (charDicts: Dict list) (dict: Dict) =
        charDicts @ (dictAsList dict)
        
    let appendStringToList (s: string) (list: string list) =
        list @ (stringAsList s)
    
    let appendWordToWords (word: char list) (wordList: char list list) =
        word :: wordList
        
    let rec findLongestWord list x =
        
        if List.isEmpty list
        then
            List.Empty
        else
            let longestWordList = List.filter (fun cl -> List.length cl = x) list
            if (List.length longestWordList) > 0
                then longestWordList
                else
                    findLongestWord list (x-1)
    
    // Desired format:
    // char list -> (coord * (uint32 * (char * int))) list
    
    // char -> (uint32 * (char * int)))
   
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
        
        
    let getNewAnchorPoint (list : ((int*int) * (uint32 * (char * int))) list ) =
        match List.tryLast list with
        | Some s -> fst(s)
        | None -> (-1,-1)
    
    let printList (list: uint32 list) =
        List.map (fun u -> (printf "%c" (translate u))) list
        printfn ""
    
    (*let checkWildcards (hand: uint32 list) =
        List.map (fun u ->
            match u with
            | 0u -> 
            | _ -> u
            ) hand *)
    





            
