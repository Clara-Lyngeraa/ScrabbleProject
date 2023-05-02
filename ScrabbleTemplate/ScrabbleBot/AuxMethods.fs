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
    
    // Converts a list of uint32s to a list of ints (List.map uses int)
    let IntFromUint (list:uint32 list) =
        list |> List.map int
        
    // Reverse method:
    let UintFromInt (list:int list) =
        list |> List.map uint32
    
    let charListToString (cl : char list) =
        String.Concat(Array.ofList(cl))
    // Convert list of ids to list of chars:
    let HandToChar (hand: MultiSet<uint32>) (pieces: Map<uint32,tile>) =
        let handIDList = toList hand
        let getChar tile = fst (Set.toList (pieces[tile])).[0]
        let test = List.map getChar handIDList
        printfn "Our hand: %s" (charListToString test)
        test
    
    let HandToIDs (hand: char list) = NotImplementedException
    
    let charAsList (c:char)=
        c :: []
   
    let dictAsList (d:Dict) =
        d :: []
    
    let stringAsList (s: string) =
        s :: []
        
     
    let appendCharToWord word char =
        word @ (charAsList char)
    
    let removeCharHand (hand: char list) char =
        let charToRemove = charAsList char
        List.except charToRemove hand
    
    let appendCharToHand hand (word: char list) index =
        hand @ (charAsList word[index])
    
    let appendDict (charDicts: Dict list) (dict: Dict) =
        charDicts @ (dictAsList dict)
        
    let appendStringToList (s: string) (list: string list) =
        list @ (stringAsList s)
    
    let appendWordToWords (word: char list) (wordList: char list list) =
        word :: wordList
        
    let rec findLongestWord list x =
        let longestWordList = List.filter (fun cl -> List.length cl = x) list
        if (List.length longestWordList) > 0
            then longestWordList
            else
                findLongestWord list (x-1)
    
    // Desired format:
    // char list -> (coord * (uint32 * (char * int))) list
    
    // char -> (uint32 * (char * int)))
   
    let charListToUInt_Char_Int list (map: Map<char,int>) =
        List.map (fun c -> (charToIDMap[c],(c,map[c]))) list
        
    let charListToTile list =
        List.map (fun x -> charToIDMap[x]) list
    
    // (boardState: Map<coord, char * int>) (squaresUsed: Map<coord, uint32>)
    let convertCharList list  (pieces: Map<uint32,tile>) =
        let charToScoreMap = buildCharToScoreValueMap pieces
        let temp = charListToUInt_Char_Int list charToScoreMap
        
        printfn "The following is the word in the format (uint32*(char*int)), one char pr. line:"
        for uint32,(char,int) in temp
            do
                printfn "(%d,(%c,%d)" uint32 char int
        
        
    

   
