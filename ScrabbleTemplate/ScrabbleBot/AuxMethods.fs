module internal AuxMethods
    open System
    open MultiSet
    open ScrabbleUtil.Dictionary
    open StateMonad
    open ScrabbleUtil
    open ScrabbleUtil.ServerCommunication

    open System.IO
    open ScrabbleUtil.DebugPrint
    
    // Converts a list of uint32s to a list of ints (List.map uses int)
    let IntFromUint (list:uint32 list) =
        list |> List.map int
        
    // Reverse method:
    let UintFromInt (list:int list) =
        list |> List.map uint32
    
    let charListToString (cl : char list) =
        System.String.Concat(Array.ofList(cl))
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
        
    

   
