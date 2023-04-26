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
    
    // Convert list of ids to list of chars:
    let HandToChar (hand: MultiSet<uint32>) (pieces: Map<uint32,tile>) =
        let handIDList = toList hand
        let getChar i = fst (Set.toList pieces[handIDList[i]]).[0]
        List.map getChar (IntFromUint handIDList)
    
    let HandToIDs (hand: char list) = NotImplementedException
    
    let charAsList (c:char)=
        c :: []
   
    let dictAsList (d:Dict) =
        d :: []
    let appendChar word (hand: char list) index =
        word @ (charAsList hand[index])
    
    let appendDict (charDicts: Dict list) (dict: Dict) =
        charDicts @ (dictAsList dict) 
    
    let charListToString (cl : char list) =
        System.String.Concat(Array.ofList(cl))
   
