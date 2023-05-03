module internal WordBuilder

    open ScrabbleUtil
    open Dictionary
    open AuxMethods
    
    let rec stepChar
        (u: uint32)
        (currentWord: uint32 list)
        (words : uint32 list list)
        (hand: uint32 list) 
        (dict: Dict)
        =
            match step (translate u) dict with
            | Some (thisIsAWord, newDict) ->
                if thisIsAWord
                then
                    let newCurrentWord = appendUIntToWord currentWord u
                    let newWords = newCurrentWord :: words
                    let newHand = removeUintHand hand u
                    
                    traverseDict newCurrentWord newWords newHand newDict
                else
                    let newCurrentWord = appendUIntToWord currentWord u
                    let newHand = removeUintHand hand u
                    
                    traverseDict newCurrentWord words newHand newDict
                    
            | None ->
                words
    
    and traverseDict
        (currentWord : uint32 list)
        (words: uint32 list list)
        (hand: uint32 list) 
        (dict: Dict) =
            List.fold (fun (acc : uint32 list list) c -> stepChar c currentWord acc hand dict) words hand
    
    let rec playTheVeryFirstWord
        (currentWord: uint32 list)
        (words : uint32 list list)
        (hand: uint32 list) 
        (dict: Dict)
        = traverseDict currentWord words hand dict
            
            
            

    