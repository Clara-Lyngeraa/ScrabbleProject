module internal WordBuilder

    open System.Threading
    open ScrabbleUtil
    open Dictionary
    open AuxMethods
    
    let rec checkChar
        (c: char)
        (words : (char list) list)
        (hand: char list) 
        (dict: Dict) =
            match step c dict with
            | Some (bool, newDict) ->
                if bool
                then words
                else
                    
                    printfn ""
                    printfn "Some -> Word before append: %s" (charListToString words)
                    
                    let newWords = appendCharToWord words c
                    printfn "Some -> newWord after append: %s" (charListToString newWord)
                    
                    printfn "Some -> Hand before remove: %s" (charListToString hand)
                    
                    let newHand = removeCharHand hand c
                    printfn "Some -> newHand after remove: %s" (charListToString newHand)
                    
                    
                    // 
                    tryBuildWord c newWord newHand newDict
                    
            | None -> word
        
    and checkDict =
        
        (hand: char list) 
        (dict: Dict) =
    
    and tryBuildWord
        (c: char)
        (word : char list)
        (hand: char list) 
        (dict: Dict) =
            List.fold (fun acc x -> checkChar x acc hand dict) word hand
            
            
            

    