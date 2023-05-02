module internal WordBuilder

    open System.Threading
    open ScrabbleUtil
    open Dictionary
    open AuxMethods
    
    let rec stepChar
        (c: char)
        (currentWord: char list)
        (words : char list list)
        (hand: char list) 
        (dict: Dict)
        =
            match step c dict with
            | Some (thisIsAWord, newDict) ->
                printfn     "                                   "
                printfn     "Some:                              "
                if thisIsAWord
                then
                    let newCurrentWord = appendCharToWord currentWord c
                    let newWords = newCurrentWord :: words
                    let newHand = removeCharHand hand c
                    
                    printfn "   IfThen:                         "
                    printfn "        This is a word: %s         " (charListToString newCurrentWord)
                    
                    traverseDict newCurrentWord newWords newHand newDict
                else
                    let newCurrentWord = appendCharToWord currentWord c
                    let newHand = removeCharHand hand c

                    printfn "   Else:                           "
                    printfn "        Word before append: %s     " (charListToString currentWord)
                    printfn "        newWord after append: %s   " (charListToString newCurrentWord)
                    printfn "        Hand before remove: %s     " (charListToString hand)
                    printfn "        newHand after remove: %s   " (charListToString newHand)
                    
                    traverseDict newCurrentWord words newHand newDict
                    
            | None ->
                printfn     "None:                              "
                words
    
    and traverseDict
        (currentWord : char list)
        (words: char list list)
        (hand: char list) 
        (dict: Dict) =
            List.fold (fun (acc : char list list) c -> stepChar c currentWord acc hand dict) words hand
    
    let rec playTheVeryFirstWord
        (currentWord: char list)
        (words : char list list)
        (hand: char list) 
        (dict: Dict)
        =
            traverseDict currentWord words hand dict
            
            
            

    