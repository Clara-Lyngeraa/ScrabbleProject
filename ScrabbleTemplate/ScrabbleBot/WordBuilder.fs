module internal WordBuilder

    open ScrabbleUtil
    open Dictionary
    open AuxMethods

    let mutable traverseCount = 0
    let rec traverseZero
        (word : char list)
        (hand: char list)
        (dict: Dict)
        (charDicts: Dict list)
        handIndex
        stepIndex
        stateDict
        =
            printfn "Top of travZero -> HandIndex: %d, StepIndex: %d" handIndex stepIndex
            match step hand[0] dict with
            | None ->
                printfn "None -> Index and hand.Length at top: %d %d" handIndex (hand.Length)
                if handIndex < hand.Length
                then traverseHand word hand dict charDicts (handIndex+1) stepIndex stateDict
                else 
                    printfn ""
                    printfn "None -> Word before remove: %s" (charListToString word)
                    let newWord = List.removeAt (word.Length-1) word
                    
                    printfn "None -> newWord after remove: %s" (charListToString newWord)
                    printfn "None -> Hand before append: %s" (charListToString hand)
                    let newHand = appendChar hand word (word.Length-1)
                    printfn "None -> newHand after append: %s" (charListToString newHand)
                    
                    
                    let charDictLength = charDicts.Length-1
                    let newCharDicts = List.removeAt charDictLength charDicts
                    let newCharDictLength = newCharDicts.Length-1

                    
                    printfn "None -> Index: %d" handIndex
                    printfn ""
                    
                    if newWord.Length = 0
                        then
                            traverseStep newWord newHand stateDict newCharDicts (handIndex % hand.Length) (stepIndex + 1) stateDict
                        else
                            let newDict = newCharDicts[newCharDictLength]
                            traverseZero newWord newHand newDict newCharDicts (handIndex % hand.Length) stepIndex stateDict
                    
            | Some (_,newDict) ->
                
                printfn "Some -> Index at top %d" handIndex
                printfn ""
                printfn "Some -> Word before append: %s" (charListToString word)
                let newWord = appendChar word hand handIndex
                printfn "Some -> newWord after append: %s" (charListToString newWord)
                
                printfn "Some -> Hand before remove: %s" (charListToString hand)
                let newHand = List.removeAt handIndex hand
                printfn "Some -> newHand after remove: %s" (charListToString newHand)
                
                let newCharDicts = appendDict charDicts newDict

                
                
                if lookup (charListToString newWord) stateDict
                then
                    printfn "Finished Word: %s" (charListToString newWord)
                    newWord                
                else traverseZero newWord newHand newDict newCharDicts 0 stepIndex stateDict

    
    and traverseStep
        (word : char list)
        (hand: char list)
        (dict: Dict)
        (charDicts: Dict list)
        handIndex
        stepIndex
        stateDict
        =
            printfn "%d" traverseCount
            traverseCount <- traverseCount + 1
            printfn "Top of travStep -> HandIndex: %d, StepIndex: %d" handIndex stepIndex
            match step hand[stepIndex] dict with
            | None ->
                printfn "None -> Index and hand.Length at top: %d %d" handIndex (hand.Length)
                if handIndex < hand.Length
                then traverseHand word hand dict charDicts (handIndex+1) stepIndex stateDict
                else 
                    printfn ""
                    printfn "None -> Word before remove: %s" (charListToString word)
                    let newWord = List.removeAt (word.Length-1) word
                    
                    printfn "None -> newWord after remove: %s" (charListToString newWord)
                    printfn "None -> Hand before append: %s" (charListToString hand)
                    let newHand = appendChar hand word (word.Length-1)
                    printfn "None -> newHand after append: %s" (charListToString newHand)
                    
                    
                    let charDictLength = charDicts.Length-1
                    let newCharDicts = List.removeAt charDictLength charDicts
                    let newCharDictLength = newCharDicts.Length-1

                    
                    printfn "None -> Index: %d" handIndex
                    printfn ""
                    
                    if newWord.Length = 0
                        then
                            traverseStep newWord newHand stateDict newCharDicts (handIndex  % hand.Length) (stepIndex + 1) stateDict
                        else
                            let newDict = newCharDicts[newCharDictLength]
                            traverseZero newWord newHand newDict newCharDicts (handIndex  % hand.Length) stepIndex stateDict
                    
            | Some (_,newDict) ->
                
                printfn "Some -> Index at top %d" handIndex
                printfn ""
                printfn "Some -> Word before append: %s" (charListToString word)
                let newWord = appendChar word hand handIndex
                printfn "Some -> newWord after append: %s" (charListToString newWord)
                
                printfn "Some -> Hand before remove: %s" (charListToString hand)
                let newHand = List.removeAt handIndex hand
                printfn "Some -> newHand after remove: %s" (charListToString newHand)
                
                let newCharDicts = appendDict charDicts newDict
                
                if lookup (charListToString newWord) stateDict
                then
                    printfn "Finished Word: %s" (charListToString newWord)
                    newWord                
                else traverseZero newWord newHand newDict newCharDicts 0 stepIndex stateDict
    
    and traverseHand
       (word : char list)
        (hand: char list)
        (dict: Dict)
        (charDicts: Dict list)
        handIndex
        stepIndex
        stateDict
        =
                printfn "%d" traverseCount
                traverseCount <- traverseCount + 1
                printfn "Top of travHand -> HandIndex: %d, StepIndex: %d" handIndex stepIndex
                match step hand[handIndex] dict with
                | None ->
                    printfn "None -> Index and hand.Length at top: %d %d" handIndex (hand.Length)
                    if handIndex < hand.Length-1
                    then traverseHand word hand dict charDicts (handIndex+1) stepIndex stateDict
                    else 
                        printfn ""
                        printfn "None -> Hand before append: %s" (charListToString hand)
                        let wordLength = word.Length-1
                        
                        // THIS IS WHERE IT GETS WEIRD
                        let newHand = appendCharToHand hand word wordLength
                        // METHOD ABOVE HERE
                        printfn "None -> newHand after append: %s" (charListToString newHand)

                        printfn "None -> Word before remove: %s" (charListToString word)
                        let newWord = List.removeAt wordLength word
                        printfn "None -> newWord after remove: %s" (charListToString newWord)
                        
                        let charDictLength = charDicts.Length-1
                        let newCharDicts = List.removeAt charDictLength charDicts
                        let newCharDictLength = newCharDicts.Length-1

                        printfn ""
                        
                        if newWord.Length = 0
                            then
                                traverseStep newWord newHand stateDict newCharDicts (handIndex  % hand.Length) (stepIndex + 1) stateDict
                            else
                                let newDict = newCharDicts[newCharDictLength]
                                traverseZero newWord newHand newDict newCharDicts (handIndex  % hand.Length) stepIndex stateDict
                        
                | Some (_,newDict) ->
                    printfn "Some -> Index at top %d" handIndex
                    printfn ""
                    printfn "Some -> Word before append: %s" (charListToString word)
                    let newWord = appendChar word hand handIndex
                    printfn "Some -> newWord after append: %s" (charListToString newWord)
                    
                    printfn "Some -> Hand before remove: %s" (charListToString hand)
                    let newHand = List.removeAt handIndex hand
                    printfn "Some -> newHand after remove: %s" (charListToString newHand)
                    
                    let newCharDicts = appendDict charDicts newDict

                    if lookup (charListToString newWord) stateDict
                    then
                        printfn "Finished Word: %s" (charListToString newWord)
                        newWord                
                    else traverseZero newWord newHand newDict newCharDicts 0 stepIndex stateDict