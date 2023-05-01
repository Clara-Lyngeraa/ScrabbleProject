module internal WordBuilder

    open System.Threading
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
        deadEndWords
        =
            Thread.Sleep(20)
            printfn ""
            printfn "Top of travZero -> HandIndex: %d, StepIndex: %d" handIndex stepIndex
            printfn "       ------- visited 0:       %c, 0-index,  %d" hand[handIndex] 0
            match step hand[0] dict with
            | None ->
                printfn "None -> Index and hand.Length at top: %d %d" handIndex (hand.Length)
                if handIndex < hand.Length
                then
                    printfn "       Zero : word:  %s --- char to be added  %c" (charListToString word) hand[handIndex]
                    traverseHand word hand dict charDicts (handIndex+1) stepIndex stateDict deadEndWords
                else
                    
                    let newDeadEndWord = charListToString word
                    let newDeadEndWords = appendStringToList newDeadEndWord deadEndWords
                    printfn "newDeadEndWord in 0  : %s " newDeadEndWord
                    
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
                            traverseStep newWord newHand stateDict newCharDicts (handIndex % hand.Length) (stepIndex + 1) stateDict newDeadEndWords
                        else
                            let newDict = newCharDicts[newCharDictLength]
                            traverseZero newWord newHand newDict newCharDicts (handIndex % hand.Length) stepIndex stateDict newDeadEndWords
                    
            | Some (_,newDict) ->
                let checkWord = charListToString (appendChar word hand 0)
                printfn "- checkWord in 0: %s" checkWord
                if List.contains checkWord deadEndWords
                then
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
                            traverseStep newWord newHand stateDict newCharDicts (handIndex % hand.Length) (stepIndex + 1) stateDict deadEndWords
                        else
                            let newDict = newCharDicts[newCharDictLength]
                            traverseZero newWord newHand newDict newCharDicts (handIndex % hand.Length) stepIndex stateDict deadEndWords
                else
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
                    else traverseZero newWord newHand newDict newCharDicts 0 stepIndex stateDict deadEndWords

    
    and traverseStep
        (word : char list)
        (hand: char list)
        (dict: Dict)
        (charDicts: Dict list)
        handIndex
        stepIndex
        stateDict
        deadEndWords
        =
            printfn "%d" traverseCount
            traverseCount <- traverseCount + 1
            printfn "Top of travStep -> HandIndex: %d, StepIndex: %d" handIndex stepIndex
            printfn "       ------- visited step:       %c, stepIndex %d" hand[stepIndex] stepIndex
            match step hand[stepIndex] dict with
            | None ->
                printfn "None -> Index and hand.Length at top: %d %d" handIndex (hand.Length)
                if handIndex < hand.Length
                then traverseHand word hand dict charDicts (handIndex+1) stepIndex stateDict deadEndWords
                else
                    let newDeadEndWord = charListToString word
                    let newDeadEndWords = appendStringToList newDeadEndWord deadEndWords
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
                            traverseStep newWord newHand stateDict newCharDicts (handIndex  % hand.Length) (stepIndex + 1) stateDict newDeadEndWords
                        else
                            let newDict = newCharDicts[newCharDictLength]
                            traverseZero newWord newHand newDict newCharDicts (handIndex  % hand.Length) stepIndex stateDict newDeadEndWords
                    
            | Some (_,newDict) ->
                let checkWord = charListToString (appendChar word hand handIndex)
                if List.contains checkWord deadEndWords
                then
                    let newDeadEndWord = charListToString word
                    let newDeadEndWords = appendStringToList newDeadEndWord deadEndWords
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
                            traverseStep newWord newHand stateDict newCharDicts (handIndex  % hand.Length) (stepIndex + 1) stateDict newDeadEndWords
                        else
                            let newDict = newCharDicts[newCharDictLength]
                            traverseZero newWord newHand newDict newCharDicts (handIndex  % hand.Length) stepIndex stateDict newDeadEndWords
                    
                else
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
                    else traverseZero newWord newHand newDict newCharDicts 0 stepIndex stateDict deadEndWords
    
    and traverseHand
       (word : char list)
        (hand: char list)
        (dict: Dict)
        (charDicts: Dict list)
        handIndex
        stepIndex
        stateDict
        deadEndWords
        =
                printfn "%d" traverseCount
                traverseCount <- traverseCount + 1
                printfn "Top of travHand -> HandIndex: %d, StepIndex: %d" handIndex stepIndex
                printfn "       ------- visited Hand:       %c, handindex %d" hand[handIndex] handIndex
                match step hand[handIndex] dict with
                | None ->
                    printfn "None -> Index and hand.Length at top: %d %d" handIndex (hand.Length)
                    if handIndex < hand.Length-1 
                    then
                        
                        traverseHand word hand dict charDicts (handIndex+1) stepIndex stateDict deadEndWords
                    else 
                        printfn ""
                        
                        let newDeadEndWord = charListToString word
                        let newDeadEndWords = appendStringToList newDeadEndWord deadEndWords
                        
                        printfn "newDeadEndWord in hand added  : %s " newDeadEndWord
                        
                        let wordLength = word.Length-1
                        
                        // THIS IS WHERE IT GETS WEIRD
                        let newHand = appendCharToHand hand word wordLength
                       
                        // METHOD ABOVE HERE

                        let newWord = List.removeAt wordLength word
                        
                        printfn "None -> Word before remove: %s" (charListToString word)
                        printfn "None -> newWord after remove: %s" (charListToString newWord)

                        printfn "None -> Hand before append: %s" (charListToString hand)
                        printfn "None -> newHand after append: %s" (charListToString newHand)



                        let charDictLength = charDicts.Length-1
                        let newCharDicts = List.removeAt charDictLength charDicts
                        let newCharDictLength = newCharDicts.Length-1

                        printfn ""
                        
                        if newWord.Length = 0
                            then
                                traverseStep newWord newHand stateDict newCharDicts (handIndex  % hand.Length) (stepIndex + 1) stateDict newDeadEndWords
                            else
                                let newDict = newCharDicts[newCharDictLength]
                                traverseZero newWord newHand newDict newCharDicts (handIndex  % hand.Length) stepIndex stateDict newDeadEndWords
                        
                | Some (_,newDict) ->
                    let checkWord = charListToString (appendChar word hand handIndex)
                    printfn "- checkWord in hand: %s" checkWord
                    for string in deadEndWords do
                        printfn "Words in deadEndWords: %s" string
                    if List.contains checkWord deadEndWords
                    then
                        let wordLength = word.Length-1
                        
                        // THIS IS WHERE IT GETS WEIRD
                        let newHand = appendCharToHand hand word wordLength
                       
                        // METHOD ABOVE HERE

                        let newWord = List.removeAt wordLength word
                        
                        printfn "None -> Word before remove: %s" (charListToString word)
                        printfn "None -> newWord after remove: %s" (charListToString newWord)

                        printfn "None -> Hand before append: %s" (charListToString hand)
                        printfn "None -> newHand after append: %s" (charListToString newHand)



                        let charDictLength = charDicts.Length-1
                        let newCharDicts = List.removeAt charDictLength charDicts
                        let newCharDictLength = newCharDicts.Length-1

                        printfn ""
                        
                        if newWord.Length = 0
                            then
                                traverseStep newWord newHand stateDict newCharDicts (handIndex  % hand.Length) (stepIndex + 1) stateDict deadEndWords
                            else
                                let newDict = newCharDicts[newCharDictLength]
                                traverseZero newWord newHand newDict newCharDicts (handIndex  % hand.Length) stepIndex stateDict deadEndWords
                    else
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
                        else traverseZero newWord newHand newDict newCharDicts 0 stepIndex stateDict deadEndWords