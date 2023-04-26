module internal ScrabbleTemplate.Dictionary
    type Dict =
            | Leaf of bool
            | Node of bool * Map<char,Dict>

        let empty () = Leaf false

        let rec lookup (searchWord: string) d =
            let length = searchWord.Length
            match d with
            | Leaf l when length = 0                            -> l
            | Leaf _                                             -> false
            | Node (isWord, _) when length = 0                  -> isWord
            | Node (_, map) ->
                match Map.tryFind searchWord.[0] map with
                | None -> false
                | Some dict -> lookup (string searchWord.[1 .. length-1]) dict

        let rec insert (word: string) dict =
            let length = word.Length
            match dict with
            | Leaf _ when length = 0 -> Leaf true
            | Node (_, map) when length = 0 -> Node (true, map)
            | Leaf l ->
                let newMap = Map.add word.[0] (insert word.[1..length-1] (empty())) Map.empty
                Node (l, newMap)
            | Node (isWord, map) ->
                match Map.tryFind word.[0] map with
                | None ->
                    let updatedMap = Map.add word.[0] (insert word.[1..length-1] (empty())) map
                    Node(isWord, updatedMap)
                | Some x ->
                    let updatedMap = Map.add word.[0] (insert word.[1..length-1] x) map
                    Node(isWord, updatedMap)

        let step c dict =
            match dict with
            | Leaf _ -> None
            | Node(_, map) ->
                match Map.tryFind c map with
                | None -> None
                | Some d ->
                    match d with
                    | Leaf l -> Some(l, d)
                    | Node (isWord, _) -> Some (isWord, d)
        
        