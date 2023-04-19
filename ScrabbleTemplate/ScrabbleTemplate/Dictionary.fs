module internal ScrabbleTemplate.Dictionary

    type CH =
        | Char of char
        | Empty
        
       
    type Dict =  
        | Leaf of bool 
        | Node of bool * Map<CH, Dict>
    
    let empty () = Leaf false
    
    let translate c =
        match c with
        | '#' -> Empty
        | char -> Char char
        
    
    let rec createStrings (s: string) i foundEmpty (acc: string) =
        match i, foundEmpty with
        | 0, false -> createStrings s (acc.Length) true (acc + (string) s.[0] + (string) '#')
        | _, true -> createStrings s (i+1) true (acc + (string) s.[i])
        | _, false -> createStrings s (i-1) false (acc + (string) s.[i])
       
    let rec insert (s: string) dict =
        match dict with
            | Leaf _ when s.Length = 0 ->  Leaf true
            | Leaf b when s.Length > 0 ->
                let child = insert s.[1..] (Node(b, Map.empty))
                Node(b, Map.add (translate s.[0]) child Map.empty)
            | Node (bool, map) when s.Length = 0 -> Node(true, map)                     
            | Node (bool, map) when s.Length > 0 ->
                match Map.tryFind (translate s.[0]) map with
                | None ->
                    let child = insert s.[1..] (Node(false, Map.empty))
                    Node(bool, Map.add (translate s.[0]) child map)
                | Some some ->
                    let child = insert s.[1..] some
                    Node(bool, Map.add (translate s.[0]) child map)
    
    let insertSecond s dict =
        let listToInsert = createStrings s s.Length false ""
        List.fold (fun acc x -> insert x acc) dict listToInsert
    
    let rec lookup (s: string) dict =
        match dict with
        | Leaf l                  when s.Length = 0 -> l
        | Leaf _                                    -> false
        | Node (_, map)when s.Length > 0            ->
            match Map.tryFind (translate s.[0]) map with
            | None -> false
            | Some some                             -> lookup (string s.[1..]) some
        | Node (b, _)                               -> b
    
    let step c dict =
        match dict with
        | Leaf _ -> None
        | Node (bool, map) ->
            match Map.tryFind (translate c) map with
            | None -> None
            | Some s ->
                match s with
                | Leaf l -> Some (l, s)
                | Node (b, d) -> Some (b, s)
    
    