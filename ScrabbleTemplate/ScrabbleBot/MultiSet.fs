// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    (*type internal MultiSet<'a> = Temp of unit // Not implemented

    let empty : MultiSet<'a> = Temp () // Not implemented
    let add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun _ _ _ -> failwith "Not implemented" 
    let fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun _ _ _ -> failwith "Not implemented"*)
    
    type internal MultiSet<'a when 'a : comparison> = S of Map<'a, uint32>
    
    (* Green *)
    let empty = S Map.empty
    let isEmpty (S s) = Map.isEmpty s
    let size (S s) =
        Map.fold (fun acc key value -> acc + value) 0u s
    let contains a (S s) = Map.containsKey a s
    let numItems a (S s) =
        match Map.tryFind a s with
        | None -> 0u
        | Some x -> x
            
    let add a n (S s) =
        match Map.tryFind a s with
        | None -> S (Map.add a n s)
        | Some x when x > 0u -> S (Map.add a (x+n) s)
        | _ -> S s
    let addSingle a (S s) = add a 1u (S s) 
    let remove a n (S s) =
        match Map.tryFind a s with
        | None -> S s
        | Some x when x <= n -> S (Map.remove a s)
        | Some x -> S (Map.add a (x-n) s)
    
    (*
        size (remove 7.0 8u (add 7.0 9u empty))
    *)
    let removeSingle a (S s) =
        match Map.tryFind a s with
        | None -> S s
        | Some x when x <= 1u -> S (Map.remove a s)
        | Some x -> S (Map.add a (x-1u) s)
    let fold f acc (S s) = Map.fold f acc s
    let foldBack f (S s) acc = Map.foldBack f s acc
    
    (* Yellow *)
    let ofList lst =
        List.fold (fun acc elem -> addSingle elem acc) empty lst
    
    (* 
        The following function works by initialising new lists of size VALUE, with elements at all
        indices being KEY. It then appends this list to ACC which begins as an empty list.
    *)
    let toList (S s) =
        let addMany (x: 'a) n = List.init (int n) (fun _ -> x)
        Map.fold (fun acc key value ->
            List.append acc (addMany key value)) List.empty s
    
    let toListBAD (S s) =
        let addMany (x: 'a) n = List.init 1 (fun _ -> x)
        Map.fold (fun acc key value ->
            List.append acc (addMany key value)) List.empty s
    
    let q = add 7.0 9u empty
    let map f (S s) =
        let temp = toListBAD (S s)
        List.fold (fun acc elem ->
            add (f elem) (numItems elem (S s)) acc) empty temp
    
    let union (S s1) (S s2) =
        let biggest a b = if (a > b) then a else b
        Map.fold (fun acc key value ->
            (add key (biggest value (numItems key (S s1)))) acc) empty s2
    let sum (S s1) (S s2) =
        Map.fold (fun acc key value ->
            (add key (value + (numItems key (S s1))) acc)) empty s2
    let subtract (S s1) (S s2) =
        Map.fold (fun acc key value ->
            remove key value acc) (S s1) s2
    let intersection (S s1) (S s2) =
        let smallest a b = if (a > b) then b else a
        Map.fold (fun acc key value ->
            (add key (smallest value (numItems key (S s1)))) acc) empty s2
