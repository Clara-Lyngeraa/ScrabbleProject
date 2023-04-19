// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    
    
    let hello = [('H', 4);('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let add a b =
         a >>= fun x ->
         b >>= fun y ->
             ret (x+y)     
    
    let div a b =
         a >>= fun x ->
         b >>= fun y ->
             if y <> 0 then ret (x / y) else fail DivisionByZero


    type internal aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp
     

    and internal cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type internal bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)
       | IsLetter of cExp
       | IsDigit of cExp
    
     

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

   (* let arithEval a : SM<int> = failwith "Not implemented"      

    let charEval c : SM<char> = failwith "Not implemented"      

    let boolEval b : SM<bool> = failwith "Not implemented"*)
     
    let isConsonant c = 
        match System.Char.ToUpper c with
        |'A'|'E'|'I'|'O'|'U'|'Y' -> false
        | _ -> true
    
    let rec arithEval a : SM<int> =
        match a with
         | N n -> ret n
         | V v -> lookup v
         | WL -> wordLength 
         | PV i -> arithEval i >>= pointValue
         | Add (x, y) ->  add (arithEval x) (arithEval y)
         | Sub (x, y) -> arithEval x >>= fun n ->
                         arithEval y >>= fun m ->
                                       ret (n-m)
                                       //((.-.) x y)
         | Mul (x, y) -> arithEval x >>= fun n ->
                         arithEval y >>= fun m ->
                                        ret (n*m)
                                        //((.*.) x y)
         | Div (x, y) -> div (arithEval x) (arithEval y)
         | Mod (x, y) -> arithEval x >>= fun n ->
                         arithEval y >>= fun m ->
                                        if m > 0 then ret (n%m)
                                        else fail DivisionByZero
                                        //((.%.) x y)
         | CharToInt c -> charEval c >>= fun k ->
                                ret ((int) k)
    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= characterValue 
        | ToUpper c -> charEval c >>= fun k ->
                                    ret (System.Char.ToUpper k)//of cExp
        | ToLower c -> charEval c >>= fun k ->
                                    ret (System.Char.ToLower k)//of cExp 
        | IntToChar a -> arithEval a >>= fun x ->
                                       ret ((char) x) //of aExp
    and  boolEval b : SM<bool> =
        match b with            
        | TT -> ret (true)                  (* true *)
        | FF -> ret (false)                  (* false *)
        | AEq (x,y) -> arithEval x >>= fun n ->
                        arithEval y >>= fun m ->
                                        ret (n = m) // of aExp * aExp   (* numeric equality *)
        | ALt (x,y) -> arithEval x >>= fun n ->
                        arithEval y >>= fun m ->
                                        ret (n<m)//of aExp * aExp   (* numeric less than *)

        | Not bool -> boolEval bool >>= fun x ->
                                        ret (not x) //of bExp          (* boolean not *)
        | Conj (x,y) -> boolEval x >>= fun n ->
                            boolEval y >>= fun m ->
                                    ret (n && m) //of bExp * bExp  (* boolean conjunction *)
        | IsVowel c -> charEval c >>= fun x ->
                                    ret (not (isConsonant x))//of cExp      (* check for vowel *)
        | IsLetter c -> charEval c >>= fun x ->
                                    ret (System.Char.IsLetter x)//of cExp     (* check for letter *)
        | IsDigit c -> charEval c >>= fun x ->
                                        ret (System.Char.IsDigit x)//of cExp      (* check for digit *)*)
            


    type internal stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type internal StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type internal word = (char * int) list
    type internal squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type internal coord = int * int

    type internal boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type internal board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    
    
   
    
    
    
       