// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2  
    let (.>*>) p1 p2  = p1  .>> spaces .>> p2 
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2 

    
    
    
    let parenthesise p = pchar '(' >*>. spaces  >*>. p .>*> spaces .>*> pchar ')' 
    let tubParenthesise p = pchar '{' >*>. spaces  >*>. p .>*> spaces .>*> pchar '}'
   

    
    let stringFromCharList (cl : char list) =
        String.concat "" <| List.map string cl   
        
    let pid =
              pchar '_' <|> pletter .>>.
              many (palphanumeric <|> pchar '_')
              |>>
              (fun (x, y) -> stringFromCharList (x :: y))

    
    let unop op a = op >*>. a
        
    let binop op p1 p2 = p1 .>*> op .>*>. p2


    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    let CtomParse, cref = createParserForwardedToRef<cExp>()
    
    let BDoubleParse, bdref = createParserForwardedToRef<bExp>()
    let BEqualsParse, bEqref = createParserForwardedToRef<bExp>()
    let BtomParse, bref = createParserForwardedToRef<bExp>()
    
    (* 1 + 2 * 3
    Term :=
        Prod + Term
        Prod - Term
        Prod
    
    Prod :=
        Atom * Prod
        Atom
        
    Atom :=
        N
    
    *)
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    
    do tref := choice [AddParse; SubParse; ProdParse]


    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse <?> "Par"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (N -1, x)) |>> Mul <?> "Neg"
    let VParse = pid |>> V <?> "Variable"
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let CharToInt = unop pCharToInt CtomParse |>> CharToInt <?> "CharToInt"
    
    do aref := choice [CharToInt; NegParse; NParse; PVParse; VParse; ParParse]

    let AexpParse = TermParse 
    
    
    
    let CParParse = parenthesise CtomParse
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C" 

    let CVParse = unop pCharValue AtomParse |>> CV <?> "CV"
    let CIntToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "IntToChar"
    
    let CToUpperParse = unop pToUpper CtomParse |>> ToUpper <?> "ToUpper" 
    let CToLowerParse = unop pToLower CtomParse |>> ToLower <?> "ToUpper" 
    
    do cref := choice [CToLowerParse; CToUpperParse; CIntToCharParse; CParse; CVParse; CParParse]
    let CexpParse = CtomParse


    // yellow
    // exe 7.10 
    
    (*
        true, false, B & B, B || B, not(B) , 
        A = A, A <> A, A < A , A <= A ,
        A > A, A >= A,
        ssDigit ( C )
        isLetter ( C )
        isVowel ( C )
        ( B ) 
    *)
    
    let BexpParse = BDoubleParse
    let BAndParse = binop (pstring"/\\") BEqualsParse BDoubleParse |>> Conj <?> "Conj"
    let BOrParse = binop (pstring"\/") BEqualsParse BDoubleParse |>> (fun (x,y) -> x.||.y) <?> "OR"
    
    do bdref := choice [BAndParse; BOrParse; BEqualsParse]

    let BEq = binop (pchar '=' ) AexpParse AexpParse |>> AEq <?> "AEq"
    let BNotEq = binop (pstring "<>") AexpParse AexpParse |>> (fun (x,y) -> x.<>.y) <?> "NotEQ"  //.<>. instead of <> turns it into a parser instead of bool
    
    let BLessThan = binop (pchar '<' ) AexpParse AexpParse |>> ALt <?> "ALt"
    let BLessThanEQ = binop (pstring "<=") AexpParse AexpParse |>> (fun (x,y) -> x.<=.y) <?> "LessthanEq"
    
    let BGreaterThan = binop (pchar '>' ) AexpParse AexpParse |>> (fun (x,y) -> x.>.y) <?> "GreaterThan"
    let BGreaterThanEQ = binop (pstring ">=") AexpParse AexpParse |>> (fun (x,y) -> x.>=.y) <?> "GreaterThanEq"
    
    do bEqref := choice [BEq; BNotEq; BLessThan; BLessThanEQ; BGreaterThan; BGreaterThanEQ; BtomParse]

    
    let BIsLetter = unop pIsLetter CtomParse |>> IsLetter <?> "IsLetter" 
    let BIsVowel = unop pIsLetter CtomParse |>> IsVowel <?> "IsVowel" 
    let BIsDigit = unop pIsLetter CtomParse |>> IsLetter <?> "IsDigit"
    let BParParse = parenthesise BDoubleParse
    let BNeg = unop (pchar '~' ) BDoubleParse |>> Not <?> "Not"
        
    let BT = pTrue |>> (fun _ -> TT) <?> "TT"
    let BF = pFalse |>> (fun _ -> FF) <?> "FF"
    
    do bref := choice [BT; BF; BNeg; BIsLetter; BIsVowel; BIsDigit; BParParse]
    
    

    
    
    
    (* the below code we havent finished / implemented yet - smus *)
    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type internal word   = (char * int) list
    type internal squareFun = word -> int -> int -> Result<int, Error>
    type internal square = Map<int, squareFun>
    
    type internal boardFun2 = coord -> Result<square option, Error>
        
    type internal board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
