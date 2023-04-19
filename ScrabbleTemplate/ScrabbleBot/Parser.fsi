// This file is new and hides the implementation details of the parser.

module internal Parser
    
    open ScrabbleUtil
    open StateMonad
    
    type internal word   = (char * int) list
    type internal squareFun = word -> int -> int -> Result<int, Error>
    type internal square = Map<int, squareFun>
    
    type internal boardFun2 = coord -> Result<square option, Error>
        
    type internal board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    val mkBoard : boardProg -> board