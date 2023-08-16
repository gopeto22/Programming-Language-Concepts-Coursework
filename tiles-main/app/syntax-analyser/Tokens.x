{
module Tokens where
}

%wrapper "posn" 

$digit = 0-9     
$alpha = [a-zA-Z] 

tokens :-
    $white+                               ;
    "~".*                                 ;  
    $digit+                         {\pos s -> TokenInt pos (read s) }
    "\n"                            {\pos s -> TokenEndln pos}
    "="                             {\pos s -> TokenAssign pos}
    "+"                             {\pos s -> TokenPlus pos }
    "-"                             {\pos s -> TokenMinus pos }
    "*"                             {\pos s -> TokenMultiply pos}
    ","                             {\pos s -> TokenComma pos}
    "["                             {\pos s -> TokenLBracket pos}
    "]"                             {\pos s -> TokenRBracket pos}
    tile                            {\pos s -> TokenTile pos}
    horizontalBuild                 {\pos s -> TokenHorizontalBuild pos}
    verticalBuild                   {\pos s -> TokenVerticalBuild pos}
    rotate                          {\pos s -> TokenRotate pos }  
    scale                           {\pos s -> TokenScale pos}     
    compareSize                     {\pos s -> TokenCompareSize pos}
    fst                             {\pos s -> TokenFst pos}
    snd                             {\pos s -> TokenSnd pos}      
    blankPiece                      {\pos s -> TokenBlankPiece pos}    
    diagonalSplit                   {\pos s -> TokenDiagonalSplit pos}
    horizontalStick                 {\pos s -> TokenHorizontalStick pos}
    verticalStick                   {\pos s -> TokenVerticalStick pos}
    and                             {\pos s -> TokenAnd pos} 
    negate                          {\pos s -> TokenNegate pos} 
    createSubtile                   {\pos s -> TokenCreateSubtile pos}
    $alpha [$alpha $digit \_ \']*   {\pos s -> TokenVar pos s }   --TokenVar carries the s value 

{
data Token =  TokenTile AlexPosn
            | TokenInt AlexPosn Int           
            | TokenAssign              AlexPosn
            | TokenEndln               AlexPosn
            | TokenPlus                AlexPosn
            | TokenMinus               AlexPosn
            | TokenMultiply            AlexPosn
            | TokenComma               AlexPosn
            | TokenLBracket            AlexPosn
            | TokenRBracket            AlexPosn
            | TokenHorizontalBuild     AlexPosn
            | TokenVerticalBuild       AlexPosn
            | TokenRotate              AlexPosn
            | TokenScale               AlexPosn
            | TokenCompareSize         AlexPosn
            | TokenFst                 AlexPosn
            | TokenSnd                 AlexPosn
            | TokenBlankPiece          AlexPosn
            | TokenDiagonalSplit       AlexPosn
            | TokenHorizontalStick     AlexPosn
            | TokenVerticalStick       AlexPosn
            | TokenAnd                 AlexPosn
            | TokenNegate              AlexPosn
            | TokenCreateSubtile       AlexPosn
            | TokenVar AlexPosn String  
            deriving (Eq,Show)

tokenPosn (TokenVar (AlexPn a l c)n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndln (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c)n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAssign (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHorizontalBuild (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVerticalBuild (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRotate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenScale (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCompareSize (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFst (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBlankPiece (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiagonalSplit (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHorizontalStick (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVerticalStick (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNegate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCreateSubtile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

}