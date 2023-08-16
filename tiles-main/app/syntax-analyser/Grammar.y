{ 
module Grammar where 
import Tokens  --taking the datatype from there
}


%name parseTiles
%tokentype { Token } 
%error { parseError } 
%token  
    tile                   {TokenTile           _ }
    endLine                {TokenEndln _}
    variable               {TokenVar _ $$          }  
    integer                {TokenInt _ $$         }  
    assign                 {TokenAssign         _}    
    plus                   {TokenPlus           _}    
    minus                  {TokenMinus          _}    
    multiply               {TokenMultiply       _}    
    comma                  {TokenComma          _}
    lBracket               {TokenLBracket       _}
    rBracket               {TokenRBracket       _}
    hBuild                 {TokenHorizontalBuild _}    
    vBuild                 {TokenVerticalBuild  _}    
    rotate                 {TokenRotate         _}    
    scale                  {TokenScale          _}    
    compSize               {TokenCompareSize    _}    
    fst                    {TokenFst            _}    
    snd                    {TokenSnd            _}    
    blankPiece             {TokenBlankPiece     _}    
    diagSplit              {TokenDiagonalSplit  _}    
    hStick                 {TokenHorizontalStick _}    
    vStick                 {TokenVerticalStick  _}    
    and                    {TokenAnd            _}    
    negate                 {TokenNegate         _}    
    createSubtile          {TokenCreateSubtile  _}

%left createSubtile
%left assign
--%right repeat
%left endln
%left plus minus
%left multiply
%left comma
%left hBuild vBuild
%left hStick vStick diagSplit
--%left equivalent
%left compSize
%left fst snd
--%nonassoc length
--%nonassoc if else
--%nonassoc lSquareBracket rSquareBracket

--%nonassoc lCurlyBracket rCurlyBracket
-- %nonassoc if else
%nonassoc lBracket rBracket
%%
Code : Exp endLine Code            {Lines $1 $3}
     | Exp                          {OneLine $1}
Exp : variable assign Function     {Assign $1 $3}
Function : hBuild Integer Function {Hbuild $2 $3}
          | vBuild Integer Function  {Vbuild $2 $3}
          | rotate Integer Function   {Rotate $2 $3}
          | scale Integer Function   {Scale $2 $3}
          | blankPiece Pair            {BlankPiece $2}
          | diagSplit Pair Function Function {DiagSplit $2 $3 $4}
          | hStick Function Function    {Hstick $2 $3}
          | vStick Function Function    {Vstick $2 $3}
          | and Function Function       {And $2 $3}
          | negate Function               {Negate $2}
          | createSubtile Function Pair Integer {CreateSubtile $2 $3 $4}
          | tile List                           {Tile $2}  -- this one dodgy idk
          | variable                        {Variable $1}
Integer : integer                         {Int $1}   -- idk
          | Integer plus Integer        { Plus $1 $3}
          | Integer minus Integer        { Minus $1 $3}
          | Integer multiply Integer        { Multiply $1 $3}
          | fst Pair                          {Fst $2}
          | snd Pair                          {Snd $2}
Pair : Integer comma Integer              { Comma $1 $3}
       | compSize Function                    {CompSize $2}

List : lBracket Elements rBracket      { List $2 }

Elements : Integer                             {A $1 }
         | Integer comma Elements              { X $1 $3 }
         | List comma Elements                 { Y $1 $3 }
         | List                                { Z $1}                             


{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Code = Lines Exp Code | OneLine Exp    deriving (Show,Eq)

data Exp = Assign String Function 
    deriving (Show,Eq)

data Function = Hbuild Integer' Function | Vbuild Integer' Function | Rotate Integer' Function | Scale Integer' Function | BlankPiece Pair | DiagSplit Pair Function Function | Hstick Function Function | Vstick Function Function | And Function Function | Negate Function | CreateSubtile Function Pair Integer' |  Tile List | Variable String
    deriving (Show,Eq)

data Integer' = Int Int | Plus Integer' Integer' | Minus Integer' Integer' | Multiply Integer' Integer' | Fst Pair | Snd Pair 
    deriving (Show,Eq)

data Pair =  CompSize Function | Comma Integer' Integer' 
    deriving (Show,Eq)

data List = List Elements deriving (Show,Eq)

data Elements = A Integer' | X Integer' Elements | Y List Elements | Z List  deriving (Show,Eq)

}


