module LOOPBF.Types where 

  data BBOp = And 
            | Or 
            | Xor
            deriving (Show)

  data BAOp = Eq
            | Neq
            | Lt
            | Gt
            | Lte
            | Gte
            deriving (Show)

  data AOp = Plus
           | Minus
           | Mult
           | Div 
           deriving (Show)

  data BExpr = BValue Bool
             | BNot BExpr 
             | BBRel BBOp BExpr BExpr 
             | BARel BAOp AExpr AExpr 
             deriving (Show)

  data AExpr = AValue Integer
             | ANeg AExpr 
             | AVariable String
             | ARel AOp AExpr AExpr
             deriving (Show) 

  data Stmt = SSeq [Stmt]
            | SAssign String AExpr
            | SIf BExpr Stmt Stmt
            | SWhile BExpr Stmt 
            | SCall String String
            | SSkip
            deriving (Show)