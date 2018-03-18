module SimplStructs where

data Expression
  = Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

