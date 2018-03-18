module Main where

import SimplParser
import SimplStructs
import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import Control.Exception as Exc

main :: IO ()
main = do
  args <- getArgs
  res  <- Exc.try $ do
    state <- runProgram args
    print $ state "Out"
  case res of
    Left e -> putStrLn $ "Error: " ++ ioeGetErrorString e
    _      -> return () 
  return ()

runProgram :: [String] -> IO State
runProgram [file, input] = do
  prog <- do
    e <- doesFileExist file 
    if e then readFile file else error "File does not exist"
  return $ parseAndRun prog $ read input
runProgram _ = error "Invalid number of arguments"

parseAndRun :: String -> Int -> State
parseAndRun prog input = case ast of
  Just a  -> run state a
  Nothing -> error "Parse failed!"
  where 
    ast = Main.parse prog
    state = extend empty "In" input

-- Part 01 -----------------------------------------

type State = String -> Int

extend :: State -> String -> Int -> State
extend prevState var val x = if x == var then val else prevState x

empty :: State
empty _ = 0

-- Part 02 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0

evalE :: State -> Expression -> Int
evalE st (Var varName) = st varName
evalE _  (Val value)   = value
evalE st (Op e1 bop e2) = case bop of
  Plus   -> e1Eval + e2Eval
  Minus  -> e1Eval - e2Eval
  Times  -> e1Eval * e2Eval
  Divide -> e1Eval `div` e2Eval
  Gt     -> boolToInt $ e1Eval >  e2Eval
  Ge     -> boolToInt $ e1Eval >= e2Eval
  Lt     -> boolToInt $ e1Eval <  e2Eval
  Le     -> boolToInt $ e1Eval <= e2Eval
  Eql    -> boolToInt $ e1Eval == e2Eval
  where 
    e1Eval = evalE st e1
    e2Eval = evalE st e2

-- Part 03 -----------------------------------------

desugar :: Statement -> DietStatement
desugar (Assign varName expr)    = DAssign varName expr
desugar (Incr varName)           = DAssign varName (Op (Var varName) Plus (Val 1))
desugar (If cond s1 s2)          = DIf cond (desugar s1) (desugar s2)
desugar (While cond s)           = DWhile cond (desugar s)
desugar (Sequence s1 s2)         = DSequence (desugar s1) (desugar s2)
desugar Skip                     = DSkip
desugar (For init cond update s) = DSequence dinit (DWhile cond (DSequence ds dupdate))
  where
    dinit   = desugar init
    dupdate = desugar update
    ds      = desugar s

-- Part 04 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign varName expr) = extend st varName $ evalE st expr
evalSimple st (DIf cond s1 s2)       = if intToBool $ evalE st cond then evalSimple st s1 else evalSimple st s2
evalSimple st (DWhile cond s)        = if intToBool $ evalE st cond then evalSimple (evalSimple st s) (DWhile cond s) else st
evalSimple st (DSequence s1 s2)      = evalSimple (evalSimple st s1) s2
evalSimple st DSkip                  = st

run :: State -> Statement -> State
run st = evalSimple st . desugar

-- Part 05 -----------------------------------------

parse :: String -> Maybe Statement
parse = SimplParser.parse

-- Programs ----------------------------------------

{- Calculate the factorial of the input
  for (Out := 1; In > 0; In := In - 1) {
      Out := In * Out
  }
-}

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{-
  Calculate the floor of the square root of the input
  B := 0;
  while (A >= B * B) {
      B++
  };
  B := B - 1
-}

squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{-
  Calculate the nth Fibonacci number

  F0 := 1;
  F1 := 1;

  if (In == 0) {
      Out := F0
  } else {
      if (In == 1) {
          Out := F1
      } else {
          for (C := 2; C <= In; C++) {
              T  := F0 + F1;
              F0 := F1;
              F1 := T;
              Out := T
          }
      }
  }

-}

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
