module SimplParser where

import Control.Applicative((<*))
import Text.Parsec as Ps
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token as Tok
import Text.Parsec.Language
import SimplStructs

parse :: String -> Maybe Statement
parse prog = case stmt of
  (Right s) -> Just s
  _         -> Nothing
  where stmt = Ps.parse mainParser "" prog

mainParser :: Parser Statement
mainParser = m_whiteSpace >> stmtParser <* eof

-- === Language definition ==========================================

langDef :: Tok.LanguageDef ()
langDef = emptyDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf "+-*/<=>:"
  , Tok.opLetter        = oneOf "+*/<=>:"
  , Tok.reservedOpNames = ["+", "-", "*", "/", "<", "==", ">", "<=", ">=", ":=", "++"]
  , Tok.reservedNames   = ["skip", "if", "else", "while", "for"]
  , Tok.caseSensitive   = True
  }

-- ==== Token parser =================================================

TokenParser{ parens     = m_parens
           , braces     = m_braces
           , identifier = m_identifier
           , integer    = m_integer
           , reservedOp = m_reservedOp
           , reserved   = m_reserved
           , semiSep1   = m_semiSep1
           , symbol     = m_symbol
           , whiteSpace = m_whiteSpace } = makeTokenParser langDef

-- ==== Expression parser ============================================

exprParser :: Parser Expression
exprParser = buildExpressionParser table term <?> "expression"


table = [ [Infix (m_reservedOp "*"  >> return (mkOp Times))  AssocLeft
          ,Infix (m_reservedOp "/"  >> return (mkOp Divide)) AssocLeft]
        , [Infix (m_reservedOp "+"  >> return (mkOp Plus))   AssocLeft
          ,Infix (m_reservedOp "-"  >> return (mkOp Minus))  AssocLeft]
        , [Infix (m_reservedOp "<"  >> return (mkOp Lt))     AssocLeft
          ,Infix (m_reservedOp ">"  >> return (mkOp Gt))     AssocLeft
          ,Infix (m_reservedOp "<=" >> return (mkOp Le))     AssocLeft
          ,Infix (m_reservedOp ">=" >> return (mkOp Ge))     AssocLeft
          ,Infix (m_reservedOp "==" >> return (mkOp Eql))    AssocLeft]
        ]
  where mkOp op a b = Op a op b

variable :: Parser Expression
variable = Var <$> m_identifier

literal :: Parser Expression
literal = (Val . fromIntegral) <$> m_integer

term :: Parser Expression
term = m_parens exprParser
       <|> variable
       <|> literal

-- ==== Statement parser =============================================

stmtParser :: Parser Statement
stmtParser = slist <$> m_semiSep1 stmtParser'

stmtParser' :: Parser Statement
stmtParser' = (m_reserved "skip" >> return Skip)
              <|> Ps.try incrParser
              <|> assignParser
              <|> ifParser
              <|> whileParser
              <|> forParser

assignParser :: Parser Statement
assignParser = do  
  v <- m_identifier;
  m_reservedOp ":=";
  e <- exprParser;
  return (Assign v e)

incrParser :: Parser Statement
incrParser = Incr <$> m_identifier <* m_reservedOp "++"

ifParser :: Parser Statement
ifParser = do
  m_reserved "if";
  cond <- m_parens exprParser;
  s1   <- m_braces stmtParser;
  m_reserved "else";
  s2   <- m_braces stmtParser;
  return (If cond s1 s2)

whileParser :: Parser Statement
whileParser = do
  m_reserved "while";
  cond <- m_parens exprParser;
  s    <- m_braces stmtParser;
  return (While cond s)

forParser :: Parser Statement
forParser = do
  m_reserved "for";
  m_symbol "(";
  init <- forStmtParser;
  m_symbol ";"
  cond <- exprParser;
  m_symbol ";"
  step <-forStmtParser;
  m_symbol ")"
  body <- m_braces stmtParser
  return (For init cond step body)
  where
    forStmtParser :: Parser Statement
    forStmtParser = Ps.try incrParser <|> assignParser

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

