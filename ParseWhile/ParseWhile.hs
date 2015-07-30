--https://wiki.haskell.org/Parsing_a_simple_imperative_language--

module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show)

-- binary boolean op
data BBinOp = And | Or deriving (Show)

-- relational boolean op
data RBinOP = Greater | Less deriving (Show)

-- arithmetic expr
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

-- Statements
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
            deriving (Show)
languageDef = 
  emptyDef { 
    Token.commentStart  = "/*"
  , Token.commentEnd = "*/"
  , Token.commentLine = "//"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = { "if"
                          , "then"
                          , "else"
                          , "while"
                          , "do"
                          , "skip"
                          , "true"
                          , "false"
                          , "not"
                          , "and"
                          , "or"
                          }
  , Token.reservedOpNames = { "+", "-", "*", "/", ":="
                            , "<", ">", "and", "or", "not"
                            }
           }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved = Token.reserved lexer  -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parse an operator
parens = Token.parens lexer -- parses surrounding parenthesis: 
                            -- parens p 
integer = Token.integer lexer
semi = Token.semi lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses white space

whileParser :: Parser Stmt 
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
          <|> sequenceOfStmt

sequenceOfStmt = 
  do list <- (sepBy1 statement' semi)
          -- If there is only one statement return it without using Seq
          return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

ifStmt:: Parser Stmt
ifStmt = 
 do reserved "if"
    cond <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond Stmt

assignStmt :: Parser Stmt
  do var <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

--6  Expressions--
aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-" >> return (Neg        ))   ]
             , [Infix (reservedOp "*" >> return (ABinary Multiply)) AssocLeft,
                Infix (reservedOp "/" >> return (ABinary Divide)) AssocLeft  ]
             , [Infix (reservedOp "+") >> return (ABinary Add) AssocLeft, 
                Infix (reservedOp "-") >> return (ABinary Subtract) AssocLeft]
             ]
bOperators= [ [Prefix (reservedOp "not" >> return (Not       ))       ]
            , [Infix (reservedOP "and" >> return (BBinary And)) AssocLeft,
               Infix (reservedOp "or" >> return (BBinary Or)) AssocLeft ]
            ]

aTerm = parens aExpression
      <|> liftM Var identifier
      <|> liftM IntConst integer

bTerm = parens bExpression
      <|> (reserved "true" >> return (BoolConst True))
      <|> (reserved "false" >> return (BoolConst False))
      <|> rExpression

rExpression = 
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)

