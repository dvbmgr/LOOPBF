module LOOPBF.Parser (parseString, parseFile) where

  import Control.Monad
  import Control.Applicative ((<$>), (<*))
  import LOOPBF.Types
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Error
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  parseString :: 
                 String -> Stmt
  parseString str
    = case parse loopParser "(stdin)" str of
        Left e  -> error $ show e
        Right r -> r
 
  parseFile :: 
               String -> IO Stmt
  parseFile file
    = do program <- readFile file
         case parse loopParser file program of
           Left e  -> print e >> fail "parse error"
           Right r -> return r

  _infix_aop = [ ("+", Plus)
               , ("-", Minus)
               , ("*", Mult)
               , ("/", Div)
               ]
  _prefix_aop = [ ("-", ANeg)
                ]

  _infix_rop = [ ("!=", Neq)
               , ("=", Eq)
               , ("<", Lt)
               , (">", Gt)
               , ("<=", Lte)
               , (">=", Gte)
               ]

  _infix_bop = [ ("&&", And)
               , ("||", Or)
               , ("xor", Xor)
               ]
  _prefix_bop = [ ("~", BNot)
                ]

  (+%) :: 
          [a] -> [(a, c)] -> [a]
  (+%) a b 
    = a ++ map fst b 

  languageDef =
    emptyDef { Token.commentStart
                 = "/*",
               Token.commentEnd
                 = "*/",
               Token.commentLine
                 = "//",
               Token.identStart
                 = letter,
               Token.identLetter
                 = alphaNum <|> oneOf (['_']),
               Token.reservedNames
                 = [ "if"
                   , "then"
                   , "else"
                   ,  "while"
                   , "do"
                   , "true"
                   , "false"
                   ],
               Token.reservedOpNames
                 = map fst _infix_aop +% _prefix_aop +%
                   _infix_rop +% _infix_bop +% _prefix_bop,
               Token.caseSensitive
                 = True
             }

  lexer
    = Token.makeTokenParser languageDef

  identifier
    = Token.identifier lexer
  reserved
    = Token.reserved lexer
  reservedOp
    = Token.reservedOp lexer
  parens
    = Token.parens lexer
  braces 
    = Token.braces lexer
  integer
    = Token.integer lexer
  semi
    = Token.semi lexer
  comma 
    = Token.comma lexer
  whiteSpace
    = Token.whiteSpace lexer

  loopParser ::
                Parser Stmt
  loopParser
    = whiteSpace >>
      statement

  statement ::
               Parser Stmt 
  statement
    = sepBy1 statement' whiteSpace >>= \l ->
      if length l == 1 then 
        return $ head l 
      else
        return $ SSeq l

  statement' ::
                Parser Stmt 
  statement' 
    = ifElseStatement <|>
      ifStatement <|>
      whileStatement <|>
      skipStatement <|>
      try assignStatement <|>
      callStatement

  ifElseStatement ::
                 Parser Stmt
  ifElseStatement
    = do reserved "if"
         cond <- parens bExpr
         ift <- braces statement
         reserved "else"
         iff <- braces statement
         return $ SIf cond ift iff 

  ifStatement ::
                 Parser Stmt
  ifStatement
    = do reserved "if"
         cond <- parens bExpr
         ift <- braces statement
         return $ SIf cond ift (SSeq []) 

  whileStatement :: 
                    Parser Stmt 
  whileStatement
    = do reserved "while"
         cond <- parens bExpr
         whct <- braces statement
         return $ SWhile cond whct

  assignStatement ::
                     Parser Stmt 
  assignStatement
    = do name <- identifier
         reservedOp ":="
         expr <- aExpr
         semi
         return $ SAssign name expr

  callStatement ::
                   Parser Stmt 
  callStatement
    = do name <- identifier
         arg <- parens identifier
         semi
         return $ SCall name arg

  skipStatement ::
                   Parser Stmt 
  skipStatement
    = do reserved "skip"
         semi
         return $ SSkip

  aExpr ::
           Parser AExpr
  aExpr
    = buildExpressionParser aOp aTerm 
    where
      aOp =  [ [Infix  (reservedOp (fst op) >> return (ARel (snd op))) AssocLeft] | op <- _infix_aop ]
          ++ [ [Prefix (reservedOp (fst op) >> return (snd op))                 ] | op <- _prefix_aop ]
      aTerm = parens aExpr <|>
              liftM AVariable identifier <|>
              liftM AValue integer

  bExpr ::
           Parser BExpr 
  bExpr
    = buildExpressionParser bOp bTerm 
    where
      bOp =  [ [Infix  (reservedOp (fst op) >> return (BBRel (snd op))) AssocLeft] | op <- _infix_bop ]
          ++ [ [Prefix (reservedOp (fst op) >> return (snd op))                  ] | op <- _prefix_bop ]
      bTerm = parens bExpr <|>
              (reserved "true" >> return (BValue True)) <|>
              (reserved "false" >> return (BValue False)) <|>
              rExpr
      rExpr = do a1 <- aExpr 
                 op <- relation
                 a2 <- aExpr
                 return $ BARel op a1 a2
              where
                relation = foldl1 (<|>) [reservedOp (fst op) >> return (snd op) | op <- _infix_rop]