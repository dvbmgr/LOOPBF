module Main where

  import LOOPBF.Types
  import LOOPBF.Parser
  import LOOPBF.Translate

  testParsed :: Stmt 
  testParsed
    = SSeq [SAssign "continue" (AValue 1),SAssign "x" (AValue 10),SAssign "y" (AValue 1),SWhile (BARel Eq (AVariable "continue") (AValue 1)) (SSeq [SAssign "y" (ARel Mult (AVariable "y") (AVariable "x")),SAssign "x" (ARel Minus (AVariable "x") (AValue 1)),SIf (BARel Eq (AVariable "x") (AValue 1)) (SAssign "continue" (AValue 0)) SSkip]),SCall "print" "y"]
    
  test :: IO Stmt
  test
    = parseFile "test.loop"

  main :: IO ()
  main = do v <- test 
            print v 
            m <- eval v 
            putStrLn m