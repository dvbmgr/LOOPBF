module LOOPBF.Translate where

  import LOOPBF.Types
  import Control.Monad (when)
  import Data.Maybe
  import Data.IORef

  data StackElem = StackElem { position :: Int
                             , identifier :: String 
                             }

  data Stack = Stack { elements :: [StackElem]
                     , currentposition :: Int 
                     }

  data LoopBF = LoopBF { out :: String
                       , stack :: Stack 
                       }

  type Machine = IORef LoopBF

  (%) :: a -> [a] ->  [a]
  (%) = flip (++) . flip (:) []

  newMachine :: 
                IO Machine
  newMachine 
    = newIORef (LoopBF { out = []
                       , stack = Stack { elements = []
                                       , currentposition = 0
                                       }
                       }
               )

  _moveC :: Int -> String
  _moveC a
    = take (abs a) $ repeat (if a < 0 then '<' else '>')

  new :: 
         Machine
      -> String
      -> IO ()
  new m name
    = readIORef m >>= \v ->
      writeIORef m $ v { stack = (stack v) { elements = StackElem { position = length . elements . stack $ v
                                                                , identifier = name 
                                                                  } % (elements . stack) v
                                           , currentposition = length . elements . stack $ v
                                           }
                       , out = out v ++ _moveC ((length $ elements $ stack v) - (currentposition $ stack v))
                       } 

  temp :: 
          Machine 
       -> IO String
  temp m
    = readIORef m >>= \v ->
      let temp = "__temp_" ++ show (1 + (length . elements . stack $ v)) in
      new m temp >>
      return temp
      
  write ::
           Machine
        -> String
        -> IO ()
  write m str 
    = readIORef m >>= \v ->
      writeIORef m $ v { out = out v ++ str }

  goto :: 
          Machine
       -> String
       -> IO ()
  goto m name 
    = readIORef m >>= \v ->
      let elems = (elements . stack) v in 
      let elemMatchList = filter (\x -> identifier x == name) elems in 
      if not $ null elemMatchList then 
        let elem = head elemMatchList in 
        writeIORef m $ v { stack = (stack v) { currentposition = position elem }
                         , out = out v ++ (_moveC $ (position elem) - (currentposition $ stack v))}
      else 
        error $ "Unable to find " ++ name ++ " in stack"

  get ::
         Machine
      -> IO Int
  get m
    = readIORef m >>= \v ->
      return $ currentposition $ stack $ v

  move :: 
          Machine
       -> Int
       -> IO ()
  move m p 
    = readIORef m >>= \v ->
      write m $ _moveC (p - (currentposition $ stack $ v))

  end :: 
         Machine
      -> IO String 
  end m 
    = readIORef m >>= \v ->
      return $ out v

  eval :: 
          Stmt 
       -> IO String
  eval stmt
    = do m <- newMachine
         evalStmt m stmt
         end m 

  evalAExpr ::
               Machine
            -> AExpr 
            -> IO ()
  evalAExpr m (AValue v)
    = do write     m ("[-]" ++ (take (fromIntegral v) $ repeat '+'))
  evalAExpr m (ANeg v)
    = do pos <- get m
         temp0 <- temp m 
         move      m pos
         evalAExpr m v
         write     m "["
         goto      m temp0
         write     m "-"
         move      m pos 
         write     m "-"
         goto      m temp0
         write     m "["
         move      m pos
         write     m "-"
         goto      m temp0
         write     m "+]"
         move      m pos
  evalAExpr m (AVariable name)
    = do px <- get m
         temp0 <- temp m 
         move      m px
         write     m "[-]"
         goto      m name
         write     m "["
         move      m px
         write     m "+"
         goto      m temp0
         goto      m name
         write     m "-]"
         goto      m temp0
         write     m "["
         goto      m name 
         write     m "+"
         goto      m temp0
         write     m "-]"
         move      m px
  evalAExpr m (ARel Plus e1 e2)
    = do pe <- get m
         x <- temp m 
         evalAExpr m e1 
         y <- temp m 
         evalAExpr m e2
         goto      m x
         write     m "["
         move      m pe
         write     m "+"
         goto      m x
         write     m "-]"
         goto      m y
         write     m "["
         move      m pe
         write     m "+"
         goto      m y
         write     m "-]"
         move      m pe 
  evalAExpr m (ARel Minus e1 e2)
    = do pe <- get m
         x <- temp m 
         evalAExpr m e1 
         y <- temp m 
         evalAExpr m e2
         goto      m x
         write     m "["
         move      m pe
         write     m "+"
         goto      m x
         write     m "-]"
         goto      m y
         write     m "["
         move      m pe
         write     m "-"
         goto      m y
         write     m "-]"
         move      m pe
  evalAExpr m (ARel Mult e1 e2)
    = do pe <- get m
         x <- temp m
         evalAExpr m e1
         y <- temp m 
         evalAExpr m e2
         temp0 <- temp m 
         temp1 <- temp m 
         goto      m x
         write     m "["
         goto      m temp1
         write     m "+"
         goto      m x 
         write     m "-]"
         goto      m temp1
         write     m "["
         goto      m y
         write     m "["
         move      m pe
         write     m "+"
         goto      m x 
         write     m "+"
         goto      m temp0
         write     m "+"
         goto      m y
         write     m "-]"
         goto      m temp0
         write     m "["
         goto      m y
         write     m "+"
         goto      m temp0
         write     m "-]"
         goto      m temp1
         write     m "-]"
  evalAExpr m (ARel Div e1 e2)
    = do pe <- get m
         x <- temp m 
         evalAExpr m e1
         y <- temp m 
         evalAExpr m e2
         temp0 <- temp m 
         temp1 <- temp m 
         temp2 <- temp m 
         temp3 <- temp m
         goto      m x 
         write     m "["
         goto      m temp0
         write     m "+"
         goto      m x
         write     m "-]"
         goto      m temp0
         write     m "["
         goto      m y
         write     m "["
         goto      m temp1
         write     m "+"
         goto      m temp2
         write     m "+"
         goto      m y
         write     m "-]"
         goto      m temp2
         write     m "[" 
         goto      m y 
         write     m "+"
         goto      m temp2 
         write     m "-"
         goto      m temp1
         write     m "["
         goto      m temp2
         write     m "+"
         goto      m temp0 
         write     m "-["
         goto      m temp2 
         write     m "[-]"
         goto      m temp3
         write     m "+"
         goto      m temp0
         write     m "-]"
         goto      m temp3
         write     m "["
         goto      m temp0
         write     m "+"
         goto      m temp3 
         write     m "-]"
         goto      m temp2
         write     m "["
         goto      m temp1 
         write     m "-["
         goto      m x
         write     m "-"
         goto      m temp1 
         write     m "[-]]+"
         goto      m temp2
         write     m "-]"
         goto      m temp1 
         write     m "-]"
         goto      m x 
         write     m "+"
         goto      m temp0 
         write     m "]"
         goto      m x 
         write     m "["
         move      m pe 
         write     m "+"
         goto      m x 
         write     m "-]"
         move      m pe

  evalBExpr ::
               Machine
            -> BExpr 
            -> IO String
  evalBExpr _ _ = return "BEXPR"

  evalStmt :: 
              Machine
           -> Stmt
           -> IO ()
  evalStmt m (SSeq xs)
    = mapM_ (evalStmt m) xs 
  evalStmt m (SAssign name expr)
    = do new       m name 
         evalAExpr m expr
  evalStmt m (SIf expr stmt1 stmt2)
    = do temp0 <- temp m 
         temp1 <- temp m
         x <- temp m 
         goto      m x
         evalBExpr m expr 
         goto      m temp0 
         write     m "[-]+"
         goto      m temp1 
         write     m "[-]"
         goto      m x
         write     m "["
         evalStmt  m stmt1
         goto      m temp0 
         write     m "-"
         goto      m x
         write     m "["
         goto      m temp1
         write     m "+"
         goto      m x
         write     m "-]]"
         goto      m temp1
         write     m "["
         goto      m x
         write     m "+"
         goto      m temp1
         write     m "-]"
         goto      m temp0 
         write     m "["
         evalStmt  m stmt2
         goto      m temp0
         write     m "]"
  evalStmt m (SWhile expr stmt1)
    = do x <- temp m
         goto      m x 
         evalBExpr m expr 
         write     m "["
         evalStmt  m stmt1
         goto      m x
         write     m "]"
  evalStmt m (SCall fn name)
    = do goto m name 
         case fn of 
           "print" -> 
             write m "."
           "input" ->
             write m ","
           _ ->
             error $ "Undefined function " ++ fn
  evalStmt m (SSkip)
    = do return ()
