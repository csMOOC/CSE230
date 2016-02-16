-- ---
-- title: Homework #2, Due Friday 2/12/16
-- ---

{-# LANGUAGE TypeSynonymInstances #-}
module Hw2 where

import Control.Applicative hiding (empty, (<|>))
import Data.Map hiding (foldl, delete)
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String


-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Zhenchao Gan"
myEmail = "zhgan@eng.ucsd.edu"
mySID   = "A53092819"


-- Problem 1: All About `foldl`
-- ============================

-- Define the following functions by filling in the "error" portion:

-- 1. Describe `foldl` and give an implementation:

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ b []     = b
myFoldl f b (x:xs) = let tmp = f b x
                      in myFoldl f tmp xs

-- 2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

myReverse :: [a] -> [a]
myReverse xs = Prelude.foldl (\ys y -> y:ys) [] xs

-- 3. Define `foldr` in terms of `foldl`:

myFoldr :: (a -> b -> b) -> b -> [a] -> b
--myFoldr f b xs = (myFoldl (\g b x -> g (f b x)) id xs) b
myFoldr f b xs = (myFoldl (\g b x -> g (f b x)) id xs) b

-- 4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f b xs = (Prelude.foldr (\b g x -> g (f x b)) id xs) b

-- 5. Try applying `foldl` to a gigantic list. Why is it so slow?
--    Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
--    instead; can you explain why it's faster?
temp1 = Prelude.foldl (+) 0 [1..1000000]

--temp2 = Data.List.foldl' (+) 0 [1..1000000]

{-
When using foldl', inner redex is repeatedly reduced first, so the stack will not grow.
So it can run in constant space for most uses.
Take the function sum = foldl['] (+) 0. When foldl' is used,
the sum is immediately calculated,
so applying sum to an infinite list will just run forever,
and most likely in constant space

With foldl, a thunk is built up, taking up a lot of space,
and in this case, it’s much better to evaluate the expression than to store the thunk
(leading to a stack overflow)
-}



-- Part 2: Binary Search Trees
-- ===========================

-- Recall the following type of binary search trees:

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show)

-- Define a `delete` function for BSTs of this type:

--test cases
tree1 = Bind 8 1 (Bind 5 4 Emp Emp) (Bind 10 9 Emp Emp)
tree2 = Bind 5 1 (Bind 2 2 (Bind (-4) 3 Emp Emp) (Bind 3 4 Emp Emp)) (Bind 18 5 Emp Emp)
tree3 = Bind 5 1 Emp (Bind 18 2 Emp (Bind 21 3 (Bind 19 4 Emp Emp) (Bind 25 5 Emp Emp)))
tree4 = Bind 5 1 Emp (Bind 12 2 (Bind 9 3 Emp Emp) (Bind 21 4 (Bind 19 5 Emp Emp) (Bind 25 6 Emp Emp)))

delete :: (Ord k) => k -> BST k v -> BST k v
delete k Emp = Emp
delete k (Bind k' v t1 t2)
  | k' == k = deleteX (Bind k v t1 t2)
  | k'  > k = Bind k' v (delete k t1)  t2
  | k'  < k = Bind k' v t1  (delete k t2)

deleteX :: (Ord k) => BST k v -> BST k v
deleteX ( Bind k' v Emp t2 ) = t2
deleteX ( Bind k' v t1 Emp ) = t1
deleteX ( Bind k' v t1  t2 ) = ( Bind (fst v2) (snd v2) t1 newtree2 )
  where
    v2 = minimumEle t2
    newtree2 = clean t2

minimumEle :: (Ord k) => BST k v -> (k, v)
minimumEle ( Bind k v Emp _ ) = (k, v)
minimumEle ( Bind k v t1  _ ) = minimumEle t1

clean :: (Ord k) => BST k v -> BST k v
clean Emp = Emp
clean ( Bind k v Emp t2 ) = t2
clean ( Bind k v t1 t2 ) = Bind k v (clean t1) t2

--TEST =

-- Part 3: An Interpreter for WHILE
-- ================================

-- Next, you will use monads to build an evaluator for
-- a simple *WHILE* language. In this language, we will
-- represent different program variables as

type Variable = String

-- Programs in the language are simply values of the type

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

-- where expressions are variables, constants or
-- binary operators applied to sub-expressions

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
  deriving (Show)

-- and binary operators are simply two-ary functions

data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

-- We will represent the *store* i.e. the machine's memory, as an associative
-- map from `Variable` to `Value`

type Store = Map Variable Value

-- **Note:** we don't have exceptions (yet), so if a variable
-- is not found (eg because it is not initialized) simply return
-- the value `0`. In future assignments, we will add this as a
-- case where exceptions are thrown (the other case being type errors.)

-- We will use the standard library's `State`
-- [monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
-- to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer
-- `s -> (a, s)`. See the above documentation for more details.
-- You can ignore the bits about `StateT` for now.

-- Expression Evaluator
-- --------------------

-- First, write a function

evalE :: Expression -> State Store Value

-- that takes as input an expression and returns a world-transformer that
-- returns a value. Yes, right now, the transformer doesnt really transform
-- the world, but we will use the monad nevertheless as later, the world may
-- change, when we add exceptions and such.

-- **Hint:** The value `get` is of type `State Store Store`. Thus, to extract
-- the value of the "current store" in a variable `s` use `s <- get`.

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus (IntVal i) (IntVal j) = IntVal (i+j)

-- >

evalE (Var x)       = do
                        table <- get
                        return $ findWithDefault (IntVal 0) x table
evalE (Val v)       = return v
evalE (Op o e1 e2)  = do
                       (IntVal v1)  <- evalE(e1)
                       (IntVal v2)  <- evalE(e2)
                       case o of
                          Plus      -> return $ IntVal  (v1   +   v2)
                          Minus     -> return $ IntVal  (v1   -   v2)
                          Times     -> return $ IntVal  (v1   *   v2)
                          Divide    -> return $ IntVal  (v1 `div` v2)
                          Gt        -> return $ BoolVal (v1   >   v2)
                          Ge        -> return $ BoolVal (v1   >=  v2)
                          Lt        -> return $ BoolVal (v1   <   v2)
                          Le        -> return $ BoolVal (v1   <=  v2)







-- Statement Evaluator
-- -------------------

-- Next, write a function

evalS :: Statement -> State Store ()

-- that takes as input a statement and returns a world-transformer that
-- returns a unit. Here, the world-transformer should in fact update the input
-- store appropriately with the assignments executed in the course of
-- evaluating the `Statement`.

-- **Hint:** The value `put` is of type `Store -> State Store ()`.
-- Thus, to "update" the value of the store with the new store `s'`
-- do `put s'`.

evalS (Assign x e )    = do
                          table <- get
                          v <- evalE e
                          put $ insert x v table

evalS w@(While e s)    = do
                          v <- evalE e
                          case v of
                            IntVal _      -> return ()
                            BoolVal False -> return ()
                            BoolVal True  -> do
                                               evalS s
                                               evalS w

evalS Skip             = return ()

evalS (Sequence s1 s2) = do
                          evalS s1
                          evalS s2

evalS (If e s1 s2)     = do
                          v <- evalE e
                          case v of
                            IntVal _       -> return ()
                            BoolVal True   -> evalS s1
                            BoolVal False  -> evalS s2


-- In the `If` case, if `e` evaluates to a non-boolean value, just skip both
-- the branches. (We will convert it into a type error in the next homework.)
-- Finally, write a function

execS :: Statement -> Store -> Store
execS stmt = execState $ evalS stmt

-- such that `execS stmt store` returns the new `Store` that results
-- from evaluating the command `stmt` from the world `store`.
-- **Hint:** You may want to use the library function

-- ~~~~~{.haskell}
-- execState :: State s a -> s -> s
-- ~~~~~

-- When you are done with the above, the following function will
-- "run" a statement starting with the `empty` store (where no
-- variable is initialized). Running the program should print
-- the value of all variables at the end of execution.

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:"
              putStrLn $ show $ execS stmt empty

-- Here are a few "tests" that you can use to check your implementation.

w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

-- As you can see, it is rather tedious to write the above tests! They
-- correspond to the code in the files `test.imp` and `fact.imp`. When you are
-- done, you should get

-- ~~~~~{.haskell}
-- ghci> run w_test
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> run w_fact
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~

-- Problem 4: A Parser for WHILE
-- =============================

-- It is rather tedious to have to specify individual programs as Haskell
-- values. For this problem, you will use parser combinators to build a parser
-- for the WHILE language from the previous problem.

-- Parsing Constants
-- -----------------

-- First, we will write parsers for the `Value` type

valueP :: Parser Value
valueP = intP <|> boolP

-- To do so, fill in the implementations of

intP :: Parser Value
intP = do
        x <- many1 digit
        return $ IntVal $ read x

-- Next, define a parser that will accept a
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = do
              string s
              return x

-- and use the above to define a parser for boolean values
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

-- Continue to use the above to parse the binary operators

opP :: Parser Bop
opP =     constP "+" Plus
      <|> constP "-" Minus
      <|> constP "*" Times
      <|> constP "/" Divide
      <|> constP ">" Gt
      <|> constP ">=" Ge
      <|> constP "<" Lt
      <|> constP "<=" Le


-- Parsing Expressions
-- -------------------

-- Next, the following is a parser for variables, where each
-- variable is one-or-more uppercase letters.

varP :: Parser Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values

exprP :: Parser Expression
exprP = try expExprP <|> parenExp <|> simpleVar <|> simpleVal
     where
       simpleVar = do
         v <- varP
         return $ Var v
       simpleVal = do
         v <- valueP
         return $ Val v
       parenExp = do
         string "("
         x <- exprP
         string ")"
         return $ x
       expExprP = do
         x <- try simpleVar <|> simpleVal <|> parenExp
         skipMany space
         o <- opP
         skipMany space
         y <- exprP
         return $ Op o x y


-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser

statementP :: Parser Statement
statementP = try sequenceP <|> assignP <|> ifP <|> whileP <|> skipP
     where
        assignP = do
          v <- varP
          skipMany space
          string ":="
          skipMany space
          e <- exprP
          skipMany space
          return $ Assign v e
        ifP = do
          string "if"
          skipMany space
          e  <- exprP
          skipMany space
          string "then"
          skipMany space
          s1 <- statementP
          skipMany space
          string "else"
          skipMany space
          s2 <- statementP
          skipMany space
          string "endif"
          return $ If e s1 s2
        whileP = do
          string "while"
          skipMany space
          e <- exprP
          skipMany space
          string "do"
          skipMany space
          s <- statementP
          skipMany space
          string "endwhile"
          return $ While e s
        sequenceP = do
          s1 <- try assignP <|> ifP <|> whileP <|> skipP
          skipMany space
          string ";"
          skipMany space
          s2 <- statementP
          return $ Sequence s1 s2
        skipP = constP "skip" Skip

-- When you are done, we can put the parser and evaluator together
-- in the end-to-end interpreter function

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt

-- When you are done you should see the following at the ghci prompt

-- ~~~~~{.haskell}
-- ghci> runFile "test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> runFile "fact.imp"
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~
