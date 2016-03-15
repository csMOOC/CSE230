Part 3: An Interpreter for WHILE
================================

\begin{code}
{-@ LIQUID "--no-termination" @-}

module Interpreter (interpret) where

import           Prelude hiding (lookup)
import qualified Data.Set as S
import qualified Data.List as L

\end{code}

**HINT:** To do this problem, first go through [this case study](http://ucsd-progsys.github.io/lh-workshop/05-case-study-eval.html)
also included in your tarball as `case-study-eval.lhs`.

Next, you will revisit your interpreter for the *WHILE* 
language to ensure that execution never fails due to a 
*use-before-definition* error.

Recall that FIXME.

Programs in the language are simply values of the type

\begin{code}
data Statement =
    Assign Variable Expression            -- x = e
  | IfZ    Expression Statement Statement -- if (e) {s1} else {s2}
  | WhileZ Expression Statement           -- while (e) {s}
  | Sequence Statement Statement          -- s1; s2
  | Skip                                  -- no-op
\end{code}

to simplify matters, we assume the branch statement `IfZ e s1 s2`
evaluates `e` and then executes `s1` if the value of `e` equals `0`
and otherwise evaluates `s2`.

Thus, the expressions are variables, constants or binary operators applied to sub-expressions

\begin{code}
data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
\end{code}

and binary operators are simply two-ary functions

\begin{code}
data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
\end{code}

and variables and values are just:

\begin{code}
type Variable = String
type Value    = Int
\end{code}

Store
-----

We will represent the *store* i.e. the machine's memory, as a list of 
`Variable` - `Value` pairs:

\begin{code}
type Store = [(Variable, Value)]

update :: Store -> Variable -> Value -> Store 
update st x v = (x, v) : st

{-@ lookup :: k:Variable -> {m:Store | has k m} -> Value @-}
lookup :: Variable -> Store -> Value 
lookup x ((y, v) : st)
  | x == y         = v 
  | otherwise      = lookup x st
lookup x []        = impossible "variable not found"

{-@ measure keys @-}
keys :: Store -> S.Set Variable
keys []        =  S.empty
keys (kv : st) =  S.union (transKey kv) (keys st)

{-@ measure transKey @-}
transKey :: (Variable, Value) -> S.Set Variable
transKey (k,v) = S.singleton k

{-@ inline has @-}
has :: Variable -> Store -> Bool
has k m = S.member k (keys m)
\end{code}

Evaluation
----------

We can now write a function that evaluates `Statement` in a `Store` to yield a
new *updated* `Store`:

\begin{code}
{-@ evalS :: s:Store -> stmt:ScopedStmt s -> Store @-}
evalS :: Store -> Statement -> Store

evalS st Skip             = st

evalS st (Assign x e )    = update st x v
                            where
                              v = evalE st e

evalS st (IfZ e s1 s2)    = if v == 0
                              then evalS st s1
                              else evalS st s2
                            where
                              v = evalE st e
evalS st w@(WhileZ e s)   = if v == 0
                              then evalS st (Sequence s w)
                              else st
                            where
                              v = evalE st e

evalS st (Sequence s1 s2) = evalS (evalS st s1) s2

{-@ type ScopedStmt G = {e: Statement | isScope G e} @-}

{-@ inline wellScopedStmt @-}
wellScopedStmt :: Store -> Statement -> Bool
wellScopedStmt g s = isScope g s

{-@ inline isScope @-}
isScope                  :: Store -> Statement -> Bool
isScope st s          = S.isSubsetOf (readS s) (keys st)
--isScope st (Assign x e)  = wellScoped st e
--isScope st (IfZ e s1 s2) = wellScoped st e && (S.isSubsetOf (readS s1) (keys st)) && ( S.isSubsetOf (readS s2) (keys st))
--isScope st w@(WhileZ e s) = wellScoped st e && readS s == S.empty
--isScope st (Sequence s1 s2) = readS s1 == S.empty && readS s2 == S.empty

\end{code}

The above uses a helper that evaluates an `Expression` in a `Store` to get a
`Value`:

\begin{code}
{-@ evalE :: s:Store -> e:ScopedExpr s -> Value @-}
evalE :: Store -> Expression -> Value
evalE st (Var x)      = lookup x st
evalE _  (Val v)      = v
evalE st (Op o e1 e2) = evalOp o (evalE st e1) (evalE st e2)

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus  i j = i + j
evalOp Minus i j = i - j

{-@ measure free @-}
free                  :: Expression -> S.Set Variable
free (Val _)          = S.empty
free (Var x)          = S.singleton x
free (Op o e1 e2)     = S.union xs1 xs2
      where xs1       = free e1
            xs2       = free e2

{-@ type ScopedExpr G = {e: Expression | wellScoped G e} @-}

{-@ inline wellScoped @-}
wellScoped :: Store -> Expression -> Bool
wellScoped g e = S.isSubsetOf (free e) (keys g)

\end{code}

GOAL: A Safe Evaluator
----------------------

Our goal is to write an evaluator that *never* fails due to an undefined
variable. This means, we must ensure that the evaluator is never called
with *malformed* programs in which some variable is *used-before-being-defined*.

In particular, this corresponds to establishing that the call to impossible
*never* happens at run time, by verifying that the below typechecks:

\begin{code}
{-@ impossible :: {v:String | false} -> a @-}
impossible msg = error msg
\end{code}

Obviously it is possible to throw an exception if you run `evalS` with a
*bad* statement. Your task is to complete the implementation of `isSafe`
and add suitable refinement type specifications, such that you can prove
that the following `interpret` function is safe:

\begin{code}
interpret :: Statement -> Maybe Store
interpret s 
  | isSafe s  = Just (evalS [] s)  -- `s` does not use any vars before definition 
  | otherwise = Nothing            -- `s` may use some var before definition

{-@ inline isSafe @-}
isSafe :: Statement -> Bool
isSafe s = readS s == S.empty
\end{code}

To implement `isSafe` you probably need to write a function that computes the
`Set` of variables that are ``read-before-definition" in a `Statement` (and
`Expression`):

\begin{code}
{-@ measure readS @-}
readS :: Statement -> S.Set Variable
readS (Assign x e)     = readE e
readS (IfZ e s1 s2)    = S.union (readE e) (S.union (readS s1) (readS s2))
readS (WhileZ e s)     = S.union (readE e) (readS s)
readS (Sequence s1 s2) = S.union (readS s1) (S.difference (readS s2) (defineS s1) )
readS Skip             = S.empty

{-@ measure readE @-}
readE :: Expression -> S.Set Variable   
readE (Var x)          = S.singleton x
readE (Val v)          = S.empty 
readE (Op o e1 e2)     = S.union (readE e1) (readE e2)

{-@ measure defineS @-}
defineS :: Statement -> S.Set Variable
defineS (Assign x e)      = S.singleton x
defineS (Sequence s1 s2)  = S.union (defineS s1) (defineS s2)
defineS _                 = S.empty

s0 = Assign "x" (Var "x")
s1 = Assign "x" (Val 10)
s2 = Assign "y" (Val 11)
s3 = Assign "z" (Var "x")
s4 = Assign "z" (Var "y")
s5 = IfZ (Op Plus (Var "x") (Var "y")) (Sequence s3 Skip) s4
s6 = WhileZ (Op Minus (Var "x") (Val 10)) (Sequence s3 (Assign "x" (Val 11)))
s7 = Sequence (Assign "x" (Val 20)) (Assign "y" (Val 21))
s8 = Skip
s9 = Sequence (Sequence s1 s2) s5
s10 = Sequence s1 s6

\end{code}



When you are done, `liquid Interpreter.lhs` should return `SAFE` and also when
you run it in GHCi you should get:

\begin{spec}
ghci> let okStmt = ...
ghci> interpret okStmt 
Just ...

ghci> let badStmt = ... 
ghci> interpret badStmt 
Nothing ...
\end{spec}
