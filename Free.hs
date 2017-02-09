{-# LANGUAGE RecordWildCards #-}

module Free where

import           Data.Maybe
import           Prelude         hiding (not, and, or)
import           qualified Prelude  (not)

newtype Fix f = Fx (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

data PropositionalF a
    = Var Char
    | Imp a a
    | And a a
    | Or a a
    | Not a

instance Functor PropositionalF where
  fmap f (Var c)    = Var c
  fmap f (Imp e e') = Imp (f e) (f e')
  fmap f (And e e') = And (f e) (f e')
  fmap f (Or e e')  = Or (f e) (f e')
  fmap f (Not e)    = Not (f e)

type Algebra f a = f a -> a
type PropositionalAlgebra a = Algebra PropositionalF a
type Expression = Fix PropositionalF

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- Convenience functions for writing expressions

not      = Fx . Not
or e e'  = Fx $ Or e e'
and e e' = Fx $ And e e'
imp e e' = Fx $ Imp e e'
var      = Fx . Var

type AssignmentMap a = [(Char, a)]
data Ops a = Ops
    { _and :: a -> a -> a
    , _or :: a -> a -> a
    , _imp :: a -> a -> a
    , _not :: a -> a
    }

mkAlgebra :: Ops a -> AssignmentMap a -> PropositionalAlgebra a
mkAlgebra Ops{..} as = alg
  where alg (Var b)     = b' where Just b' = lookup b as
        alg (And b b')  = _and b b'
        alg (Or b b')   = _or b b'
        alg (Imp b b')  = _imp b b'
        alg (Not b)     = _not b


interpret :: Ops a -> AssignmentMap a -> Expression -> a
interpret ops as = cata (mkAlgebra ops as)


-- Pretty printer (a string algebra)

printerOps :: Ops String
printerOps =
  Ops {_and = connective "\x2227"
      ,_or  = connective "\x2228"
      ,_imp = connective "\x2192"
      ,_not = ("\x00AC" ++)}
  where connective op e e' = "(" ++ e ++ " " ++ op ++ " " ++ e' ++ ")"

-- Classical two-valued logic

classicalOps :: Ops Bool
classicalOps =
  Ops {_and = (&&)
      ,_or  = (||)
      ,_imp = \e e' -> Prelude.not e || e'
      ,_not = Prelude.not}


-- Test

example_expressions :: [Expression]
example_expressions =
    [ (not $ var 'c') `or` not (var 'd' `and` var 'a')
    , var 'c' `and` (not $ var 'd')
    , var 'c' `imp` (not $ var 'd')]



test :: IO ()
test =
  do let vars = ['a' .. 'd']
         symbolMap = map (\c -> (c,[c])) vars
         toStr = map (fmap show)
         print = putStr . unlines

     print $ map (interpret printerOps symbolMap) example_expressions

     putStrLn "# Classical interpretation:"

     let boolMap = zip vars [True,True,False,True]
     print $ map (interpret printerOps (toStr boolMap)) example_expressions
     print $ map (show . interpret classicalOps boolMap) example_expressions
