module Propositional where

import           Data.Map.Strict
import           Data.Maybe
import           Prelude         hiding (lookup)

data Exp
    = Var Char
    | Imp Exp
          Exp
    | And Exp
          Exp
    | Or Exp
         Exp
    | Not Exp
    deriving (Show)

connective :: String -> Exp -> Exp -> String
connective op e e' =
    let bb e@(Var c) = pp' e
        bb e = "(" ++ pp' e ++ ")"
    in bb e ++ " " ++ op ++ " " ++ bb e'

pp' :: Exp -> String
pp' (And e e') = connective "\x2227" e e'
pp' (Or e e') = connective  "\x2228" e e'
pp' (Imp e e') = connective "\x2192" e e'
pp' (Not e) =  "\x00AC" ++ pp' e
pp' (Var c) = [c]

pp :: Exp -> IO ()
pp = putStrLn . pp'


classical :: Exp -> (Map Char Bool) -> Bool
classical (Var c) m = fromMaybe err $ lookup c m
  where
    err = error $ "Var " ++ [c] ++ "not set."
classical (And e e') m = classical e m && classical e' m
classical (Or e e') m  = classical e m || classical e' m
classical (Imp e e') m = classical (Or (Not e) e') m
classical (Not e) m    = not $ classical e m

eg = And
     (Not $ Var 'a')
     (Or
      (Var 'b')
       (Imp
        (Var 'z')
        (Var 'c')))
vals = fromList $ [('a', True), ('b', True), ('z', False), ('c', True)]
