module LambdaCalculus where

data Exp
    = App Exp
          Exp
    | Lambda Char
             Exp
    | Var Char
    deriving ((Show))

pp' :: Exp -> String
pp' (App (Var c) (Var c')) = [c, c']
pp' (App (Var c) e) = [c] ++ "(" ++ pp' e ++ ")"
pp' (App y y') = "(" ++ (pp' y) ++ ")" ++ "(" ++ (pp' y') ++ ")"
pp' (Lambda var exp) = "\x03BB" ++ [var] ++ "(" ++ pp' exp ++ ")"
pp' (Var c) = [c]

pp :: Exp -> IO ()
pp = putStrLn . pp'


eg =
    Lambda 'f' $
    App
        (Lambda 'u' (App (Var 'f') (App (Var 'u') (Var 'u'))))
        (Lambda 'v' (App (Var 'f') (App (Var 'v') (Var 'v'))))

-- data Exp where
--   Var :: Char -> Exp
--   App :: Exp -> Exp -> Exp
--   Lambda ::
