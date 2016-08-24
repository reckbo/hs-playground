import qualified Data.Map   as Map
import           Data.Maybe
import Control.Monad.Identity

type Name = String -- variable names
data Exp = Lit Integer -- expressions
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value = IntVal Integer -- values
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value


lift :: Monad m => Maybe a -> m a
lift Nothing  = fail "you die now"
lift (Just a) = return a


type Eval1 α = Identity α
runEval1 :: Eval1 α -> α
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = lift $ Map.lookup n env
eval1 env (Plus e1 e2 ) = do IntVal i1 <- eval1 env e1
                             IntVal i2 <- eval1 env e2
                             return $ IntVal (i1 + i2 )
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2 ) = do val1 <- eval1 env e1
                            val2 <- eval1 env e2
                            case val1 of
                              FunVal env0 n body -> eval1 (Map.insert n val2 env0) body

ex :: Exp
ex = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

main :: IO ()
main = do
  print $ runEval1 (eval1 Map.empty ex)
