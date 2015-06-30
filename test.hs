import Control.Monad.Error
import Control.Monad.Identity

data Exception
    = Failure String | GenericFailure
    deriving Show

instance Error Exception where
    noMsg = GenericFailure

type ErrMonad a = ErrorT Exception Identity a

example :: Int -> Int -> ErrMonad Int
example x y = do
  case y of
    0 -> throwError $ Failure "division by zero"
    x -> return $ x `div` y

runFail :: ErrMonad a -> Either Exception a
runFail = runIdentity . runErrorT
