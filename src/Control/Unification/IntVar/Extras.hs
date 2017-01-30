{-# language ScopedTypeVariables #-}
module Control.Unification.IntVar.Extras (liftCatch) where

import Control.Unification.IntVar
import Control.Monad.Signatures (Catch)
import Control.Monad.Trans (lift)
import qualified Control.Monad.State.Class as St

runWithState :: St.MonadState s m => s -> m a -> m a
runWithState s m = St.put s >> m

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: forall e tm m a . Monad m => Catch e m (a, IntBindingState tm) -> Catch e (IntBindingT tm m) a
liftCatch catch comp handler = do
  initial <- St.get
  (ans, final) <- lift $ guardedComp initial comp
  St.put final
  return ans
  where
    guardedComp :: Monad m => IntBindingState tm -> IntBindingT tm m a -> m (a, IntBindingState tm)
    guardedComp s m = runIntBindingT (runWithState s m) `catch` (runIntBindingT . runWithState s . handler)
    
  
