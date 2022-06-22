module FFICXX.Generate.IDL.Translate
  ( TState (..),
    translateFunction,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State (State, gets)
import Data.Map (Map)
import qualified Data.Map as M (lookup)
-- import Data.Text (Text)
import qualified Data.Text as T
import qualified FFICXX.Generate.IDL.Type as I
import qualified FFICXX.Generate.Type.Class as O

data TError = NoIdentifier I.Name

newtype TState = TState
  { tsMap :: Map I.Name O.Types
  }

translateFunction :: I.Function -> ExceptT TError (State TState) O.TopLevel
translateFunction (I.Function out name _args) = do
  mout' <- lift $ gets (M.lookup outTyName . tsMap)
  case mout' of
    Nothing -> throwE (NoIdentifier outTyName)
    Just out' ->
      pure $
        O.TLOrdinary (O.TopLevelFunction out' name' args' Nothing)
  where
    I.Name ntxt = name
    name' = T.unpack ntxt
    I.Typ outTyName = out
    -- wrong stuff for now
    args' = []
