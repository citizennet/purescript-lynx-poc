module App.Data.Relate.Handler where

import Prelude

import App.Data.Relate.Type (Relate(..))
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.State (get)
import Control.Monad.State.Class (class MonadState)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Lynx.Components.Form as Form
import Lynx.Data.Graph (InputRef)

handleRelate :: ∀ v i eff m
   . MonadState (Form.State v i Relate) m
  => MonadAff (Form.Effects eff) m
  => Eq i
  => Relate
  -> InputRef
  -> m Unit
handleRelate relation refA = case relation of
  Equals refB -> do
    st <- get
    let equal = do
          c0 <- Map.lookup refA st.form
          c1 <- Map.lookup refB st.form
          pure $ c0 == c1
    case equal of
      Just true -> pure unit
      otherwise -> do
        liftAff $ Console.log $ show refA <> " is NOT equal to " <> show refB
        pure unit

  Clears refB -> do
		pure unit
