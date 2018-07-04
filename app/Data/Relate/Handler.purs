module App.Data.Relate.Handler where

import Prelude

import App.Data.Relate.Type (Relate(..))
import Control.Monad.State (get)
import Control.Monad.State.Class (class MonadState)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Lynx.Components.Form as Form
import Lynx.Data.Graph (InputRef)

handleRelate :: ∀ v i m
   . MonadState (Form.State v i Relate) m
  => MonadAff m
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
        liftEffect
          $ Console.log $ show refA <> " is NOT equal to " <> show refB
        pure unit

  Clears refB -> do
		pure unit
