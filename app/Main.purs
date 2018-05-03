module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App.Router as R

main :: ∀ eff. Eff (HA.HalogenEffects (R.Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI R.component R.Index body
