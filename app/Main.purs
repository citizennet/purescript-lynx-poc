module Main where

import Prelude

import App.Forms.Signup as Signup
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lynx.Component as Component

main :: ∀ eff. Eff (HA.HalogenEffects (Component.Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (Component.component Signup.handleValidation Signup.renderInput Signup.handleRelation) unit body
