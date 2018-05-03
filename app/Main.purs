module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lynx.Components.Form as Form
import App.Forms.Signup as Signup

main :: ∀ eff. Eff (HA.HalogenEffects (Component.Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (Form.component Signup.handleValidation Signup.renderInput Signup.handleRelation) unit body
