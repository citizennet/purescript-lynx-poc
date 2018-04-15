module Main where

import Prelude

import App.Forms.Dynamic.Signup as Signup
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lynx.Dynamic.Component as Component
import Lynx.Dynamic.GraphRecord as GraphRecord

main :: ∀ eff. Eff (HA.HalogenEffects (Component.Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let x = GraphRecord.built
  runUI (Component.component Signup.handleValidation Signup.renderInput Signup.handleRelation) unit body
