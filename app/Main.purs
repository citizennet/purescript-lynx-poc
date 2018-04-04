module Main where

import Prelude

import App.Form as Form
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lynx.Component (component)

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (component Form.userSignup) unit body
