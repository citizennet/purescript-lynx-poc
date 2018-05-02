module Main where

import Prelude

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import FormBuilder as FormBuilder

type Effects eff =
  ( ajax :: AJAX
  , console :: CONSOLE
  | eff
  )

main :: ∀ eff. Eff (HA.HalogenEffects (Effects eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI FormBuilder.formBuilder unit body
