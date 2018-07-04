module Main where

import Prelude

import App.Router as R
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matches)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI R.component R.Index body
  liftEffect $ matches R.route $ \_ new -> do
    launchAff_ $ driver.query $ R.Navigate new unit
