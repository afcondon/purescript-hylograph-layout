module Gallery.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- Switch between old and HATS versions:
-- import Gallery.Component as Gallery
import Gallery.ComponentHATS as Gallery

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Gallery.component unit body
