module Gallery.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Gallery.ComponentHATS as Gallery
import Gallery.MasonryDemo as MasonryDemo

foreign import getLocationHash :: Effect String

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  hash <- liftEffect getLocationHash
  case hash of
    "#masonry" -> runUI MasonryDemo.component unit body
    _ -> runUI Gallery.component unit body
