module Gallery.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Gallery.LandingPage as LandingPage
import Gallery.HierarchyPage as HierarchyPage
import Gallery.FlowPage as FlowPage
import Gallery.PatternPage as PatternPage

foreign import getLocationHash :: Effect String
foreign import onHashChange :: Effect Unit -> Effect Unit

main :: Effect Unit
main = do
  onHashChange (reloadPage unit)
  HA.runHalogenAff do
    body <- HA.awaitBody
    hash <- liftEffect getLocationHash
    case hash of
      "#hierarchy" -> runUI HierarchyPage.component unit body
      "#flow" -> runUI FlowPage.component unit body
      "#pattern" -> runUI PatternPage.component unit body
      _ -> runUI LandingPage.component unit body

foreign import reloadPage :: Unit -> Effect Unit
