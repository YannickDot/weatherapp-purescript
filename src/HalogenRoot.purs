module HalogenRoot where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HalogenComponent as C
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  _ <- HA.awaitLoad
  node <- HA.selectElement $ QuerySelector "#halogen"
  case node of
    Just n -> runUI C.myButton unit n
    Nothing -> runUI C.myButton unit body
