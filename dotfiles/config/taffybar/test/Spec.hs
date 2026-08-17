module Main (main) where

import qualified TaffybarConfig.ProfileSpec
import qualified TaffybarConfig.WidgetPlanSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    TaffybarConfig.ProfileSpec.spec
    TaffybarConfig.WidgetPlanSpec.spec
