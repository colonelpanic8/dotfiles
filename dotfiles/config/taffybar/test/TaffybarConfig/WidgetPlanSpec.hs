module TaffybarConfig.WidgetPlanSpec (spec) where

import TaffybarConfig.RuntimeCapabilities
import TaffybarConfig.WidgetPlan
import Test.Hspec

spec :: Spec
spec = describe "endWidgetPlanForCapabilities" $ do
  it "uses the desktop layout without a battery" $
    endWidgetPlanForCapabilities (RuntimeCapabilities False False False)
      `shouldBe` EndWidgetPlan False False False
  it "selects battery, backlight, and CPU power independently" $
    map
      endWidgetPlanForCapabilities
      [ RuntimeCapabilities True False False,
        RuntimeCapabilities False True False,
        RuntimeCapabilities False False True
      ]
      `shouldBe` [ EndWidgetPlan True False False,
                   EndWidgetPlan False True False,
                   EndWidgetPlan False False True
                 ]
