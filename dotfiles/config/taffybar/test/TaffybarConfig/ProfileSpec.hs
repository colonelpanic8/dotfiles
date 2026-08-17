module TaffybarConfig.ProfileSpec (spec) where

import TaffybarConfig.Profile
import Test.Hspec

spec :: Spec
spec = describe "profileForLogicalWidth" $ do
  it "selects normal for very wide logical displays" $
    profileForLogicalWidth 3840 `shouldBe` NormalProfile
  it "selects compact for ultrawide logical displays" $ do
    profileForLogicalWidth 3839 `shouldBe` CompactProfile
    profileForLogicalWidth 3000 `shouldBe` CompactProfile
  it "selects small for medium logical displays" $ do
    profileForLogicalWidth 2999 `shouldBe` SmallProfile
    profileForLogicalWidth 2000 `shouldBe` SmallProfile
  it "selects tiny for constrained logical displays" $
    profileForLogicalWidth 1999 `shouldBe` TinyProfile
