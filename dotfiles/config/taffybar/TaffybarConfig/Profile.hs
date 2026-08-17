{-# LANGUAGE OverloadedStrings #-}

module TaffybarConfig.Profile
  ( BarProfile (..),
    barProfileClass,
    barProfileHeightRatio,
    barProfilePadding,
    profileForLogicalWidth,
  )
where

import Data.Text (Text)

data BarProfile
  = NormalProfile
  | CompactProfile
  | SmallProfile
  | TinyProfile
  deriving (Eq, Show)

profileForLogicalWidth :: Int -> BarProfile
profileForLogicalWidth width
  | width >= 3840 = NormalProfile
  | width >= 3000 = CompactProfile
  | width >= 2000 = SmallProfile
  | otherwise = TinyProfile

barProfileClass :: BarProfile -> Text
barProfileClass NormalProfile = "bar-profile-normal"
barProfileClass CompactProfile = "bar-profile-compact"
barProfileClass SmallProfile = "bar-profile-small"
barProfileClass TinyProfile = "bar-profile-tiny"

barProfilePadding :: BarProfile -> Int
barProfilePadding NormalProfile = 4
barProfilePadding CompactProfile = 2
barProfilePadding SmallProfile = 1
barProfilePadding TinyProfile = 0

barProfileHeightRatio :: BarProfile -> Rational
barProfileHeightRatio NormalProfile = 2 / 99
barProfileHeightRatio CompactProfile = 1 / 60
barProfileHeightRatio SmallProfile = 1 / 72
barProfileHeightRatio TinyProfile = 1 / 90
