#!/usr/bin/env run_haskell_stack.sh

import Data.Bifunctor
import Data.Tuple

knightDeltasSimple =
  applyAll [swap] (1, 2)
  >>= applyAll [first negate, second negate, bimapBoth negate]
  where applyAll fns v = map ($ v) $ id:fns
        bimapBoth fn = bimap fn fn

knightDeltas = applyInStages [(1, 2)]
               [ [swap]
               , [bimapBoth (* 2)]
               , [first negate, second negate, bimapBoth negate]
               ]
  where applyAll fns v = map ($ v) $ id:fns
        applyAllToValues values fns = values >>= applyAll fns
        applyInStages = foldl applyAllToValues
        bimapBoth fn = bimap fn fn

main = print knightDeltas
