module Gen where

import Hedgehog (Gen)
import Hedgehog.Gen (choice, enum)
-- import qualified Hedgehog.Gen as Gen
-- import Hedgehog.Range (Range)
-- import qualified Hedgehog.Range as Range
import Relude

sourceCharacter :: Gen Char
sourceCharacter =
  choice
    [ pure '\x0009',
      pure '\x000A',
      pure '\x000D',
      enum '\x0020' '\xFFFF'
    ]
