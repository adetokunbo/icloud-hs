{- |
Module      : Network.ICloud.Drive
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides functions and/or data types that support Top Sample goals
-}
module Network.ICloud.Drive (
  -- * match a predicate
  endsThen,

) where

import Test.Hspec (shouldSatisfy)


{- | @action \`endsThen\` expected@ sets the expectation that the result of
 @action@ __satisfies__ the predicate @p@.

-}
endsThen :: (Show a) => IO a -> (a -> Bool) -> IO ()
endsThen action p = action >>= (`shouldSatisfy` p)


infix 1 `endsThen`
