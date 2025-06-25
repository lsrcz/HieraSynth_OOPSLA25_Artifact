module HieraSynth.Util.Show (showAsText, showFloat, showDiffTime) where

import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Numeric (showFFloat)

showAsText :: (Show a) => a -> T.Text
showAsText = fromString . show

showFloat :: (RealFloat a) => a -> String
showFloat x = showFFloat (Just 2) x []

showDiffTime :: NominalDiffTime -> String
showDiffTime dt = showFloat (realToFrac dt) <> "s"
