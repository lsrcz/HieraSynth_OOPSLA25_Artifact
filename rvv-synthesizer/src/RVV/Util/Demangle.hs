{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Util.Demangle
  ( demangleSymbol,
    demangleResultToText,
  )
where

import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Demangler (Result, demangle1)
import Prettyprinter
  ( LayoutOptions (LayoutOptions),
    PageWidth (Unbounded),
    layoutPretty,
  )
import Prettyprinter.Render.Text (renderStrict)
import Text.Sayable (Sayable (sayable), Saying (saying))

demangleResultToText :: Result -> T.Text
demangleResultToText =
  renderStrict
    . layoutPretty (LayoutOptions Unbounded)
    . saying
    . sayable @"normal"

demangleSymbol :: BS.ShortByteString -> Result
demangleSymbol = demangle1 . T.decodeUtf8Lenient . BS.fromShort
