{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.Util.Parser
  ( parseOrDie,
    parseFullInputOrDie,
  )
where

import qualified Data.Text as T
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, parse)

parseOrDie :: (HasCallStack) => Parsec Void T.Text a -> T.Text -> a
parseOrDie parser input = case parse parser "" input of
  Left bundle ->
    error $ "Failed to parse input" <> T.unpack input <> "\n" <> errorBundlePretty bundle
  Right result -> result

parseFullInputOrDie :: (HasCallStack) => Parsec Void T.Text a -> T.Text -> a
parseFullInputOrDie parser = parseOrDie (parser <* eof)
