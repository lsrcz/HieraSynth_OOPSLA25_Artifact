{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection (..),
    mkSlideUp,
    mkSlideDown,
    slideDirectionParser,
    slide1DirectionParser,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
import Grisette (PPrint (pformat), makePrefixedUnifiedCtor)
import HieraSynth.Util.Parser (CharParser)
import RVV.EvalMode (EvalMode)
import RVV.Synthesizer.Parameter.Common (opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data SlideDirection = SlideUp | SlideDown

deriveFull noModeDeriveConfig [''SlideDirection]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''SlideDirection

instance PPrint SlideDirection where
  pformat SlideUp = "up"
  pformat SlideDown = "down"

{-# INLINE slideDirectionNames #-}
slideDirectionNames :: (IsString s) => [(s, SlideDirection)]
slideDirectionNames =
  [ ("up", SlideUp),
    ("down", SlideDown)
  ]

slideDirectionParser ::
  (CharParser e s m) => s -> m (Identity SlideDirection)
slideDirectionParser = fmap Identity . opCodeParser slideDirectionNames "vslide"

slide1DirectionParser ::
  (CharParser e s m) => s -> m (Identity SlideDirection)
slide1DirectionParser =
  fmap Identity . opCodeParser slideDirectionNames "vslide1"
