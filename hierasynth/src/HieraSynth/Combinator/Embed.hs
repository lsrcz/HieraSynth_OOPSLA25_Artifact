{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Combinator.Embed ((:<:) (..)) where

import Grisette (Mergeable, Union, mrgReturn, pattern Single)

class sub :<: sup where
  inj :: sub -> sup
  prj :: sup -> Maybe sub

instance {-# OVERLAPPING #-} a :<: a where
  inj = id
  prj = Just

instance (a :<: b, Mergeable b) => (a :<: Union b) where
  inj = mrgReturn . inj
  prj r = case r of
    Single a -> prj a
    _ -> Nothing
