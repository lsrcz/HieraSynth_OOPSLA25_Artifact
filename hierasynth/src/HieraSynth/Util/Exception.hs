{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HieraSynth.Util.Exception
  ( CancellingException (..),
    catchErrno,
  )
where

import Control.Exception (Exception, catch)
import Foreign.C (Errno, getErrno)

data CancellingException where
  CancellingException :: (Exception e) => e -> CancellingException

instance Show CancellingException where
  show (CancellingException e) = show e

instance Exception CancellingException

catchErrno :: IO a -> (IOError -> Errno -> IO a) -> IO a
catchErrno action handler =
  action `catch` \(e :: IOError) ->
    getErrno >>= handler e
