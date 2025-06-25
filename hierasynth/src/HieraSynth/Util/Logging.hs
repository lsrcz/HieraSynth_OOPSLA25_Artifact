module HieraSynth.Util.Logging
  ( logMultiLine,
    logMultiLineText,
    logMultiLineDoc,
    logMultiLinePretty,
  )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Grisette (Doc, PPrint (pformat))
import HieraSynth.Util.Pretty (renderDoc)
import System.Console.ANSI
  ( Color (Blue, Green, Red, Yellow),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGRCode,
  )
import System.Log.Logger
  ( Logger,
    Priority (ALERT, CRITICAL, DEBUG, EMERGENCY, ERROR, INFO, NOTICE, WARNING),
    getLevel,
    logL,
  )

priorityToColor :: Priority -> String
priorityToColor prio = case prio of
  DEBUG -> code Blue
  INFO -> code Green
  NOTICE -> code Green
  WARNING -> code Yellow
  ERROR -> code Red
  CRITICAL -> code Red
  ALERT -> code Red
  EMERGENCY -> code Red
  where
    code color = setSGRCode [SetColor Foreground Vivid color]

logLWithColoredMarker :: Logger -> Priority -> String -> String -> IO ()
logLWithColoredMarker logger prio marker msg =
  let color = priorityToColor prio
   in logL logger prio $ color ++ marker ++ setSGRCode [Reset] ++ msg

logMultiLine :: Logger -> Priority -> String -> IO ()
logMultiLine logger prio msg = do
  let level = fromMaybe DEBUG (getLevel logger)
  when (prio >= level) $ do
    let allLines = lines msg
    case length allLines of
      0 -> logLWithColoredMarker logger prio "> " "<empty message>"
      1 -> logLWithColoredMarker logger prio "> " (head allLines)
      _ -> do
        let goRemaining [] = return ()
            goRemaining [lastLine] =
              logLWithColoredMarker logger prio "└ " lastLine
            goRemaining (l : ls) = do
              logLWithColoredMarker logger prio "│ " l
              goRemaining ls
        logLWithColoredMarker logger prio "┌ " (head allLines)
        goRemaining (tail allLines)

logMultiLineText :: Logger -> Priority -> T.Text -> IO ()
logMultiLineText logger prio = logMultiLine logger prio . T.unpack

logMultiLineDoc :: Logger -> Priority -> Doc ann -> IO ()
logMultiLineDoc logger prio = logMultiLineText logger prio . renderDoc 120

logMultiLinePretty :: (PPrint a) => Logger -> Priority -> a -> IO ()
logMultiLinePretty logger prio = logMultiLineDoc logger prio . pformat
