{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HieraSynth.Reasoning.Parallel.Scheduler.LogConfig
  ( LogConfig (..),
    logRootDir,
    getLogConfig,
    defaultFormatter,
    getDefaultLogger,
  )
where

import qualified Data.Text as T
import Data.Time
  ( defaultTimeLocale,
    diffUTCTime,
    formatTime,
    getCurrentTime,
    getZonedTime,
    zonedTimeToUTC,
  )
import HieraSynth.Util.Show (showDiffTime)
import System.Directory.Extra (createDirectoryIfMissing)
import System.IO (stderr)
import System.Log.Formatter (LogFormatter, varFormatter)
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger
  ( Logger,
    Priority (DEBUG, NOTICE),
    getLogger,
    removeHandler,
    rootLoggerName,
    saveGlobalLogger,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )

data LogConfig = LogConfig
  { baseDir :: FilePath,
    progName :: T.Text,
    timeStamp :: String
  }

logRootDir :: LogConfig -> FilePath
logRootDir LogConfig {..} =
  baseDir <> "/" <> T.unpack progName <> "/" <> timeStamp

getLogConfig :: FilePath -> String -> IO LogConfig
getLogConfig baseDir progName = do
  timeStamp <- fmap (\case ' ' -> '-'; c -> c) . show <$> getZonedTime
  let logConfig = LogConfig {baseDir, progName = T.pack progName, timeStamp}
  createDirectoryIfMissing True (logRootDir logConfig)
  return logConfig

defaultFormatter :: forall a. String -> IO (LogFormatter a)
defaultFormatter format = do
  loggerCreatedTime <- getCurrentTime
  return $
    varFormatter
      [ ( "time",
          do
            time <- getZonedTime
            let diffTime =
                  diffUTCTime (zonedTimeToUTC time) loggerCreatedTime
            return $
              formatTime defaultTimeLocale "%F %X" time
                ++ " ("
                ++ showDiffTime diffTime
                ++ ")"
        )
      ]
      format

getDefaultLogger :: LogConfig -> Bool -> IO Logger
getDefaultLogger logConfig@LogConfig {..} enableDebugLogging = do
  logger <- getLogger (T.unpack progName)
  updateGlobalLogger rootLoggerName removeHandler
  defaultFormatter <- defaultFormatter "[$time : $loggername : $prio] $msg"
  let setDefaultFormatter lh = return $ setFormatter lh defaultFormatter
  let rootDir = logRootDir logConfig
  h <- fileHandler (rootDir <> "/notice.log") NOTICE >>= setDefaultFormatter
  hstderr <- streamHandler stderr NOTICE >>= setDefaultFormatter
  handlers <-
    if enableDebugLogging
      then do
        hdebug <- fileHandler (rootDir <> "/debug.log") DEBUG >>= setDefaultFormatter
        return [h, hstderr, hdebug]
      else return [h, hstderr]
  let logger' =
        setHandlers handlers
          . setLevel (if enableDebugLogging then DEBUG else NOTICE)
          . removeHandler
          $ logger
  saveGlobalLogger logger'
  return logger'
