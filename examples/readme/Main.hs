{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick                     as B
import           Control.Exception         (bracket)
import qualified Graphics.Vty              as Vty
import qualified Mortar                    as M
import qualified System.Log.Formatter      as Log
import qualified System.Log.Handler        as LogH
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Logger         as Log

import qualified TwoForms


main :: IO ()
main =
  M.start M.Config
    { M.initModel = TwoForms.initModelRequests
    , M.update = TwoForms.update
    , M.liftVtyEvent = TwoForms.VtyEvent
    , M.render = TwoForms.render
    , M.handleRequest = TwoForms.handleRequest
    , M.runHandlerM = runWithLogging
    , M.haltRequest = TwoForms.QuitRequest
    , M.mkAttrMap =
        const $ B.attrMap Vty.defAttr
          [ ("selected" , Vty.white `B.on` Vty.black)
          ]
    }


runWithLogging :: IO () -> IO ()
runWithLogging body =
  bracket initLogger LogH.close $ \h -> do
    Log.updateGlobalLogger
      Log.rootLoggerName
      (Log.setLevel Log.DEBUG . Log.setHandlers [h])

    body
  where
    initLogger =
      Log.fileHandler "debug.log" Log.DEBUG >>= \handler ->
        return $
          LogH.setFormatter
            handler
            (Log.simpleLogFormatter  "[$time : $loggername : $prio] $msg")
