{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick        as B
import qualified Graphics.Vty as Vty
import qualified Mortar       as M

import qualified TwoForms


main :: IO ()
main =
  M.start M.Config
    { M.initModel = TwoForms.initModelRequests
    , M.update = TwoForms.update
    , M.liftVtyEvent = TwoForms.VtyEvent
    , M.render = TwoForms.render
    , M.handleRequest = TwoForms.handleRequest
    , M.runHandlerM = id
    , M.haltRequest = TwoForms.QuitRequest
    , M.mkAttrMap =
        const $ B.attrMap Vty.defAttr
          [ ("selected" , Vty.white `B.on` Vty.black)
          ]
    }
