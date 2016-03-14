{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Mortar
  ( Config(..)
  , start
  ) where

import qualified Brick.AttrMap          as A
import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Control.Concurrent     (Chan, forkIO, newChan, readChan,
                                         writeChan, writeList2Chan)
import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Default           (def)
import qualified Graphics.Vty           as Vty

{-| The @Config@ data type contains everything you need to run your app.

The @model@ type contains the entire state of your application.

The @update@ function should take the current @model@ and an @action@ to
process, and returns the updated @model@ along with any @request@s.

The @action@ type represents user input (by wrapping Vty Events) as well as
responses from the request handler. @action@s are the only way to change the
state of the model (via the @udpate@ function).

The @request@ type represents some computation that should happen off the main
application thread. @request@ handlers may perform IO. When the @request@
handler completes, an @action@ is returned, which is fed back to the @update@
function.

@handlerM@ is the monad in which the request handler is run. This is useful, for
example, for passing around a database connection in the Reader monad.
-}
data Config model action request handlerM = Config
  { -- | The initial @model@ and any @request@s to kick off processing.
    initModel     :: (model, [request])
    -- | The @update@ function takes the current @model@ and an @action@ to
    -- respond to, and returns the udpated @model@ along with any @request@s to
    -- be processed.  If the @action@ is not handled, returns @Nothing@.
  , update        :: model -> action -> Maybe (model, [request])
    -- | Lift a @Vty.Event@ to your app's @action@ type.
  , liftVtyEvent  :: Vty.Event -> action
    -- | Take the @model@ and render a @Widget@.
  , render        :: model -> T.Widget
    -- | Handle a @request@ and return an @action@ in your app's monad @handlerM@.
  , handleRequest :: request -> handlerM action
    -- | Do any setup (e.g. connecting to a database) and run your @handlerM@.
  , runHandlerM   :: handlerM () -> IO ()
    -- | When your @update@ function returns this @request@, the app will exit.
  , haltRequest   :: request
  , mkAttrMap     :: model -> A.AttrMap
  }

{-| Start a Mortar application with a @Config@.
-}
start ::
  forall model action request handlerM.
  ( Eq request , MonadIO handlerM)
  => Config model action request handlerM
  -> IO ()
start Config{..} = do
  -- Requests received from the app.
  requestChan <- newChan

  -- Actions to be sent into the app.
  actionChan <- newChan

  -- Listen for requests and generate actions.
  _ <- forkIO $ runHandlerM $ forever $ do
    request <- liftIO $ readChan requestChan
    action <- handleRequest request
    liftIO $ writeChan actionChan action

  let (model, requests) = initModel

  writeList2Chan requestChan requests

  void $
    M.customMain
      (Vty.mkVty def)
      actionChan
      (mkApp requestChan)
      model

  where
    mkApp
      :: Chan request
      -> M.App model action
    mkApp requestChan =
      M.App
        { M.appDraw = return . render
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appHandleEvent requestChan
        , M.appStartEvent = return
        , M.appAttrMap = mkAttrMap
        , M.appLiftVtyEvent = liftVtyEvent
        }

    appHandleEvent
      :: Chan request
      -> model
      -> action
      -> T.EventM (T.Next model)
    appHandleEvent requestChan model action =
      case update model action of
        Nothing ->
          M.continue model
        Just (model', requests)
          | requests == [haltRequest] ->
              M.halt model'
          | null requests ->
              M.continue model'
          | otherwise ->
              M.suspendAndResume $ do
                writeList2Chan requestChan requests
                return model'
