{-# language OverloadedStrings #-}

module Form
  ( Model
  , initModelRequests
  , Action(VtyEvent)
  , update
  , Request
  , handleRequest
  , render
  )
  where


import           Brick               ((<+>), (<=>))
import qualified Brick               as B
import           Control.Applicative ((<|>))
import           Control.Concurrent  (threadDelay)
import qualified Graphics.Vty        as Vty
import qualified System.Log.Logger   as Log

import qualified TextBox


data Model = Model
  { textBox          :: TextBox.Model
  , textBoxActive    :: Bool
  , submittedContent :: Maybe String
  }


initModelRequests :: (Model, [Request])
initModelRequests =
  let
    (tbox, _) =
      TextBox.initModelRequests
  in
    ( Model
        { textBox = tbox
        , textBoxActive = True
        , submittedContent = Nothing
        }
    , []
    )


data Action
  = VtyEvent Vty.Event
  | TextBoxAction TextBox.Action
  | ToggleTextBoxActive
  | Submit
  | NoAction


data Request
  = DelayToggleTextBoxActive
  | LogSubmitted String
  deriving Eq


vtyEventToAction :: Vty.Event -> Maybe Action
vtyEventToAction event =
  case event of
    Vty.EvKey Vty.KEnter [] ->
      Just Submit

    _ ->
      Nothing


update :: Model -> Action -> Maybe (Model, [Request])
update model action =
  case action of
    NoAction ->
      Nothing

    VtyEvent event ->
      handleTopLevelVtyEvent model event
      <|>
      maybeDeferToTextBox model (TextBox.VtyEvent event)

    TextBoxAction textBoxAction ->
      maybeDeferToTextBox model textBoxAction

    ToggleTextBoxActive ->
      Just ( model { textBoxActive = not (textBoxActive model) }
           , []
           )

    Submit ->
      let
        content = TextBox.getContent (textBox model)
        hasContent = not (null content)
        -- submitSelected = not (textBoxActive model)
      in
        if hasContent then do
          (model', requests) <- deferToTextBox model TextBox.Clear
          Just ( model'
                   { submittedContent = Just content
                   , textBoxActive = False
                   }
               , LogSubmitted content : DelayToggleTextBoxActive : requests
               )
        else
          Nothing


handleTopLevelVtyEvent :: Model -> Vty.Event -> Maybe (Model, [Request])
handleTopLevelVtyEvent model event = do
  action <- vtyEventToAction event
  update model action


maybeDeferToTextBox :: Model -> TextBox.Action -> Maybe (Model, [Request])
maybeDeferToTextBox model textBoxAction =
  if textBoxActive model then
    deferToTextBox model textBoxAction
  else
    Nothing


deferToTextBox :: Model -> TextBox.Action -> Maybe (Model, [Request])
deferToTextBox model textBoxAction = do
  (textBoxModel, _) <-
    TextBox.update (textBox model) textBoxAction
  Just ( model { textBox = textBoxModel }
       , []
       )


render :: Model -> B.Widget
render model =
  let
    (mkButtonFocus, mkTextBoxFocus) =
      if textBoxActive model then
        (id, B.withAttr "selected")
      else
        (B.withAttr "selected", id)
  in
    (mkTextBoxFocus . B.hLimit 40 . B.padRight (B.Pad 40) . TextBox.render . textBox) model
    <+>
    (mkButtonFocus . B.padLeftRight 3 . B.str) "submit"
    <=>
    (B.str . unlines)
      [ ""
      , maybe "Nothing submitted yet" ((++) "You submitted: " . show) (submittedContent model)
      , ""
      , "Press <ENTER> to submit."
      ]


handleRequest :: String -> Request -> IO Action
handleRequest thisName request =
  case request of
    DelayToggleTextBoxActive -> do
      threadDelay (2 * 10 ^ (5 :: Int))
      return ToggleTextBoxActive

    LogSubmitted content -> do
      Log.debugM ("MyApp." ++ thisName) ("User submitted: " ++ content)
      return NoAction
