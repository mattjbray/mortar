{-# LANGUAGE OverloadedStrings #-}

module Form
  ( Model
  , initModelRequests
  , Action(VtyEvent, SetFocus)
  , update
  , Request
  , handleRequest
  , render
  )
  where


import           Brick                ((<+>), (<=>))
import qualified Brick                as B
import qualified Brick.Widgets.Border as B
import           Control.Applicative  ((<|>))
import           Control.Concurrent   (threadDelay)
import qualified Graphics.Vty         as Vty
import qualified System.Log.Logger    as Log

import qualified TextBox


data Model = Model
  { textBox          :: TextBox.Model
  , textBoxActive    :: Bool
  , submittedContent :: Maybe String
  , hasFocus         :: Bool
  }


initModelRequests :: Bool -> (Model, [Request])
initModelRequests focus =
  ( Model
      { textBox = TextBox.initModel
      , textBoxActive = True
      , submittedContent = Nothing
      , hasFocus = focus
      }
  , []
  )


data Action
  = VtyEvent Vty.Event
  | TextBoxAction TextBox.Action
  | ToggleTextBoxActive
  | Submit
  | SetFocus Bool
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

    SetFocus focus ->
      Just ( model { hasFocus = focus }
           , []
           )


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
  textBoxModel <-
    TextBox.update (textBox model) textBoxAction
  Just ( model { textBox = textBoxModel }
       , []
       )


render :: Model -> B.Widget
render model =
  let
    (mkButtonFocus, mkTextBoxFocus)
      | not (hasFocus model) =
          (id, id)
      | textBoxActive model =
          (id, B.withAttr "selected")
      | otherwise =
          (B.withAttr "selected", id)
  in
    ( mkTextBoxFocus
      . B.border
      . B.hLimit 40
      . B.padRight (B.Pad 40)
      . TextBox.render
      . textBox ) model
    <+>
    ( mkButtonFocus
      . B.border
      . B.padLeftRight 3
      . B.str ) "Submit"
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
