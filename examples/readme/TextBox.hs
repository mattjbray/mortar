module TextBox
  ( Model
  , initModel
  , Action(VtyEvent, Clear)
  , update
  , render
  , getContent
  ) where


import qualified Brick                as B
import           Data.List            (intercalate)
import qualified Data.Text.Zipper     as Z
import qualified Graphics.Vty         as Vty


data Model = Model
  { zipper :: Z.TextZipper String
  }


initModel :: Model
initModel =
  Model
    { zipper = Z.stringZipper [] (Just 1)
    }


data Action
  = VtyEvent Vty.Event
  | Input Char
  | Backspace
  | Delete
  | MoveLeft
  | MoveRight
  | Clear


vtyEventToAction :: Vty.Event -> Maybe Action
vtyEventToAction event =
  case event of
    Vty.EvKey (Vty.KChar c) [] ->
      Just (Input c)

    Vty.EvKey Vty.KLeft [] ->
      Just MoveLeft

    Vty.EvKey Vty.KRight [] ->
      Just MoveRight

    Vty.EvKey Vty.KBS [] ->
      Just Backspace

    Vty.EvKey Vty.KDel [] ->
      Just Delete

    _ ->
      Nothing


data Request
  = NoRequest
  deriving Eq


update :: Model -> Action -> Maybe Model
update model action =
  case action of
    VtyEvent event -> do
      action' <- vtyEventToAction event
      update model action'

    Input char ->
      applyEdit (Z.insertChar char) model

    Backspace ->
      applyEdit Z.deletePrevChar model

    Delete ->
      applyEdit Z.deleteChar model

    MoveLeft ->
      applyEdit Z.moveLeft model

    MoveRight ->
      applyEdit Z.moveRight model

    Clear ->
      Just initModel


applyEdit :: (Z.TextZipper String -> Z.TextZipper String) -> Model -> Maybe Model
applyEdit f model =
  Just model { zipper = f (zipper model) }


render :: Model -> B.Widget
render model =
  let
    content = getContent model
  in
    B.str $
      if null content then
        "enter some text"
      else
        content


getContent :: Model -> String
getContent =
  intercalate "\n" . Z.getText . zipper
