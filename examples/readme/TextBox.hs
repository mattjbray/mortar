module TextBox
  ( Model
  , initModelRequests
  , Action(VtyEvent, Clear)
  , Request
  , update
  , render
  , getContent
  ) where


import qualified Brick            as B
import           Data.List        (intercalate)
import qualified Data.Text.Zipper as Z
import qualified Graphics.Vty     as Vty


data Model = Model
  { zipper :: Z.TextZipper String
  }


initModelRequests :: (Model, [Request])
initModelRequests =
  ( Model (Z.stringZipper [] (Just 1))
  , []
  )


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


update :: Model -> Action -> Maybe (Model, [Request])
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
      Just initModelRequests


applyEdit :: (Z.TextZipper String -> Z.TextZipper String) -> Model -> Maybe (Model, [Request])
applyEdit f model =
  Just ( model { zipper = f (zipper model) }
       , []
       )


render :: Model -> B.Widget
render model =
  let
    content = getContent model
  in
    if null content then
      B.str "enter some text"
    else
      B.str content

getContent :: Model -> String
getContent =
  intercalate "\n" . Z.getText . zipper