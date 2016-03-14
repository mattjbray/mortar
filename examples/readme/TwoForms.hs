module TwoForms
  ( Model
  , initModelRequests
  , Action(VtyEvent)
  , update
  , Request(QuitRequest)
  , handleRequest
  , render
  ) where

import           Brick                ((<+>), (<=>))
import qualified Brick                as B
import qualified Brick.Widgets.Center as B
import           Control.Applicative  ((<|>))
import           Data.Maybe           (fromMaybe)
import qualified Graphics.Vty         as Vty

import qualified Form


data Model = Model
  { forms     :: (Form.Model, Form.Model)
  , selection :: Maybe Selection
  }


data Selection
  = LeftForm
  | RightForm
  deriving Show


initModelRequests :: (Model, [Request])
initModelRequests =
  let
    (form1Model, form1Reqs) =
      Form.initModelRequests True
    (form2Model, form2Reqs) =
      Form.initModelRequests False
  in
    ( Model
        { forms =
            (form1Model, form2Model)
        , selection =
            Just LeftForm
        }
    , (Form1Request <$> form1Reqs) ++ (Form2Request <$> form2Reqs)
    )


data Action
  = VtyEvent Vty.Event
  | QuitAction
  | NoAction
  | ToggleActiveForm
  | Form1Action Form.Action
  | Form2Action Form.Action


data Request
  = QuitRequest
  | Form1Request Form.Request
  | Form2Request Form.Request
  deriving Eq


update :: Model -> Action -> Maybe (Model, [Request])
update model action =
  case action of
    NoAction ->
      Nothing

    QuitAction ->
      Just (model, [QuitRequest])

    VtyEvent event ->
      handleTopLevelVtyEvent model event
      <|>
      deferToForms model (Form.VtyEvent event)

    ToggleActiveForm ->
      let
        (newSelection, form1Active, form2Active) =
          case selection model of
            Nothing ->
              (Just LeftForm, True, False)
            Just LeftForm ->
              (Just RightForm, False, True)
            Just RightForm ->
              (Nothing, False, False)
        (model', requests1) =
          fromMaybe (model, []) $
            deferToForm (Just LeftForm) model (Form.SetFocus form1Active)
        (model'', requests2) =
          fromMaybe (model', []) $
            deferToForm (Just RightForm) model' (Form.SetFocus form2Active)
      in
        Just ( model'' { selection = newSelection }
             , requests1 ++ requests2
             )

    Form1Action formAction ->
      deferToForm (Just LeftForm) model formAction

    Form2Action formAction ->
      deferToForm (Just RightForm) model formAction


handleTopLevelVtyEvent :: Model -> Vty.Event -> Maybe (Model, [Request])
handleTopLevelVtyEvent model event = do
  action <- vtyEventToAction event
  update model action


vtyEventToAction :: Vty.Event -> Maybe Action
vtyEventToAction event =
  case event of
    Vty.EvKey Vty.KEsc [] ->
      Just QuitAction

    Vty.EvKey (Vty.KChar '\t') [] ->
      Just ToggleActiveForm

    _ ->
      Nothing


deferToForms :: Model -> Form.Action -> Maybe (Model, [Request])
deferToForms model =
  deferToForm (selection model) model


deferToForm :: Maybe Selection -> Model -> Form.Action -> Maybe (Model, [Request])
deferToForm mSelected model formAction =
  case mSelected of
    Nothing ->
      Nothing

    Just selected -> do
      (formModel, formRequests) <-
        Form.update (getForm model selected) formAction
      Just ( setForm model selected formModel
           , case selected of
               LeftForm ->
                 Form1Request
               RightForm ->
                 Form2Request
             <$> formRequests
           )


getForm :: Model -> Selection -> Form.Model
getForm model selected =
  let
    formGetter =
      case selected of
        LeftForm ->
          fst
        RightForm ->
          snd
  in
    formGetter (forms model)


setForm :: Model -> Selection -> Form.Model -> Model
setForm model selected newForm =
  model
    { forms =
        case selected of
          LeftForm ->
            (newForm, snd (forms model))
          RightForm ->
            (fst (forms model), newForm)
    }


handleRequest :: Request -> IO Action
handleRequest request =
  case request of
    QuitRequest ->
      return NoAction

    Form1Request formRequest ->
      Form1Action <$> Form.handleRequest "Form1" formRequest

    Form2Request formRequest ->
      Form2Action <$> Form.handleRequest "Form2" formRequest


render :: Model -> B.Widget
render model =
  B.hCenter (Form.render (fst (forms model)))
  <+>
  B.hCenter (Form.render (snd (forms model)))
  <=>
  (B.padTop B.Max . B.hCenter . B.str . unlines)
    [ ""
    , "Currently selected: " ++ show (selection model)
    , ""
    , "Press <TAB> to toggle between forms."
    , "Press <ESC> to quit."
    ]
