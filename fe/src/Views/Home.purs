module SimpleLists.Views.Home where

import Prelude

import Data.Array (filter, reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Halogen (ClassName(..), liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as B
import SimpleLists.Data.Todo (Todo, TodoEntry)
import SimpleLists.Repository.Todo (createTodoEntry, deleteTodoList, getTodoList, updateTodoList)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

type ComponentState = { todoList :: Either String Todo, isLoading :: Boolean, editing :: Maybe Int, editingText :: String, newEntryText :: String }

data ComponentAction
  = GetTodoList
  | ToggleIsDone TodoEntry
  | DeleteTodoListEntry Int
  | EditTodoListEntry Int
  | SetEditingListEntryText String
  | UpdateTodoListEntryText TodoEntry
  | SetNewEntryText String
  | SubmitNewEntry
  | SubmitOnEnter KeyboardEvent
  | UpdateOnEnter TodoEntry KeyboardEvent

homeView :: forall q i o m. MonadAff m => H.Component q i o m
homeView =
  H.mkComponent
    { initialState: \_ -> { todoList: Left "", isLoading: true, editing: Nothing, editingText: "", newEntryText: "" }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just GetTodoList }
    }

render :: forall cs m. ComponentState -> H.ComponentHTML ComponentAction cs m
render state =
  HH.section [ HP.classes [ B.vh100 ], HP.style "background-color: #3da2c3;" ]
    [ HH.div [ HP.classes [ B.container ] ]
        [ HH.h1
            [ HP.classes [ B.textCenter ] ]
            [ HH.text $ "Simple lists" ]
        , if state.isLoading then
            HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Loading..." ]
          else case state.todoList of
            Left s -> HH.h2 [ HP.classes [ B.textCenter, B.bgDanger ] ] [ HH.text $ "Error: " <> s ]
            Right todo ->
              let
                notCompletedEntries = reverse $ filter (\t -> not t.isDone) todo.entries

                completedEntries = reverse $ filter (\t -> t.isDone) todo.entries
              in
                HH.div_
                  [ HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "New item:" ]
                  , createItem state.newEntryText
                  , HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Todo:" ]
                  , HH.ul [ HP.classes [ B.listGroup, B.rounded0 ] ] (map renderItem notCompletedEntries)
                  , HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Completed:" ]
                  , HH.ul [ HP.classes [ B.listGroup, B.rounded0 ] ] (map renderItem completedEntries)
                  ]
        ]
    ]
  where
  createItem t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t, HE.onValueInput SetNewEntryText, HE.onKeyDown SubmitOnEnter ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnSuccess ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> SubmitNewEntry
              ]
              [ HH.text "💾" ]
          ]
      ]

  -- TODO try to split it to a separate list
  renderCompletedItem t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnWarning ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> ToggleIsDone t
              ]
              [ HH.text "↺" ]
          ]
      , HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t.text, HP.disabled true ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnDanger ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> DeleteTodoListEntry t.id
              ]
              [ HH.text "x" ]
          ]
      ]

  renderNotCompletedItem t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnSuccess ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> ToggleIsDone t
              ]
              [ HH.text "✔" ]
          ]
      , HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t.text, HP.disabled true ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnPrimary ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> EditTodoListEntry t.id
              ]
              [ HH.text "✎" ]
          ]
      ]

  renderEditingItem t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t.text, HE.onValueInput SetEditingListEntryText, HE.onKeyDown (UpdateOnEnter t) ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnSuccess ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> UpdateTodoListEntryText t
              ]
              [ HH.text "💾" ]
          ]
      ]

  renderItem t =
    let
      isEditing = fromMaybe 0 state.editing == t.id
    in
      HH.li [ HP.classes [ B.listGroupItem, B.border0, B.alignItemsCenter ] ]
        [ if isEditing then
            renderEditingItem t
          else if t.isDone then
            renderCompletedItem t
          else
            renderNotCompletedItem t
        ]

notEmpty :: String -> Boolean
notEmpty t = not $ eq t ""

isEnter :: KeyboardEvent -> Boolean
isEnter e = eq (key e) "Enter"

handleAction :: forall cs o m. (MonadAff m) => ComponentAction → H.HalogenM ComponentState ComponentAction cs o m Unit
handleAction = case _ of
  GetTodoList -> do
    result <- liftAff $ getTodoList
    H.modify_ \st -> st { todoList = result, isLoading = false }

  ToggleIsDone e -> do
    _ <- liftAff $ updateTodoList e { isDone = not e.isDone }
    result <- liftAff $ getTodoList
    H.modify_ \st -> st { todoList = result, isLoading = false, editing = Nothing }

  DeleteTodoListEntry id -> do
    _ <- liftAff $ deleteTodoList id
    result <- liftAff $ getTodoList
    H.modify_ \st -> st { todoList = result, isLoading = false }

  EditTodoListEntry id -> do
    H.modify_ \st -> st { editing = Just id, editingText = "" }

  SetEditingListEntryText t -> do
    H.modify_ \st -> st { editingText = t }

  SetNewEntryText t -> do
    H.modify_ \st -> st { newEntryText = t }

  SubmitNewEntry -> do
    state <- H.get
    case notEmpty state.newEntryText of
      true -> do
        _ <- liftAff $ createTodoEntry { id: 0, text: state.newEntryText, isDone: false }
        result <- liftAff $ getTodoList
        H.modify_ \st -> st { todoList = result, isLoading = false, newEntryText = "" }
      _ -> pure unit

  UpdateTodoListEntryText e -> do
    state <- H.get
    case notEmpty state.editingText of
      true -> do
        _ <- liftAff $ updateTodoList e { text = state.editingText }
        result <- liftAff $ getTodoList
        H.modify_ \st -> st { todoList = result, isLoading = false, editing = Nothing }
      _ -> pure unit

  SubmitOnEnter e -> do
    case isEnter e of
      true -> handleAction SubmitNewEntry
      _ -> pure unit

  UpdateOnEnter entry event -> do
    case isEnter event of
      true -> handleAction $ UpdateTodoListEntryText entry
      _ -> pure unit

