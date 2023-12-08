module SimpleLists.Views.Home where

import Prelude

import Data.Array (filter, find, head, reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Halogen (ClassName(..), liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Halogen.Router.Class (navigate)
import Halogen.Themes.Bootstrap5 as B
import SimpleLists.Data.SimpleList (SimpleListEntry, SimpleList)
import SimpleLists.Repository.SimpleList (createSimpleList, createSimpleListEntry, deleteSimpleList, deleteSimpleListEntry, getSimpleListEntries, getSimpleListEntriesByListId, getSimpleLists, updateSimpleList, updateSimpleListEntry)
import Web.HTML as W
import Web.HTML.Location as WL
import Web.HTML.Location as WL
import Web.HTML.Window as WW
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

type ComponentState =
  { lists :: Either String (Array SimpleList)
  , currentList :: Maybe SimpleList
  , currentListEntries :: Either String (Array SimpleListEntry)
  , isLoading :: Boolean
  , simpleListEntryBeingEditedId :: Maybe String
  , simpleListEntryBeingEditedText :: String
  , simpleListBeingEditedId :: Maybe String
  , simpleListBeingEditedName :: String
  , newEntryText :: String
  , newListName :: String
  }

data ComponentAction
  = Init (Maybe String)
  | GetAllSimpleLists (Maybe String)
  | GetSimpleList
  | ToggleIsDone SimpleListEntry

  | DeleteSimpleListEntry String
  | DeleteSimpleList String
  | EditSimpleListEntry String
  | EditSimpleList String

  | SetEditingSimpleListEntryText String
  | SetEditingSimpleListName String

  | UpdateSimpleListEntry SimpleListEntry
  | UpdateSimpleList SimpleList

  | SetNewSimpleListEntryText String
  | SetNewListName String

  | SubmitNewSimpleListEntry
  | SubmitNewSimpleList

  | SubmitOnEnter KeyboardEvent
  | UpdateOnEnter SimpleListEntry KeyboardEvent

  | ChangeList String

homeView :: forall q i o m. MonadAff m => Maybe String -> H.Component q i o m
homeView listId =
  H.mkComponent
    { initialState: \_ ->
        { lists: Left ""
        , currentListEntries: Left ""
        , currentList: Nothing
        , isLoading: true
        , simpleListEntryBeingEditedId: Nothing
        , simpleListEntryBeingEditedText: ""
        , simpleListBeingEditedId: Nothing
        , simpleListBeingEditedName: ""
        , newEntryText: ""
        , newListName: ""
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just (Init listId)
        }
    }

getSimpleListNameOrEmptyString :: Maybe SimpleList -> String
getSimpleListNameOrEmptyString l = case l of
  Just l -> l.name
  Nothing -> ""

getSimpleListIdOrEmptyString :: Maybe SimpleList -> String
getSimpleListIdOrEmptyString l = case l of
  Just l -> l.id
  Nothing -> ""

render :: forall cs m. ComponentState -> H.ComponentHTML ComponentAction cs m
render state =
  HH.section [ HP.classes [ B.vh100 ] ]
    [ HH.div [ HP.classes [ B.container ] ]
        [ HH.h1
            [ HP.classes [ B.textCenter ] ]
            [ HH.text $ "Current list: " <> getSimpleListNameOrEmptyString state.currentList ]
        , if state.isLoading then
            HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Loading..." ]
          else case state.currentListEntries of
            Left s -> HH.h2 [ HP.classes [ B.textCenter, B.bgDanger ] ] [ HH.text $ "Error: " <> s ]
            Right simpleListEntries ->
              let
                notCompletedEntries = reverse $ filter (\t -> not t.isDone) simpleListEntries

                completedEntries = reverse $ filter (\t -> t.isDone) simpleListEntries

              in
                HH.div_
                  [ HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "New item:" ]
                  , createItem state.newEntryText
                  , HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Active items:" ]
                  , HH.ul [ HP.classes [ B.listGroup, B.rounded0 ] ] (map renderItem notCompletedEntries)
                  , HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Completed items:" ]
                  , HH.ul [ HP.classes [ B.listGroup, B.rounded0 ] ] (map renderItem completedEntries)
                  , HH.hr [ HP.classes [ B.hr ], HP.style "height: 250px;" ]
                  ]
        , if state.isLoading then
            HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Loading..." ]
          else case state.lists of
            Left s -> HH.h2 [ HP.classes [ B.textCenter, B.bgDanger ] ] [ HH.text $ "Error: " <> s ]
            Right lists ->
              HH.div [ HP.classes [ B.bgWarning ] ] --,HP.style "background-color: #45A0E2;"]
                [ HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "Lists:" ]
                , HH.ul [ HP.classes [ B.listGroup, B.rounded0 ] ] (map renderListItem lists)
                , HH.h2 [ HP.classes [ B.textCenter ] ] [ HH.text "New list:" ]
                , createList state.newListName
                ]
        ]
    ]
  where
  createItem t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t, HE.onValueInput SetNewSimpleListEntryText, HE.onKeyDown SubmitOnEnter ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnSuccess ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> SubmitNewSimpleListEntry
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
              , HE.onClick \_ -> DeleteSimpleListEntry t.id
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
              , HE.onClick \_ -> EditSimpleListEntry t.id
              ]
              [ HH.text "✎" ]
          ]
      ]

  renderEditingItem t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t.text, HE.onValueInput SetEditingSimpleListEntryText, HE.onKeyDown (UpdateOnEnter t) ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnSuccess ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> UpdateSimpleListEntry t
              ]
              [ HH.text "💾" ]
          ]
      ]

  renderItem t =
    let
      isEditing = fromMaybe "0" state.simpleListEntryBeingEditedId == t.id
    in
      HH.li [ HP.classes [ B.listGroupItem, B.border0, B.alignItemsCenter ] ]
        [ if isEditing then
            renderEditingItem t
          else if t.isDone then
            renderCompletedItem t
          else
            renderNotCompletedItem t
        ]

  createList t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t, HE.onValueInput SetNewListName, HE.onKeyDown SubmitOnEnter ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnSuccess ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> SubmitNewSimpleList
              ]
              [ HH.text "💾" ]
          ]
      ]

  -- [ HH.a
  --     [ HE.onClick \_ -> navigate route
  --     , HP.href "javascript:"
  --     ]
  --     [ HH.text $ label route ]
  -- ]

  renderEditingListItem t =
    HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
      [ HH.input [ HP.classes [ B.formControl ], HP.type_ HP.InputText, HP.value t.name, HE.onValueInput SetEditingSimpleListName ] --, HE.onKeyDown (UpdateOnEnter t) ]
      , HH.div [ HP.classes [ B.inputGroupText ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnSuccess ]
              , HP.type_ HP.ButtonButton
              , HE.onClick \_ -> UpdateSimpleList t
              ]
              [ HH.text "💾" ]
          ]
      ]

  renderListItem t =
    let
      isEditing = fromMaybe "0" state.simpleListBeingEditedId == t.id
      bgColor = case getSimpleListIdOrEmptyString state.currentList == t.id of
        true -> B.bgInfo
        _ -> unitClass
    in
      if isEditing then
        renderEditingListItem t
      else
        HH.div [ HP.classes [ B.inputGroup, B.mb3 ] ]
          [ HH.div [ HP.classes [ B.inputGroupText ] ]
              [ HH.button
                  [ HP.classes [ B.btn, B.btnDanger ]
                  , HP.type_ HP.ButtonButton
                  , HE.onClick \_ -> DeleteSimpleList t.id
                  ]
                  [ HH.text "x" ]
              ]
          , HH.input [ HP.classes [ B.formControl, bgColor ], HP.type_ HP.InputText, HP.value t.name, HP.disabled false, HE.onClick \_ -> ChangeList t.id ]
          , HH.div [ HP.classes [ B.inputGroupText ] ]
              [ HH.button
                  [ HP.classes [ B.btn, B.btnPrimary ]
                  , HP.type_ HP.ButtonButton
                  , HE.onClick \_ -> EditSimpleList t.id
                  ]
                  [ HH.text "✎" ]
              ]
          ]

-- renderList t =
--   let
--     bgColor = case getSimpleListIdOrEmptyString state.currentList == t.id of
--       true -> B.bgSuccess
--       _ -> unitClass
--   in
--     HH.li [ HP.classes [ B.listGroupItem, B.border0, B.alignItemsCenter ] ]
--       [ HH.a [ HP.href t.id, HP.classes [ B.textDark, bgColor ] ] [ HH.text t.name ]
--       ]

unitClass :: ClassName
unitClass =
  ClassName "a"

notEmpty :: String -> Boolean
notEmpty t = not $ eq t ""

isEnter :: KeyboardEvent -> Boolean
isEnter e = eq (key e) "Enter"

handleAction :: forall cs o m. (MonadAff m) => ComponentAction → H.HalogenM ComponentState ComponentAction cs o m Unit
handleAction = case _ of
  Init listId -> do
    handleAction $ GetAllSimpleLists listId

  GetAllSimpleLists maybeListId -> do
    allLists <- liftAff $ getSimpleLists
    case allLists of
      Right l -> do
        case maybeListId of
          Just id -> do
            let maybeLatest = find (\i -> i.id == id) l
            H.modify_ \st -> st { lists = allLists, currentList = maybeLatest, isLoading = false }

          Nothing -> do
            let maybeLatest = head l
            H.modify_ \st -> st { lists = allLists, currentList = maybeLatest, isLoading = false }
      _ -> pure unit
    handleAction GetSimpleList

  GetSimpleList -> do
    state <- H.get
    case state.currentList of
      Just latest -> do
        result <- liftAff $ getSimpleListEntriesByListId latest.id
        H.modify_ \st -> st { currentListEntries = result, isLoading = false }
      _ -> pure unit

  ToggleIsDone e -> do
    _ <- liftAff $ updateSimpleListEntry { id: e.id, text: e.text, isDone: not e.isDone, listId: e.listId }
    handleAction GetSimpleList
    H.modify_ \st -> st { simpleListEntryBeingEditedId = Nothing }

  DeleteSimpleListEntry id -> do
    _ <- liftAff $ deleteSimpleListEntry id
    handleAction GetSimpleList

  DeleteSimpleList id -> do
    window <- liftEffect W.window
    confirm <- liftEffect $ WW.confirm "Are you certain about deleting this list?" window
    if confirm then do
      _ <- liftAff $ deleteSimpleList id
      pure unit
    else
      pure unit

    handleAction $ Init Nothing

  EditSimpleListEntry id -> do
    H.modify_ \st -> st { simpleListEntryBeingEditedId = Just id, simpleListEntryBeingEditedText = "" }

  EditSimpleList id -> do
    H.modify_ \st -> st { simpleListBeingEditedId = Just id, simpleListBeingEditedName = "" }

  SetEditingSimpleListEntryText t -> do
    H.modify_ \st -> st { simpleListEntryBeingEditedText = t }

  SetEditingSimpleListName t -> do
    H.modify_ \st -> st { simpleListBeingEditedName = t }

  SetNewSimpleListEntryText t -> do
    H.modify_ \st -> st { newEntryText = t }

  SetNewListName t -> do
    H.modify_ \st -> st { newListName = t }

  SubmitNewSimpleListEntry -> do
    state <- H.get
    case notEmpty state.newEntryText of
      true -> do
        _ <- liftAff $ createSimpleListEntry { text: state.newEntryText, isDone: false, listId: getSimpleListIdOrEmptyString state.currentList }
        handleAction $ Init Nothing
        H.modify_ \st -> st { newEntryText = "" }
      _ -> pure unit

  SubmitNewSimpleList -> do
    state <- H.get
    case notEmpty state.newListName of
      true -> do
        _ <- liftAff $ createSimpleList { name: state.newListName }
        handleAction $ GetAllSimpleLists Nothing
        H.modify_ \st -> st { newListName = "" }
      _ -> pure unit

  UpdateSimpleListEntry e -> do
    state <- H.get
    case notEmpty state.simpleListEntryBeingEditedText of
      true -> do
        _ <- liftAff $ updateSimpleListEntry { id: e.id, text: state.simpleListEntryBeingEditedText, isDone: not e.isDone, listId: e.listId }
        handleAction GetSimpleList
        H.modify_ \st -> st { simpleListEntryBeingEditedId = Nothing }
      _ -> pure unit

  UpdateSimpleList e -> do
    state <- H.get
    case notEmpty state.simpleListBeingEditedName of
      true -> do
        _ <- liftAff $ updateSimpleList { id: e.id, name: state.simpleListBeingEditedName }
        H.modify_ \st -> st { simpleListBeingEditedId = Nothing }
        handleAction $ Init Nothing
      _ -> pure unit

  SubmitOnEnter e -> do
    case isEnter e of
      true -> handleAction SubmitNewSimpleListEntry
      _ -> pure unit

  UpdateOnEnter entry event -> do
    case isEnter event of
      true -> handleAction $ UpdateSimpleListEntry entry
      _ -> pure unit

  ChangeList id -> do
    window <- liftEffect W.window
    location <- liftEffect (WW.location window)
    liftEffect $ WL.assign id location
    pure unit

