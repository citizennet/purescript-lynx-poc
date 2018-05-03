module Lynx.Components.Builder where

import Prelude

import Control.Monad.Aff.Console (CONSOLE)
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , jsonEmptyObject
  , (.?)
  , (:=)
  , (~>)
  )
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX)
import Lynx.Components.Form as Component

--  data Query a
--    = AddInput FormInput a
--    | UpdateInput InputRef FormInputConfig a
--    | UpdateLabel InputRef FormInputConfig FBInput String a
--    | UpdateKey InputRef FormInputConfig FBInput String a
--    | UpdateHelptext InputRef FormInputConfig FBInput String a
--    | ToggleRequired InputRef FormInputConfig a
--    | Submit a

--  type State =
--    { config :: FBFormConfig }

type Input = Unit

type Message = Void

type Effects eff =
  ( ajax :: AJAX
  , console :: CONSOLE
  | eff
  )

type ChildQuery v i r = Component.Query v i r
type ChildSlot = Unit

data FormInput
  = ShortText { label :: String, key :: String, helptext :: String }
  | LongText { label :: String, key :: String, helptext :: String }

instance decodeJsonFormInput :: DecodeJson FormInput where
  decodeJson json = do
    x <- decodeJson json
    type_ <- x .? "inputType"
    input <- case type_ of
      "ShortText" -> do
        label <- x .? "label"
        key <- x .? "key"
        helptext <- x .? "helptext"
        pure $ ShortText { label, key, helptext }
      "LongText" -> do
        label <- x .? "label"
        key <- x .? "key"
        helptext <- x .? "helptext"
        pure $ LongText { label, key, helptext }
      _ -> Left $ "No case written to decode:\n" <> "  " <> type_ <> "\n"
    pure input

instance encodeJsonFormInput :: EncodeJson FormInput where
  encodeJson = case _ of
    ShortText st -> do
      "inputType" := "ShortText"
      ~> "label" := st.label
      ~> "key" := st.key
      ~> "helptext" := st.helptext
      ~> jsonEmptyObject
    LongText st -> do
      "inputType" := "LongText"
      ~> "label" := st.label
      ~> "key" := st.key
      ~> "helptext" := st.helptext
      ~> jsonEmptyObject

--  handleInput
--    :: Component.State v i r
--    -> InputRef
--    -> H.ComponentHTML (Component.Query v i FBRelation)
--  handleInput st ref =
--    let attr = HP.attr (HH.AttrName "data-inputref") (show $ unwrap ref)
--        config = Map.lookup ref (_.inputs $ unwrap st.config)
--     in case config of
--          Just (InputConfig { inputType }) -> case inputType of
--            ShortText { label, helptext } ->
--              FormField.field_
--                { helpText: Just helptext
--                , label
--                , valid: Nothing
--                , inputId: (show <<< unwrap) ref
--                }
--                [ Input.input
--                  [ attr
--                  , HE.onValueInput $ HE.input $ Component.UpdateValue ref
--                  , HE.onBlur $ HE.input_ $ Component.Blur ref
--                  , HP.value $ fromMaybe "" $ Map.lookup ref st.form
--                  ]
--                ]
--            LongText { label, helptext } ->
--              FormField.field_
--                { helpText: Just helptext
--                , label
--                , valid: Nothing
--                , inputId: (show <<< unwrap) ref
--                }
--                [ Input.input
--                  [ attr
--                  , HE.onValueInput $ HE.input $ Component.UpdateValue ref
--                  , HE.onBlur $ HE.input_ $ Component.Blur ref
--                  , HP.value $ fromMaybe "" $ Map.lookup ref st.form
--                  ]
--                ]
--          otherwise -> HH.div_ []


--  defaultFormConfig :: FBFormConfig
--  defaultFormConfig = FormConfig
--    { id: (FormId 0)
--    , inputs: Map.empty
--    , supply: 0
--    }

--  fbInput :: FBFormConfig -> FormInput -> FBFormConfig
--  fbInput (FormConfig f) inputType =
--    wrap $ f { supply =  supply
--             , inputs = Map.insert ref inputConfig f.inputs
--             }
--    where
--      supply = f.supply + 1
--      ref = InputRef f.supply
--      inputConfig = InputConfig { inputType, relations: [], validations: [] }
--
--  updatefbInput :: FBFormConfig -> InputRef -> FormInputConfig -> FBFormConfig
--  updatefbInput (FormConfig f) ref inputConfig =
--    FormConfig $ f { inputs = Map.insert ref inputConfig f.inputs }

--  component
--    :: ∀ eff
--     . H.Component HH.HTML Query Unit Message (Aff (Effects eff))
--  component =
--    H.parentComponent
--      { initialState
--      , render
--      , eval
--      , receiver: const Nothing
--      }
--    where
--      initialState :: Input -> State
--      initialState _ = { config: defaultFormConfig }
--
--      eval
--        :: Query
--        ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (Effects eff))
--      eval = case _ of
--        Submit a -> do
--          state <- H.get
--          let state' = spy state
--          pure a
--
--        AddInput i a -> do
--          state <- H.get
--          H.modify _ { config = fbInput state.config i }
--          pure a
--
--        UpdateInput ref i a -> do
--          state <- H.get
--          H.modify _ { config = updatefbInput state.config ref i}
--          pure a
--
--        UpdateHelptext ref (InputConfig inputConfig) i helptext a -> do
--          let i' = updateKey i helptext
--              inputConfig' = InputConfig (inputConfig { inputType = i' })
--              updateKey (ShortText x) k = ShortText $ x { helptext = helptext }
--              updateKey (LongText x) k = LongText $ x { helptext = helptext }
--          eval $ UpdateInput ref inputConfig' a
--
--        UpdateKey ref (InputConfig inputConfig) i key a -> do
--          let i' = updateKey i key
--              inputConfig' = InputConfig (inputConfig { inputType = i' })
--              updateKey (ShortText x) k = ShortText $ x { key = k }
--              updateKey (LongText x) k = LongText $ x { key = k }
--          eval $ UpdateInput ref inputConfig' a
--
--        UpdateLabel ref (InputConfig inputConfig) i label a -> do
--          let i' = updateLabel i label
--              inputConfig' = InputConfig (inputConfig { inputType = i' })
--              updateLabel (ShortText x) l = ShortText $ x { label = l }
--              updateLabel (LongText x) l = LongText $ x { label = l }
--          eval $ UpdateInput ref inputConfig' a
--
--        ToggleRequired ref (InputConfig inputConfig) a -> do
--          let isReq = isRequired inputConfig.validations
--              inputConfig' = InputConfig (inputConfig { validations = if isReq then [] else [Required] })
--          eval $ UpdateInput ref inputConfig' a
--
--      render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
--      render state =
--        HH.div
--          [ HP.class_ (HH.ClassName "flex bg-grey-lightest") ]
--          [ HH.div
--            [ HP.class_ (HH.ClassName "w-1/4 h-screen m-8") ]
--            [ mkInput
--              { color: "bg-red"
--              , icon: "fa fa-align-justify"
--              , label: "Short Text"
--              , type_: ShortText { label: "", key: "", helptext: "" }
--              }
--            , mkInput
--              { color: "bg-green"
--              , icon: "fa fa-align-justify"
--              , label: "Long Text"
--              , type_: LongText { label: "", key: "", helptext: "" }
--              }
--            ]
--          , HH.div
--            [ HP.class_ (HH.ClassName "w-1/2 h-screen bg-grey-lightest") ]
--            [ Button.buttonDark
--              [ HE.onClick $ HE.input_ Submit ]
--              [ HH.text "Submit" ]
--            , Card.card_ (renderInputs state.config)
--            ]
--          , HH.div
--            [ HP.class_ (HH.ClassName "w-1/4 h-screen bg-grey-lightest") ]
--            [ HH.slot' CP.cp1 unit (Component.component handleValidation handleInput handleRelation) (Left state.config) (const Nothing)
--            ]
--          ]
--        where
--          renderInputs (FormConfig config) =
--            foldrWithKey r [] config.inputs
--            where
--              r k (InputConfig x) acc = snoc acc (renderInputType k x)
--
--              renderInputType k x = case x.inputType of
--                ShortText l ->  renderShortText k l x
--                LongText l -> renderLongText k l x
--
--              renderShortText k l c@{ inputType, validations, relations } =
--                HH.div
--                  [ HP.class_ (HH.ClassName "m-8") ]
--                  [ HH.div_
--                    [ renderIcon { color: "bg-red", icon: "fa fa-align-justify"}
--                    , HH.span_ [ HH.text "Short Text" ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Label"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Input.input
--                      [ HP.value l.label
--                      , HE.onValueInput $ HE.input $ UpdateLabel k (InputConfig c) inputType
--                      ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Key"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Input.input
--                      [ HP.value l.key
--                      , HE.onValueInput $ HE.input $ UpdateKey k (InputConfig c) inputType
--                      ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Helptext"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Input.input
--                      [ HP.value l.helptext
--                      , HE.onValueInput $ HE.input $ UpdateHelptext k (InputConfig c) inputType
--                      ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Required"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Toggle.toggle
--                      [ HP.checked (isRequired validations)
--                      , HE.onClick $ HE.input_ $ ToggleRequired k (InputConfig c)
--                      ]
--                    ]
--                  ]
--
--              renderLongText k l c@{ inputType, validations, relations } =
--                HH.div
--                  [ HP.class_ (HH.ClassName "m-8") ]
--                  [ HH.div_
--                    [ renderIcon { color: "bg-green", icon: "fa fa-align-justify"}
--                    , HH.span_ [ HH.text "Long Text" ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Label"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Input.input
--                      [ HP.value l.label
--                      , HE.onValueInput $ HE.input $ UpdateLabel k (InputConfig c) inputType
--                      ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Key"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Input.input
--                      [ HP.value l.key
--                      , HE.onValueInput $ HE.input $ UpdateKey k (InputConfig c) inputType
--                      ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Helptext"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Input.input
--                      [ HP.value l.helptext
--                      , HE.onValueInput $ HE.input $ UpdateHelptext k (InputConfig c) inputType
--                      ]
--                    ]
--                  , FormField.field_
--                    { helpText: Nothing
--                    , label: "Required"
--                    , valid: Nothing
--                    , inputId: ""
--                    }
--                    [ Toggle.toggle
--                      [ HP.checked (isRequired validations)
--                      , HE.onClick $ HE.input_ $ ToggleRequired k (InputConfig c)
--                      ]
--                    ]
--                  ]
--
--          mkInput { color, icon, label, type_ } =
--            HH.div
--              [ HP.class_ (HH.ClassName "w-full")
--              , HE.onClick $ HE.input_ $ AddInput type_
--              ]
--              [ HH.div
--                [ HP.class_ (HH.ClassName "flex items-center bg-white shadow-md mb-4") ]
--                [ renderIcon { color, icon }
--                , HH.div
--                  [ HP.class_ (HH.ClassName "flex-grow") ]
--                  [ HH.text label ]
--                ]
--              ]
--
--          renderIcon { color, icon } =
--            HH.div
--              [ HP.class_ (HH.ClassName $ "m-2 rounded-sm inline-block " <> color) ]
--              [ HH.div
--                [ HP.class_ (HH.ClassName "w-8 h-8 flex items-center justify-center") ]
--                [ HH.span
--                  [ HP.class_ (HH.ClassName icon) ]
--                  []
--                ]
--              ]
--
--
--  isRequired :: Array FBValidate -> Boolean
--  isRequired xs =
--    case (findIndex (\x -> x == Required) xs) of
--      Just _ -> true
--      Nothing -> false
--
--  foldrWithKey :: ∀ k a b. (k -> a -> b -> b) -> b -> Map k a -> b
--  foldrWithKey f z =
--    foldr (uncurry f) z
--    <<< (toUnfoldable :: Map k a -> Array (Tuple k a))
--
