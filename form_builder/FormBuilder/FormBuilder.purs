module FormBuilder where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.State (class MonadState, get)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, fromString, jsonEmptyObject, (:=), (~>), (.?))
import Data.Array (findIndex, snoc)
import Data.Either (Either(..))
import Data.Either.Nested (Either1)
import Data.Foldable (foldr)
import Data.Foldable as Foldable
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Map (Map, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), uncurry)
import Debug.Trace (spy)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp3) as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Component as Component
import Lynx.Graph (FormConfig(..), FormId(..), InputConfig(..), InputRef(..), input, runFormBuilder)
import Network.HTTP.Affjax (AJAX)
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Block.Toggle as Toggle

data Query a 
  = AddInput FBInput a
  | UpdateInput InputRef FBInputConfig a
  | UpdateLabel InputRef FBInputConfig FBInput String a
  | UpdateKey InputRef FBInputConfig FBInput String a
  | ToggleRequired InputRef FBInputConfig a
  | Submit a

type State =
  { config :: FBFormConfig }

type Input = Unit

type Message = Void

type Effects eff =
  ( ajax :: AJAX
  , console :: CONSOLE
  | eff
  )

type ChildQuery = Coproduct1 (Component.Query FBValidate FBInput FBRelation)

type ChildSlot = Either1 Unit

data FBRelation 
  = MustEqual InputRef
  | Clear InputRef

instance decodeJsonFBRelation :: DecodeJson FBRelation where
  decodeJson json = do
    x <- decodeJson json
    ref <- x .? "ref"
    type_ <- x .? "relationType"
    relation <- case type_ of
      "MustEqual" -> pure (MustEqual ref)
      "Clear" -> pure (Clear ref)
      _ -> Left $ "No case written to decode:\n" <> "  " <> type_ <> "\n"
    pure relation

instance encodeJsonFBRelation :: EncodeJson FBRelation where
  encodeJson = case _ of
    MustEqual ref -> do
      "relationType" := "MustEqual"
      ~> "ref" := ref
      ~> jsonEmptyObject
    Clear ref ->
      "relationType" := "Clear"
      ~> "ref" := ref
      ~> jsonEmptyObject

handleRelation :: ∀ eff m
   . MonadState (Component.State FBValidate FBInput FBRelation) m
  => MonadAff (Component.Effects eff) m
  => FBRelation 
  -> InputRef
  -> m Unit
handleRelation relation refA = case relation of
  MustEqual refB -> do
    st <- H.get
    let equal = do
          v0 <- Map.lookup refA st.form
          v1 <- Map.lookup refB st.form
          pure $ v0 == v1
    case equal of
      Just true -> pure unit
      otherwise -> do
        H.liftAff $ log $ show refA <> " is NOT equal to " <> show refB
        pure unit

  Clear refB -> do
    H.modify \st -> st { form = Map.insert refB "" st.form }
    H.liftAff $ logShow $ "Deleted " <> show refB
    pure unit

data FBInput
  = ShortText { label :: String, key :: String }
  | LongText { label :: String, key :: String }
  {--| Number { label :: String, key :: String }--}

instance decodeJsonFBInput :: DecodeJson FBInput where
  decodeJson json = do
    x <- decodeJson json
    type_ <- x .? "inputType"
    input <- case type_ of
      "ShortText" -> do
        label <- x .? "label"
        key <- x .? "key"
        pure $ ShortText { label, key }
      "LongText" -> do
        label <- x .? "label"
        key <- x .? "key"
        pure $ LongText { label, key }
      {--"Number" -> do--}
        {--label <- x .? "label"--}
        {--key <- x .? "key"--}
        {--pure $ Number { label, key }--}
      _ -> Left $ "No case written to decode:\n" <> "  " <> type_ <> "\n"
    pure input

instance encodeJsonFBInput :: EncodeJson FBInput where
  encodeJson = case _ of
    ShortText st -> do
      "inputType" := "ShortText"
      ~> "label" := st.label
      ~> "key" := st.key
      ~> jsonEmptyObject
    LongText st -> do
      "inputType" := "LongText"
      ~> "label" := st.label
      ~> "key" := st.key
      ~> jsonEmptyObject
    {--Number st -> do--}
      {--"inputType" := "Number"--}
      {--~> "label" := st.label--}
      {--~> "key" := st.key--}
      {--~> jsonEmptyObject--}

handleInput
  :: Component.State FBValidate FBInput FBRelation
  -> InputRef
  -> H.ComponentHTML (Component.Query FBValidate FBInput FBRelation)
handleInput st ref =
  let attr = HP.attr (HH.AttrName "data-inputref") (show $ unwrap ref)
      config = Map.lookup ref (_.inputs $ unwrap st.config)
   in case config of
        Just (InputConfig { inputType }) -> case inputType of
          ShortText { label } ->
            FormField.field_
              { helpText: Nothing
              , label
              , valid: Nothing
              , inputId: (show <<< unwrap) ref
              }
              [ Input.input
                [ attr
                , HE.onValueInput $ HE.input $ Component.UpdateValue ref
                , HE.onBlur $ HE.input_ $ Component.Blur ref
                , HP.value $ fromMaybe "" $ Map.lookup ref st.form
                ]  
              ]
          LongText { label } ->
            FormField.field_
              { helpText: Nothing
              , label
              , valid: Nothing
              , inputId: (show <<< unwrap) ref
              }
              [ Input.input
                [ attr
                , HE.onValueInput $ HE.input $ Component.UpdateValue ref
                , HE.onBlur $ HE.input_ $ Component.Blur ref
                , HP.value $ fromMaybe "" $ Map.lookup ref st.form
                ]  
              ]
        otherwise -> HH.div_ []

data FBValidate 
  = Required

instance eqFBValidate :: Eq FBValidate where
  eq Required Required = true 

instance showFBValidate :: Show FBValidate where
  show Required = "Required"

instance decodeJsonFBValidate :: DecodeJson FBValidate where
  decodeJson json = do
    x <- decodeJson json
    type_ <- x .? "validationType"
    validate <- case type_ of
      "Required" -> pure Required 
      _ -> Left $ "No case written to decode:\n" <> "  " <> type_ <> "\n"
    pure validate

instance encodeJsonFBValidate :: EncodeJson FBValidate where
  encodeJson = case _ of
    Required -> do
      "validationType" := "Required"
      ~> jsonEmptyObject

handleValidation :: FBValidate -> String -> Either String String
handleValidation v str = case v of
  Required ->
    if String.null str
      then Left "Field is required"
      else Right str

type FBFormConfig = FormConfig FBValidate FBInput FBRelation

type FBInputConfig = InputConfig FBValidate FBInput FBRelation

defaultFormConfig :: FBFormConfig
defaultFormConfig = FormConfig
  { id: (FormId 0)
  , inputs: Map.empty
  , supply: 0
  }

fbInput :: FBFormConfig -> FBInput -> FBFormConfig
fbInput (FormConfig f) inputType =
  wrap $ f { supply =  supply
           , inputs = Map.insert ref inputConfig f.inputs
           }
  where
    supply = f.supply + 1
    ref = InputRef f.supply
    inputConfig = InputConfig { inputType, relations: [], validations: [] }

updatefbInput :: FBFormConfig -> InputRef -> FBInputConfig -> FBFormConfig
updatefbInput (FormConfig f) ref inputConfig =
  FormConfig $ f { inputs = Map.insert ref inputConfig f.inputs }

formBuilder
  :: ∀ eff
   . H.Component HH.HTML Query Unit Message (Aff (Effects eff))
formBuilder =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState _ = { config: defaultFormConfig }

    eval 
      :: Query 
      ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (Effects eff))
    eval = case _ of
      Submit a -> do
        state <- H.get
        let state' = spy state
        pure a

      AddInput i a -> do
        state <- H.get
        H.modify _ { config = fbInput state.config i }
        pure a

      UpdateInput ref i a -> do
        state <- H.get
        H.modify _ { config = updatefbInput state.config ref i}
        pure a

      UpdateKey ref (InputConfig inputConfig) i key a -> do
        let i' = updateKey i key
            inputConfig' = InputConfig (inputConfig { inputType = i' })
            updateKey (ShortText x) k = ShortText $ x { key = k }
            updateKey (LongText x) k = LongText $ x { key = k }
            {--updateKey (Number x) k = Number $ x { key = k }--}
        eval $ UpdateInput ref inputConfig' a

      UpdateLabel ref (InputConfig inputConfig) i label a -> do
        let i' = updateLabel i label
            inputConfig' = InputConfig (inputConfig { inputType = i' })
            updateLabel (ShortText x) l = ShortText $ x { label = l }
            updateLabel (LongText x) l = LongText $ x { label = l }
            {--updateLabel (Number x) l = Number $ x { label = l }--}
        eval $ UpdateInput ref inputConfig' a

      ToggleRequired ref (InputConfig inputConfig) a -> do
        let isReq = isRequired inputConfig.validations
            inputConfig' = InputConfig (inputConfig { validations = if isReq then [] else [Required] })
        eval $ UpdateInput ref inputConfig' a

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
    render state =
      HH.div
        [ HP.class_ (HH.ClassName "flex bg-grey-lightest") ]
        [ HH.div
          [ HP.class_ (HH.ClassName "w-1/4 h-screen m-8") ]
          [ mkInput 
            { color: "bg-red"
            , icon: "fa fa-align-justify"
            , label: "Short Text"
            , type_: ShortText { label: "", key: "" }
            }
          , mkInput 
            { color: "bg-green"
            , icon: "fa fa-align-justify"
            , label: "Long Text"
            , type_: LongText { label: "", key: "" } 
            }
          {--, mkInput --}
            {--{ color: "bg-blue"--}
            {--, icon: "fa fa-list-ol"--}
            {--, label: "Number"--}
            {--, type_: Number { label: "", key: "" } --}
            {--}--}
          ]
        , HH.div
          [ HP.class_ (HH.ClassName "w-1/2 h-screen bg-grey-lightest") ]
          [ Button.buttonDark
            [ HE.onClick $ HE.input_ Submit ]
            [ HH.text "Submit" ]
          , Card.card_ (renderInputs state.config) 
          ]
        , HH.div
          [ HP.class_ (HH.ClassName "w-1/4 h-screen bg-grey-lightest") ]
          [ HH.slot' CP.cp1 unit (Component.component handleValidation handleInput handleRelation) (Left state.config) (const Nothing) 
          ]
        ]
      where
        renderInputs (FormConfig config) =
          foldrWithKey r [] config.inputs
          where
            r k (InputConfig x) acc = snoc acc (renderInputType k x)

            renderInputType k x = case x.inputType of
              ShortText l ->  renderShortText k l x
              LongText l -> renderLongText k l x
              {--Number l -> renderNumber k l x--}

            renderShortText k l c@{ inputType, validations, relations } =
              HH.div
                [ HP.class_ (HH.ClassName "m-8") ]
                [ HH.div_
                  [ renderIcon { color: "bg-red", icon: "fa fa-align-justify"} 
                  , HH.span_ [ HH.text "Short Text" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , valid: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label
                    , HE.onValueInput $ HE.input $ UpdateLabel k (InputConfig c) inputType
                    ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Key"
                  , valid: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.key
                    , HE.onValueInput $ HE.input $ UpdateKey k (InputConfig c) inputType
                    ]  
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Required"
                  , valid: Nothing
                  , inputId: ""
                  }
                  [ Toggle.toggle
                    [ HP.checked (isRequired validations)
                    , HE.onClick $ HE.input_ $ ToggleRequired k (InputConfig c)
                    ]
                  ]
                ]
            
            renderLongText k l c@{ inputType, validations, relations } =
              HH.div
                [ HP.class_ (HH.ClassName "m-8") ]
                [ HH.div_
                  [ renderIcon { color: "bg-green", icon: "fa fa-align-justify"} 
                  , HH.span_ [ HH.text "Long Text" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , valid: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label 
                    , HE.onValueInput $ HE.input $ UpdateLabel k (InputConfig c) inputType
                    ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Key"
                  , valid: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.key
                    , HE.onValueInput $ HE.input $ UpdateKey k (InputConfig c) inputType
                    ]  
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Required"
                  , valid: Nothing
                  , inputId: ""
                  }
                  [ Toggle.toggle
                    [ HP.checked (isRequired validations)
                    , HE.onClick $ HE.input_ $ ToggleRequired k (InputConfig c)
                    ]
                  ]
                ]

            {--renderNumber k l c@{ inputType, validations, relations } =--}
              {--HH.div--}
                {--[ HP.class_ (HH.ClassName "m-8") ]--}
                {--[ HH.div_--}
                  {--[ renderIcon { color: "bg-blue", icon: "fa fa-list-ol"} --}
                  {--, HH.span_ [ HH.text "Number" ]--}
                  {--]--}
                {--, FormField.field_--}
                  {--{ helpText: Nothing--}
                  {--, label: "Key"--}
                  {--, valid: Nothing--}
                  {--, inputId: ""--}
                  {--}--}
                  {--[ Input.input--}
                    {--[ HP.value l.key--}
                    {--, HE.onValueInput $ HE.input $ UpdateKey k (InputConfig c) inputType--}
                    {--]  --}
                  {--]--}
                {--, FormField.field_--}
                  {--{ helpText: Nothing--}
                  {--, label: "Label"--}
                  {--, valid: Nothing--}
                  {--, inputId: ""--}
                  {--}--}
                  {--[ Input.input--}
                    {--[ HP.value l.label --}
                    {--, HE.onValueInput $ HE.input $ UpdateLabel k (InputConfig c) inputType--}
                    {--]  --}
                  {--]--}
                {--, FormField.field_--}
                  {--{ helpText: Nothing--}
                  {--, label: "Required"--}
                  {--, valid: Nothing--}
                  {--, inputId: ""--}
                  {--}--}
                  {--[ Toggle.toggle--}
                    {--[ HP.checked (isRequired validations)--}
                    {--, HE.onClick $ HE.input_ $ ToggleRequired k (InputConfig c)--}
                    {--]--}
                  {--]--}
                {--]--}

        mkInput { color, icon, label, type_ } =
          HH.div
            [ HP.class_ (HH.ClassName "w-full")
            , HE.onClick $ HE.input_ $ AddInput type_
            ]
            [ HH.div
              [ HP.class_ (HH.ClassName "flex items-center bg-white shadow-md mb-4") ]
              [ renderIcon { color, icon }
              , HH.div
                [ HP.class_ (HH.ClassName "flex-grow") ]
                [ HH.text label ]
              ]
            ]

        renderIcon { color, icon } =
          HH.div
            [ HP.class_ (HH.ClassName $ "m-2 rounded-sm inline-block " <> color) ]
            [ HH.div
              [ HP.class_ (HH.ClassName "w-8 h-8 flex items-center justify-center") ]
              [ HH.span
                [ HP.class_ (HH.ClassName icon) ]
                []
              ]
            ]


isRequired :: Array FBValidate -> Boolean
isRequired xs = 
  case (findIndex (\x -> x == Required) xs) of
    Just _ -> true
    Nothing -> false

foldrWithKey :: ∀ k a b. (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z =
  foldr (uncurry f) z
  <<< (toUnfoldable :: Map k a -> Array (Tuple k a))


