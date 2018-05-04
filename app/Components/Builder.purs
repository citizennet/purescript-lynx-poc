module App.Components.Builder where

import Prelude

import App.Data.Input.Handler (handleInput) as I
import App.Data.Input.Type (AppInput, Attrs(Attrs), Input(Number, TextArea, Text)) as I
import App.Data.Relate.Handler (handleRelate) as R
import App.Data.Relate.Type (Relate) as R
import App.Data.Validate.Handler (handleValidate) as V
import App.Data.Validate.Type (Validate(..)) as V
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, error)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (setHash)
import DOM.HTML.Window (location)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (elem, filter, snoc, (:))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple, uncurry)
import Debug.Trace (spy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Components.Form as Component
import Lynx.Data.Graph (FormConfig(..), FormId, InputConfig(..), InputRef(..))
import Network.HTTP.Affjax (AJAX, post)
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Block.Toggle as Toggle
import Ocelot.HTML.Properties (css)

data Query a
  = Create I.AppInput a
  | UpdateAttrs InputRef AttrField a
  | UpdateValidate InputRef (Action V.Validate) a
  | Submit a

data Action a
  = Add a
  | Remove a

data AttrField
  = Label String
  | HelpText (Maybe String)

type State = { config :: FormConfig' }
type InputConfig' = InputConfig V.Validate I.AppInput R.Relate
type FormConfig' = FormConfig V.Validate I.AppInput R.Relate

type Input = FormId
type Message = Void

type Effects eff =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM
  | eff
  )

type ChildQuery = Component.Query V.Validate I.AppInput R.Relate
type ChildSlot = Unit

component
  :: ∀ eff
   . H.Component HH.HTML Query Input Message (Aff (Effects eff))
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { config: FormConfig { supply: 0, id: i, inputs: Map.empty } }

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (Effects eff))
    eval = case _ of
      Create input a -> do
         H.modify \st -> st { config = makeInput st.config input }
         pure a

      UpdateAttrs ref change a -> case change of
        Label str -> do
          H.modify \st -> st { config = updateInput st.config ref (setInputLabel str) }
          pure a
        HelpText x -> do
          H.modify \st -> st { config = updateInput st.config ref (setInputHelpText x) }
          pure a

      UpdateValidate ref action a -> case action of
        Add v -> do
          H.modify \st -> st { config = updateInput st.config ref (insertValidation v) }
          pure a
        Remove v -> do
          H.modify \st -> st { config = updateInput st.config ref (removeValidation v) }
          pure a

      Submit a -> do
        -- Submit the form to the backend DB
        state <- H.get
        _ <- submit state
        pure a
      where
        submit state = do
          config <- H.liftAff (saveFormConfig state.config)
          case config of
            Left err -> do
              H.liftAff (error err)
            Right (FormConfig { id }) -> do
              H.liftEff $ setHash ("#/forms/" <> (show <<< unwrap) id) =<< location =<< window

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
    render state =
      HH.div
        [ css "flex bg-grey-lightest" ]
        [ HH.div
          [ css "w-1/4 h-screen m-8" ]
          [ mkInput
            { color: "bg-red"
            , icon: "fa fa-align-justify"
            , label: "Text"
            , type_: I.Text $ I.Attrs { label: "", helpText: Just "" }
            }
          , mkInput
            { color: "bg-green"
            , icon: "fa fa-align-justify"
            , label: "Long Text"
            , type_: I.TextArea $ I.Attrs { label: "", helpText: Just "" }
            }
          ]
        , HH.div
          [ css "w-1/2 h-screen bg-grey-lightest" ]
          [ Button.buttonDark
            [ HE.onClick $ HE.input_ Submit ]
            [ HH.text "Submit" ]
          , Card.card_ (renderInputs state.config)
          ]
        , HH.div
          [ css "w-1/4 h-screen bg-grey-lightest" ]
          [ HH.slot
              unit
              (Component.component
                { handleInput: I.handleInput
                , handleValidate: V.handleValidate
                , handleRelate: R.handleRelate
                })
              (Left state.config)
              (const Nothing)
          ]
        ]
      where
        renderInputs (FormConfig config) =
          foldrWithKey r [] config.inputs
          where
            r k (InputConfig x) acc = snoc acc (renderInputType k x)

            renderInputType k x = case x.inputType of
              I.Text (I.Attrs l) ->  renderText k l x
              I.TextArea (I.Attrs l) -> renderTextArea k l x
              I.Number (I.Attrs l) -> renderText k l x

            renderText k l c@{ inputType, validations, relations } =
              HH.div
                [ css "m-8" ]
                [ HH.div_
                  [ renderIcon { color: "bg-red", icon: "fa fa-align-justify" }
                  , HH.span_ [ HH.text "Text" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< Label
                    ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Helptext"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value $ fromMaybe "" l.helpText
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpText <<< Just
                    ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Required"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Toggle.toggle
                    [ HP.checked (elem V.Required validations)
                    , HE.onClick $ HE.input_ $ UpdateValidate k (Add V.Required)
                    ]
                  ]
                ]

            renderTextArea k l c@{ inputType, validations, relations } =
              HH.div
                [ css "m-8" ]
                [ HH.div_
                  [ renderIcon { color: "bg-green", icon: "fa fa-align-justify"}
                  , HH.span_ [ HH.text "Long Text" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< Label
                    ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Helptext"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value $ fromMaybe "" l.helpText
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpText <<< Just
                    ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Required"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Toggle.toggle
                    [ HP.checked (elem V.Required validations)
                    , HE.onClick $ HE.input_ $ UpdateValidate k (Add V.Required)
                    ]
                  ]
                ]

        mkInput { color, icon, label, type_ } =
          HH.div
            [ css "w-full"
            , HE.onClick $ HE.input_ $ Create type_
            ]
            [ HH.div
              [ css "flex items-center bg-white shadow-md mb-4" ]
              [ renderIcon { color, icon }
              , HH.div
                [ css "flex-grow" ]
                [ HH.text label ]
              ]
            ]

        renderIcon { color, icon } =
          HH.div
            [ css $ "m-2 rounded-sm inline-block " <> color ]
            [ HH.div
              [ css "w-8 h-8 flex items-center justify-center" ]
              [ HH.span
                [ css icon ]
                []
              ]
            ]


makeInput :: FormConfig' -> I.AppInput -> FormConfig'
makeInput (FormConfig config) inputType =
  wrap $ config
    { supply = newSupply
    , inputs = Map.insert newRef newInput config.inputs
    }
  where
    newSupply = config.supply + 1
    newRef = InputRef config.supply
    newInput = InputConfig { inputType, relations: [], validations: [] }

updateInput :: FormConfig' -> InputRef -> (InputConfig' -> InputConfig') -> FormConfig'
updateInput (FormConfig config) ref f =
  wrap $ config { inputs = Map.update (map Just f) ref config.inputs }

setInputLabel :: String -> InputConfig' -> InputConfig'
setInputLabel str (InputConfig i) = case i.inputType of
  I.Text (I.Attrs x) -> InputConfig $ i { inputType = I.Text $ I.Attrs $ x { label = str } }
  I.TextArea (I.Attrs x) -> InputConfig $ i { inputType = I.TextArea $ I.Attrs $ x { label = str } }
  I.Number (I.Attrs x) -> InputConfig $ i { inputType = I.Number $ I.Attrs $ x { label = str } }

setInputHelpText :: Maybe String -> InputConfig' -> InputConfig'
setInputHelpText str (InputConfig i) = case i.inputType of
  I.Text (I.Attrs x) -> InputConfig $ i { inputType = I.Text $ I.Attrs $ x { helpText = str } }
  I.TextArea (I.Attrs x) -> InputConfig $ i { inputType = I.TextArea $ I.Attrs $ x { helpText = str } }
  I.Number (I.Attrs x) -> InputConfig $ i { inputType = I.Number $ I.Attrs $ x { helpText = str } }

insertValidation :: V.Validate -> InputConfig' -> InputConfig'
insertValidation v (InputConfig i) = InputConfig $ i { validations = v : filter ((/=) v) i.validations }

removeValidation :: V.Validate -> InputConfig' -> InputConfig'
removeValidation v (InputConfig i) = InputConfig $ i { validations = filter ((/=) v) i.validations }

foldrWithKey :: ∀ k a b. (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z =
  foldr (uncurry f) z
  <<< (Map.toUnfoldable :: Map k a -> Array (Tuple k a))

saveFormConfig :: ∀ eff. FormConfig' -> Aff (Effects eff) (Either String FormConfig')
saveFormConfig config =
  decodeJson <<< _.response <$> post uri (encodeJson config)
  where
    uri = "http://localhost:3000/forms"
