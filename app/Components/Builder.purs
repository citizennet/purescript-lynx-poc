module App.Components.Builder where

import Prelude

import App.Data.Input.Handler (handleInput, optionItemToStr)
import App.Data.Input.Type as I
import App.Data.Relate.Handler (handleRelate) as R
import App.Data.Relate.Type (Relate) as R
import App.Data.Validate.Handler (handleValidate) as V
import App.Data.Validate.Type (Validate(..)) as V
import Control.Monad.State.Class (class MonadState)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (deleteAt, elem, filter, snoc, updateAt, (:))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (error)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Components.Form as Form
import Lynx.Data.ForeignAPI (ArrayKeys(..), ItemKeys(..), renderArrayKeys, renderItemKeys, readArrayKeys, readItemKeys, fetch)
import Lynx.Data.Graph (FormConfig(..), FormId, InputConfig(..), InputRef(..))
import Network.HTTP.Affjax (get, post)
import Network.HTTP.Affjax.Request (json) as Request
import Network.HTTP.Affjax.Response (json) as Response
import Network.RemoteData (RemoteData(..), withDefault)
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Block.Radio (radio_) as Radio
import Ocelot.Block.Toggle as Toggle
import Ocelot.HTML.Properties (css)
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

data Query a
  = Create I.AppInput a
  | Initialize a
  | UpdateAttrs InputRef AttrField a
  | UpdateOptValue InputRef Int String a
  | UpdateForeign InputRef ForeignField a
  | ChangeOptions InputRef (ArrayAction (Tuple Int String)) a
  | ChangeValidations InputRef (ArrayAction V.Validate) a
  | RunForeign a
  | Submit a

-- Perform an update on array-based data by adding, removing,
-- or overwriting entirely.
data ArrayAction a
  = Add a
  | Remove a

data AttrField
  = LabelField String
  | HelpTextField (Maybe String)

data ForeignField
 = UrlField String
 | ArrayKeyField String
 | ItemKeyField String

type State = { config :: FormConfig', runForeign :: Boolean }
type InputConfig' = InputConfig V.Validate I.AppInput R.Relate
type FormConfig' = FormConfig V.Validate I.AppInput R.Relate

type Input = FormId
type Message = Void

type ChildQuery = Form.Query V.Validate I.AppInput R.Relate
type ChildSlot = Unit

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
    initialState :: Input -> State
    initialState i =
      { config: FormConfig { supply: 0, id: i, inputs: Map.empty }
      , runForeign: false }

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
    eval = case _ of
      -- Load the existing form config, if it exists;
      -- otherwise, keep the blank config.
      Initialize a -> do
        formId <- H.gets (_.id <<< unwrap <<< _.config)
        res <- H.liftAff $ getFormConfig formId
        traverse_ (\config -> H.put { config, runForeign: false }) res
        pure a

      Create input a -> do
         H.modify_ \st -> st { config = makeInput st.config input }
         pure a

      UpdateAttrs ref change a -> case change of
        LabelField str -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (setInputLabel str) }
          pure a
        HelpTextField x -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (setInputHelpText x) }
          pure a

      UpdateForeign ref change a -> case change of
        UrlField str -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (setForeignUrl str) }
          pure a
        ArrayKeyField str -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (setForeignArrayKeys str) }
          pure a
        ItemKeyField str -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (setForeignItemKeys str) }
          pure a

      UpdateOptValue ref index str a -> do
        H.modify_ \st ->
          st { config = updateInput st.config ref (setOptionText index str) }
        pure a

      ChangeOptions ref action a -> case action of
        Add (Tuple _ val) -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (insertOption val) }
          pure a
        Remove (Tuple index _) -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (removeOption index) }
          pure a

      ChangeValidations ref action a -> case action of
        Add v -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (insertValidation v) }
          pure a
        Remove v -> do
          H.modify_ \st ->
            st { config = updateInput st.config ref (removeValidation v) }
          pure a

      RunForeign a -> do
        H.modify_ _ { runForeign = true }
        H.modify_ _ { runForeign = false }
        pure a

      Submit a -> do
        -- Submit the form to the backend DB
        submit =<< H.get
        pure a

      where
        submit state = do
          config <- H.liftAff (saveFormConfig state.config)
          case config of
            Left err -> do
              H.liftEffect (error err)
            Right (FormConfig { id }) -> do
              H.liftEffect
                $ setHash
                    ("#/forms/" <> (show <<< unwrap) id)
                    =<< location
                    =<< window

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render state =
      let stringInput = I.FormInput { input: "", result: Left [], validate: false }
          numberInput = I.FormInput { input: "", result: Left [], validate: false }
       in
      HH.div
        [ css "flex bg-grey-lightest" ]
        [ HH.div
          [ css "w-1/4 h-screen m-8" ]
          [ mkInput
            { color: "bg-red"
            , icon: "fa fa-align-justify"
            , label: "Text"
            , type_: I.Text (I.Attrs { label: "", helpText: Just "" }) stringInput
            }
          , mkInput
            { color: "bg-green"
            , icon: "fa fa-align-justify"
            , label: "Long Text"
            , type_: I.TextArea (I.Attrs { label: "", helpText: Just "" }) stringInput
            }
          , mkInput
            { color: "bg-blue"
            , icon: "fa fa-align-justify"
            , label: "Number"
            , type_: I.Number (I.Attrs { label: "", helpText: Just "" }) numberInput
            }
          , mkInput
            { color: "bg-yellow"
            , icon: "fa fa-align-justify"
            , label: "Options (Radio)"
            , type_: I.Options
                (I.Attrs { label: "", helpText: Just "" })
                (I.FormInput { input: I.Radio [ ]
                             , result: Left []
                             , validate: false }
                )
            }
          , mkInput
            { color: "bg-yellow"
            , icon: "fa fa-align-justify"
            , label: "Options (Checkbox)"
            , type_: I.Options
                (I.Attrs { label: "", helpText: Just "" })
                (I.FormInput { input: I.Checkbox [ ]
                             , result: Left []
                             , validate: false }
                )
            }
          , mkInput
            { color: "bg-yellow"
            , icon: "fa fa-align-justify"
            , label: "Options (Dropdown)"
            , type_: I.Options
                (I.Attrs { label: "", helpText: Just "" })
                (I.FormInput { input: I.Dropdown [ ]
                             , result: Left []
                             , validate: false }
                )
            }
          , mkInput
            { color: "bg-orange"
            , icon: "fa fa-align-justify"
            , label: "Custom API Options (Radio)"
            , type_: I.OptionsForeign
                (I.Attrs { label: "", helpText: Just "" })
                (I.FormInput { input: I.Radio [ ]
                             , result: Left []
                             , validate: false }
                )
                (I.ForeignData "" (ArrayKeys []) (ItemKeys []))
            }
          , mkInput
            { color: "bg-orange"
            , icon: "fa fa-align-justify"
            , label: "Custom API Options (Checkbox)"
            , type_: I.OptionsForeign
                (I.Attrs { label: "", helpText: Just "" })
                (I.FormInput { input: I.Checkbox [ ]
                             , result: Left []
                             , validate: false }
                )
                (I.ForeignData "" (ArrayKeys []) (ItemKeys []))
            }
          , mkInput
            { color: "bg-orange"
            , icon: "fa fa-align-justify"
            , label: "Custom API Options (Dropdown)"
            , type_: I.OptionsForeign
                (I.Attrs { label: "", helpText: Just "" })
                (I.FormInput { input: I.Dropdown [ ]
                             , result: Left []
                             , validate: false }
                )
                (I.ForeignData "" (ArrayKeys []) (ItemKeys []))
            }
          ]
        , HH.div
          [ css "w-1/2 h-screen bg-grey-lightest" ]
          [ Button.buttonDark
            [ HE.onClick $ HE.input_ Submit
            , css "m-2" ]
            [ HH.text "Submit" ]
          , Button.buttonDark
            [ HE.onClick $ HE.input_ RunForeign
            , css "m-2" ]
            [ HH.text "Fetch Data" ]
          , Card.card_ (renderInputs state.config)
          ]
        , HH.div
          [ css "w-1/4 h-screen bg-grey-lightest" ]
          [ HH.slot
              unit
              (Form.component
                { handleInput: handleInput
                , handleValidate: V.handleValidate
                , handleRelate: R.handleRelate
                , initialize
                })
              (Left (Tuple state.config state.runForeign))
              (const Nothing)
          ]
        ]
      where
        renderInputs (FormConfig config) =
          foldrWithKey r [] config.inputs
          where
            r k (InputConfig x) acc = snoc acc (renderInputType k x)

            renderInputType k x = case x.inputType of
              I.Text (I.Attrs l) _
                -> renderText k l x
              I.TextArea (I.Attrs l) _
                -> renderTextArea k l x
              I.Number (I.Attrs l) _
                -> renderNumber k l x
              I.DateTimeInput (I.Attrs l) _
                -> renderDateTimeInput k l x
              I.Options (I.Attrs l) (I.FormInput { input })
                -> renderOptions k l x input
              I.OptionsForeign
                (I.Attrs l)
                (I.FormInput { input } )
                foreignData
                -> renderOptionsForeign k l x foreignData

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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< LabelField
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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpTextField <<< Just
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
                    , HE.onClick $ HE.input_ $ ChangeValidations k (Add V.Required)
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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< LabelField
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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpTextField <<< Just
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
                    , HE.onClick $ HE.input_ $ ChangeValidations k (Add V.Required)
                    ]
                  ]
                ]

            renderNumber k l c@{ inputType, validations, relations } =
              HH.div
                [ css "m-8" ]
                [ HH.div_
                  [ renderIcon { color: "bg-blue", icon: "fa fa-align-justify" }
                  , HH.span_ [ HH.text "Number" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< LabelField
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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpTextField <<< Just
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
                    , HE.onClick $ HE.input_ $ ChangeValidations k (Add V.Required)
                    ]
                  ]
                ]

            renderDateTimeInput k l c@{ inputType, validations, relations } =
              HH.div
                [ css "m-8" ]
                [ HH.div_
                  [ renderIcon { color: "bg-black", icon: "fa fa-align-justify" }
                  , HH.span_ [ HH.text "Date & Time" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< LabelField
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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpTextField <<< Just
                    ]
                  ]
                , FormField.fieldset_
                  { label: "Component Type"
                  , inputId: "radio-vertical"
                  , helpText: Just "Choose a date, time, or date time component."
                  , error: Nothing
                  }
                  [ HH.div_
                    [ Radio.radio_
                      [ HP.name "datetime"
                      , HP.checked true
                      ]
                      [ HH.text "Date" ]
                    , Radio.radio_
                      [ HP.name "datetime" ]
                      [ HH.text "Time" ]
                    , Radio.radio_
                      [ HP.name "datetime" ]
                      [ HH.text "Date & Time" ]
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
                    , HE.onClick $ HE.input_ $ ChangeValidations k (Add V.Required)
                    ]
                  ]
                ]

            renderOptions k l c@{ inputType, validations, relations } opts =
              HH.div
                [ css "m-8" ]
                [ HH.div_
                  [ renderIcon
                    { color: "bg-yellow", icon: "fa fa-align-justify" }
                  , HH.span_
                    [ HH.text "Options" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< LabelField
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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpTextField <<< Just
                    ]
                  ]
                , FormField.fieldset_
                  { label: "Group"
                  , inputId: ""
                  , helpText: Just "Add options above."
                  , error: Nothing
                  }
                  [ HH.button
                    [ HE.onClick $ HE.input_ $ ChangeOptions k (Add $ Tuple (-1) "") ]
                    [ HH.text "Add" ]
                  , HH.div_ $
                      let f arr = flip mapWithIndex arr $ \i v ->
                            HH.div_
                            [ Input.input
                                [ HP.value $ optionItemToStr v
                                , HE.onValueInput $ HE.input $ \str ->
                                    UpdateOptValue k i str
                                ]
                            , HH.button
                              [ HE.onClick
                                $ HE.input_
                                $ ChangeOptions k
                                  ( Remove $ Tuple i (optionItemToStr v) )
                              ]
                              [ HH.text "Remove" ]
                            ]

                          f' arr = flip mapWithIndex arr $ \i v ->
                            HH.div_
                            [ Input.input
                                [ HP.value v
                                , HE.onValueInput $ HE.input $ \str ->
                                    UpdateOptValue k i str
                                ]
                            , HH.button
                              [ HE.onClick
                                $ HE.input_
                                $ ChangeOptions k
                                  ( Remove $ Tuple i v )
                              ]
                              [ HH.text "Remove" ]
                            ]
                       in case opts of
                         I.Radio arr -> f arr
                         I.Checkbox arr -> f arr
                         I.Dropdown arr -> f' arr
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Required"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Toggle.toggle
                    [ HP.checked (elem V.Required validations)
                    , HE.onClick $ HE.input_ $ ChangeValidations k (Add V.Required)
                    ]
                  ]
                ]

            renderOptionsForeign
              k
              l
              c@{ inputType, validations, relations }
              (I.ForeignData url akeys ikeys)
              =
              HH.div
                [ css "m-8" ]
                [ HH.div_
                  [ renderIcon
                    { color: "bg-orange", icon: "fa fa-align-justify" }
                  , HH.span_
                    [ HH.text "Custom API Options" ]
                  ]
                , FormField.field_
                  { helpText: Nothing
                  , label: "Label"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value l.label
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< LabelField
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
                    , HE.onValueInput $ HE.input $ UpdateAttrs k <<< HelpTextField <<< Just
                    ]
                  ]
                , FormField.field_
                  { helpText: Just "Provide the source URL for your data."
                  , label: "Source URL"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value url
                    , HE.onValueInput $ HE.input $ UpdateForeign k <<< UrlField
                    ]
                  ]
                , FormField.field_
                  { helpText: Just "Tell us how to access the array of items in your data that will display as options. Comma-separate either integers (to represent array indices) or strings (to represent object keys) to describe the path to the items. Leave this empty if the array of items is the response."
                  , label: "Path to Array in JSON Response"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value $ renderArrayKeys akeys
                    , HE.onValueInput $ HE.input $ UpdateForeign k <<< ArrayKeyField
                    ]
                  ]
                , FormField.field_
                { helpText: Just "Tell us how to turn each item into a string in your array of items. For example, if each item is an object and you want to display the \"name\" key, provide that as the key. If the items are already strings, leave this empty."
                  , label: "Path to String in Item in JSON Response"
                  , error: Nothing
                  , inputId: ""
                  }
                  [ Input.input
                    [ HP.value $ renderItemKeys ikeys
                    , HE.onValueInput $ HE.input $ UpdateForeign k <<< ItemKeyField
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
                    , HE.onClick $ HE.input_ $ ChangeValidations k (Add V.Required)
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
    newInput = InputConfig $ case inputType of
      I.Number _ _ -> { inputType, relations: [], validations: [ V.IsNumber ] }
      otherwise  -> { inputType, relations: [], validations: [] }

updateInput :: FormConfig' -> InputRef -> (InputConfig' -> InputConfig') -> FormConfig'
updateInput (FormConfig config) ref f =
  wrap $ config { inputs = Map.update (map Just f) ref config.inputs }

updateForm
  :: Map InputRef I.AppInput
  -> InputRef
  -> (I.AppInput -> I.AppInput)
  -> Map InputRef I.AppInput
updateForm m ref f = Map.update (map Just f) ref m

setInputLabel :: String -> InputConfig' -> InputConfig'
setInputLabel str (InputConfig i) = InputConfig $ case i.inputType of
  I.Text (I.Attrs x) formInput ->
    i { inputType = I.Text (I.Attrs $ x { label = str }) formInput }
  I.TextArea (I.Attrs x) formInput ->
    i { inputType = I.TextArea (I.Attrs $ x { label = str }) formInput }
  I.Number (I.Attrs x) formInput ->
    i { inputType = I.Number (I.Attrs $ x { label = str }) formInput }
  I.DateTimeInput (I.Attrs x) formInput ->
    i { inputType = I.DateTimeInput (I.Attrs $ x { label = str }) formInput }
  I.Options (I.Attrs x) formInput ->
    i { inputType = I.Options (I.Attrs $ x { label = str }) formInput }
  I.OptionsForeign (I.Attrs x) formInput fd ->
    i { inputType = I.OptionsForeign (I.Attrs $ x { label = str }) formInput fd }

setInputHelpText :: Maybe String -> InputConfig' -> InputConfig'
setInputHelpText str (InputConfig i) = InputConfig $ case i.inputType of
  I.Text (I.Attrs x) formInput ->
    i { inputType = I.Text (I.Attrs $ x { helpText = str }) formInput }
  I.TextArea (I.Attrs x) formInput ->
    i { inputType = I.TextArea (I.Attrs $ x { helpText = str }) formInput }
  I.Number (I.Attrs x) formInput ->
    i { inputType = I.Number (I.Attrs $ x { helpText = str }) formInput }
  I.DateTimeInput (I.Attrs x) formInput ->
    i { inputType = I.DateTimeInput (I.Attrs $ x { helpText = str }) formInput }
  I.Options (I.Attrs x) formInput ->
    i { inputType = I.Options (I.Attrs $ x { helpText = str }) formInput }
  I.OptionsForeign (I.Attrs x) formInput fd ->
    i { inputType = I.OptionsForeign (I.Attrs $ x { helpText = str }) formInput fd }

setOptionText :: Int -> String -> InputConfig' -> InputConfig'
setOptionText index str (InputConfig i) = InputConfig $ case i.inputType of
  I.Options attrs (I.FormInput f@{ input }) ->
    let new = case input of
          I.Radio arr ->
            I.Radio $ fromMaybe arr $ updateAt index (I.TextItem str) arr
          I.Checkbox arr ->
            I.Checkbox $ fromMaybe arr $ updateAt index (I.TextItem str) arr
          I.Dropdown arr ->
            I.Dropdown $ fromMaybe arr $ updateAt index str arr
     in i { inputType = I.Options attrs $ I.FormInput (f { input = new }) }
  otherwise -> i

setForeignUrl :: String -> InputConfig' -> InputConfig'
setForeignUrl str (InputConfig i) = InputConfig $ case i.inputType of
  I.OptionsForeign attrs fi (I.ForeignData _ akeys ikeys) ->
    i { inputType = I.OptionsForeign attrs fi $ I.ForeignData str akeys ikeys }
  otherwise -> i

setForeignArrayKeys :: String -> InputConfig' -> InputConfig'
setForeignArrayKeys str (InputConfig i) = InputConfig $ case i.inputType of
  I.OptionsForeign attrs fi (I.ForeignData url _ ikeys) ->
    i { inputType = I.OptionsForeign attrs fi
      $ I.ForeignData url (readArrayKeys str) ikeys }
  otherwise -> i

setForeignItemKeys :: String -> InputConfig' -> InputConfig'
setForeignItemKeys str (InputConfig i) = InputConfig $ case i.inputType of
  I.OptionsForeign attrs fi (I.ForeignData url akeys _) ->
    i { inputType = I.OptionsForeign attrs fi
      $ I.ForeignData url akeys (readItemKeys str) }
  otherwise -> i

insertOption :: String -> InputConfig' -> InputConfig'
insertOption str (InputConfig i) = InputConfig $ case i.inputType of
  I.Options attrs (I.FormInput f@{ input }) ->
    let new = case input of
          I.Radio arr ->
            I.Radio $ arr <> [ I.TextItem str ]
          I.Checkbox arr ->
            I.Checkbox $ arr <> [ I.TextItem str ]
          I.Dropdown arr ->
            I.Dropdown $ arr <> [ str ]
     in i { inputType = I.Options attrs $ I.FormInput (f { input = new }) }
  I.OptionsForeign attrs (I.FormInput f@{ input }) fdata ->
    let new = case input of
          I.Radio arr ->
            I.Radio $ arr <> [ str ]
          I.Checkbox arr ->
            I.Checkbox $ arr <> [ str ]
          I.Dropdown arr ->
            I.Dropdown $ arr <> [ str ]
     in i { inputType = I.OptionsForeign attrs (I.FormInput (f { input = new })) fdata }
  otherwise -> i

removeOption :: Int -> InputConfig' -> InputConfig'
removeOption index (InputConfig i) = InputConfig $ case i.inputType of
  I.Options attrs (I.FormInput f@{ input }) ->
    let new = case input of
          I.Radio arr ->
            I.Radio $ fromMaybe arr $ deleteAt index arr
          I.Checkbox arr ->
            I.Checkbox $ fromMaybe arr $ deleteAt index arr
          I.Dropdown arr ->
            I.Dropdown $ fromMaybe arr $ deleteAt index arr
     in i { inputType = I.Options attrs $ I.FormInput (f { input = new }) }
  I.OptionsForeign attrs (I.FormInput f@{ input }) fdata ->
    let new = case input of
          I.Radio arr ->
            I.Radio (fromMaybe arr $ deleteAt index arr)
          I.Checkbox arr ->
            I.Checkbox (fromMaybe arr $ deleteAt index arr)
          I.Dropdown arr ->
            I.Dropdown (fromMaybe arr $ deleteAt index arr)
     in i { inputType = I.OptionsForeign attrs (I.FormInput (f { input = new })) fdata }
  otherwise -> i

insertValidation :: V.Validate -> InputConfig' -> InputConfig'
insertValidation v (InputConfig i) = InputConfig $
  i { validations = v : filter ((/=) v) i.validations }

removeValidation :: V.Validate -> InputConfig' -> InputConfig'
removeValidation v (InputConfig i) = InputConfig $
  i { validations = filter ((/=) v) i.validations }

foldrWithKey :: ∀ k a b. (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z =
  foldr (uncurry f) z
  <<< (Map.toUnfoldable :: Map k a -> Array (Tuple k a))

getFormConfig :: FormId -> Aff (Either String FormConfig')
getFormConfig formId =
  decodeJson
  <<< _.response
  <$> get Response.json ("http://localhost:3000/forms/" <> (show <<< unwrap) formId)

saveFormConfig :: FormConfig' -> Aff (Either String FormConfig')
saveFormConfig config =
  decodeJson <<< _.response <$> post Response.json uri (Request.json $ encodeJson config)
  where
    uri = "http://localhost:3000/forms"

-- A helper function that can be used to initialize the form
-- builder.
initialize
  :: ∀ m
   . MonadState (Form.State V.Validate I.AppInput R.Relate) m
  => MonadAff m
  => m Unit
initialize = do
  (refs :: Array (Tuple InputRef I.AppInput))
    <- H.gets (Map.toUnfoldable <<< _.form)
  flip traverse_ refs $ \(Tuple ref input) -> do
    case input of
      I.OptionsForeign attrs formInput (I.ForeignData url akeys ikeys) -> do
        arr <- do
          data_ <- H.liftAff $ fetch akeys ikeys url ""
          case data_ of
            Success xs -> pure xs
            Failure err -> do
               H.liftEffect $ Console.log err
               pure []
            otherwise -> pure $ withDefault [] data_

        -- Do something with array of strings...
        flip traverse_ arr $ \str ->
          H.modify_ \st ->
            st { form = updateForm st.form ref (insertForeignOption str) }

      otherwise -> pure unit


insertForeignOption  :: String -> I.AppInput -> I.AppInput
insertForeignOption str i = case i of
  I.OptionsForeign attrs (I.FormInput f@{ input }) fdata ->
    let new = case input of
          I.Radio arr ->
            I.Radio $ arr <> [ str ]
          I.Checkbox arr ->
            I.Checkbox $ arr <> [ str ]
          I.Dropdown arr ->
            I.Dropdown $ arr <> [ str ]
     in I.OptionsForeign attrs (I.FormInput (f { input = new })) fdata
  otherwise -> i
