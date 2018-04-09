module App.Forms.Dynamic.Signup where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.State (class MonadState, get, modify)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (:=), (~>), (.?))
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (input, input_, onBlur, onValueInput) as HE
import Halogen.HTML.Properties as HP
import Lynx.Dynamic.Component as Component
import Lynx.Dynamic.Graph (FormConfig, FormId(..), InputConfig(..), InputRef, input, relate, runFormBuilder, validate)

type SignupForm = FormConfig SignupValidate SignupInput SignupRelation

-- A user signup form
form :: SignupForm
form = runFormBuilder (FormId 0) do
  user  <- input (Text { label: "Username" })
    >>= validate NonEmpty
  pass1 <- input (Text { label: "Password 1" })
    >>= validate (InRange 5 15)
    >>= relate (MustEqual user)
  pass2 <- input (Text { label: "Password 2" })
    >>= validate (InRange 5 15)
    >>= relate (Clear pass1)
  pure =<< get

form2 :: SignupForm
form2 = runFormBuilder (FormId 1) do
  name <- input (Text { label: "Name" })
    >>= validate NonEmpty
  email <- input (Text { label: "Email" })
    >>= validate NonEmpty
    >>= validate (InRange 5 15)
  count <- input (Text { label: "Count" })
    >>= validate NonEmpty
  size <- input (Text { label: "Size" })
  count2 <- input (Text { label: "Count 2" })
    >>= validate NonEmpty
    >>= validate (InRange 0 2)
    >>= relate (MustEqual count)

  -- Can add relations later on, not necessarily in-place
  _ <- relate (Clear count) size
  _ <- validate NonEmpty size
  pure =<< get

----------
-- A form, in parts

data SignupInput
  = Text { label :: String }

instance encodeJsonSignupInput :: EncodeJson SignupInput where
  encodeJson = case _ of
    Text { label } -> do
      "inputType" := "Text"
      ~> "label" := label
      ~> jsonEmptyObject

instance decodeJsonSignupInput :: DecodeJson SignupInput where
  decodeJson json = do
    obj <- decodeJson json
    str <- obj .? "inputType"
    input <- case str of
      "Text" -> do
        label <- obj .? "label"
        pure $ Text { label }
      otherwise ->
        Left $ "No case written to decode:\n" <> "  " <> str <> "\n"
    pure input

data SignupRelation
  = MustEqual InputRef
  | Clear InputRef

instance encodeJsonSignupRelation :: EncodeJson SignupRelation where
  encodeJson =
    let update str i = do
          "relationType" := str
          ~> "ref" := i
          ~> jsonEmptyObject
     in case _ of
      MustEqual i -> update "MustEqual" i
      Clear i -> update "Clear" i

instance decodeJsonSignupRelation :: DecodeJson SignupRelation where
  decodeJson json = do
    obj <- decodeJson json
    ref <- obj .? "ref"
    str <- obj .? "relationType"
    relation <- case str of
      "MustEqual" -> pure $ MustEqual ref
      "Clear" -> pure $ Clear ref
      otherwise ->
        Left $ "No case written to decode:\n" <> "  " <> str <> "\n"
    pure relation

data SignupValidate
  = InRange Int Int
  | NonEmpty

instance encodeJsonSignupValidate :: EncodeJson SignupValidate where
  encodeJson = case _ of
    InRange i0 i1 -> do
      "validationType" := "InRange"
      ~> "from" := i0
      ~> "to" := i1
      ~> jsonEmptyObject
    NonEmpty -> do
      "validationType" := "NonEmpty"
      ~> jsonEmptyObject

instance decodeJsonSignupValidate :: DecodeJson SignupValidate where
  decodeJson json = do
    obj <- decodeJson json
    str <- obj .? "validationType"
    validate <- case str of
      "InRange" -> do
        from <- obj .? "from"
        to <- obj .? "to"
        pure $ InRange from to
      "NonEmpty" -> pure NonEmpty
      otherwise ->
        Left $ "No case written to decode:\n" <> "  " <> str <> "\n"
    pure validate


---------
-- HELPERS

-- A function to run user validation
handleValidation :: SignupValidate -> String -> Either String String
handleValidation v str = case v of
  NonEmpty ->
    if String.null str
      then Left "Field cannot be empty"
      else Right str
  InRange i0 i1 ->
    if String.length str < i0 || String.length str > i1
      then Left $ "Field must be between " <> show i0 <> " and " <> show i1 <> " characters."
      else Right str

-- A function to run user relations
handleRelation :: ∀ eff m
   . MonadState (Component.State SignupValidate SignupInput SignupRelation) m
  => MonadAff (Component.Effects eff) m
  => SignupRelation
  -> InputRef
  -> m Unit
handleRelation relation refA = case relation of
  MustEqual refB -> do
    st <- get
    let equal = do
          v0 <- Map.lookup refA st.form
          v1 <- Map.lookup refB st.form
          pure $ v0 == v1
    case equal of
      Just true -> pure unit
      otherwise -> do
        liftAff $ Console.log $ show refA <> " is NOT equal to " <> show refB
        pure unit

  Clear refB -> do
    modify \st -> st { form = Map.insert refB "" st.form }
    liftAff $ Console.logShow $ "Deleted " <> show refB
    pure unit

-- A function to render user inputs
renderInput
  :: Component.State SignupValidate SignupInput SignupRelation
  -> InputRef
  -> H.ComponentHTML Component.Query
renderInput st ref =
  let attr = HP.attr (HH.AttrName "data-inputref") (show $ unwrap ref)
      config = Map.lookup ref (_.inputs $ unwrap st.config)
   in case config of
        Just (InputConfig { inputType }) -> case inputType of
          Text { label } ->
            HH.div_
              [ HH.text label
              , HH.input
                  [ attr
                  , HE.onValueInput $ HE.input $ Component.UpdateValue ref
                  , HE.onBlur $ HE.input_ $ Component.Blur ref
                  , HP.value $ fromMaybe "field not found in form! nooo" $ Map.lookup ref st.form
                  ]
              ]
        otherwise -> HH.div_ []
