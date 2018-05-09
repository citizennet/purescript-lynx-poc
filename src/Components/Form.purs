module Lynx.Components.Form where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as Console
import Control.Monad.State.Class (class MonadState)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Array (fromFoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Debug.Trace (spy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lynx.Data.Graph (FormConfig(..), InputConfig(..), InputRef, FormId(..))
import Network.HTTP.Affjax (AJAX, get)

-- `i` is the Input type
data Query v i r a
  = UpdateValue InputRef (i -> i) a
  | Blur InputRef (i -> i) a -- reset function
  | Submit a
  | GetForm FormId a
  | Initialize a
  | Receiver (Input v i r) a

type ComponentConfig v i r eff =
  { handleInput
    :: State v i r
    -> InputRef
    -> H.ComponentHTML (Query v i r)
  , handleValidate
    :: v
    -> i
    -> i
  , handleRelate
    :: r
    -> InputRef
    -> H.ComponentDSL (State v i r) (Query v i r) Message (Aff eff) Unit
  }

type Input v i r = Either (FormConfig v i r) FormId

data Message

type State v i r =
  { config       :: FormConfig v i r
  , form         :: Map InputRef i
  , selectedForm :: FormId
  , fromDB       :: Boolean
  }

type Effects eff =
  ( console :: CONSOLE
  , ajax :: AJAX
  | eff )

inputAp :: ∀ i. (i -> i) -> InputRef -> Map InputRef i -> Map InputRef i
inputAp f ref orig
  = fromMaybe orig new
  where
    new = do
      type' <- Map.lookup ref orig
      pure $ Map.insert ref (spy $ f type') orig

component :: ∀ v i r eff
   . DecodeJson v
  => DecodeJson i
  => DecodeJson r
  => ComponentConfig v i r (Effects eff)
  -> H.Component HH.HTML (Query v i r) (Input v i r) Message (Aff (Effects eff))
component { handleInput, handleValidate, handleRelate } =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Receiver
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where
    initialState = case _ of
      Left config ->
        { form: (_.inputType <<< unwrap) <$> (_.inputs <<< unwrap $ config)
        , config
        , selectedForm: FormId 0
        , fromDB: false
        }
      Right formId ->
        { config: FormConfig
          { id: FormId 0
          , supply: 0
          , inputs: Map.empty
          }
        , form: Map.empty
        , selectedForm: formId
        , fromDB: true
        }

    eval
      :: Query v i r
      ~> H.ComponentDSL (State v i r) (Query v i r) Message (Aff (Effects eff))
    eval = case _ of
      Initialize a -> do
        state <- H.get
        if state.fromDB
          then eval (GetForm state.selectedForm a)
          else pure a

      Receiver (Left config) a -> do
        H.modify _
          { config = config
          , form = (_.inputType <<< unwrap) <$> (_.inputs <<< unwrap $ config)
          }
        pure a
      Receiver (Right _) a -> pure a

      GetForm i a -> a <$ do
        (res :: Json) <- H.liftAff $
           _.response <$> get ("http://localhost:3000/forms/" <> (show $ unwrap i))
        case decodeJson res of
          Left s -> H.liftAff $ Console.log s *> pure a
          Right form -> do
             H.modify _ { form = form }
             pure a

      UpdateValue ref func a -> a <$ do
        H.modify \st -> st { form = inputAp func ref st.form }

      Blur ref f a -> a <$ do
        runRelations ref handleRelate
        runValidations ref f handleValidate

      Submit a -> a <$ do
        refs <- H.gets (Map.keys <<< _.form)
        traverse_ (flip runRelations $ handleRelate) refs

    render :: State v i r -> H.ComponentHTML (Query v i r)
    render st = HH.div_
      [ HH.div_
        $ Array.fromFoldable
        $ handleInput st <$> Map.keys st.form
      , HH.button
          [ HE.onClick (HE.input_ Submit) ]
          [ HH.text "Submit" ]
      ]

-- Attempt to use the provided validation helper to run on the form validations
runValidations :: ∀ eff v i r m
  . MonadAff (Effects eff) m
 => InputRef
 -> (i -> i) -- reset
 -> (v -> i -> i)
 -> H.ComponentDSL (State v i r) (Query v i r) Message m Unit
runValidations ref reset validate = do
  st <- H.get
  case Map.lookup ref (_.inputs $ unwrap st.config) of
    Nothing -> do
       H.liftAff $ Console.log $ "Could not find ref " <> show ref <> " in config."
       pure unit
    Just (InputConfig config) -> case Map.lookup ref st.form of
      Nothing -> do
        H.liftAff $ Console.log $ "Could not find ref " <> show ref <> " in form."
        pure unit
      Just input -> do
        let type' = foldr (\v i -> validate v i) (reset input) config.validations
        H.modify _ { form = inputAp (const type') ref st.form }
        pure unit

-- Attempt to use the provided relations helper to run on form relations
runRelations :: ∀ v i r m
  . MonadState (State v i r) m
 => InputRef
 -> (r -> InputRef -> m Unit)
 -> m Unit
runRelations ref runRelation = do
  st <- H.get
  case Map.lookup ref (_.inputs $ unwrap st.config) of
    Nothing ->
       pure unit
    Just (InputConfig config) -> do
      traverse_ (flip runRelation $ ref) config.relations
      pure unit
