module Lynx.Components.Form where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as Console
import Control.Monad.State.Class (class MonadState)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Array ((:))
import Data.Array (fromFoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lynx.Data.Graph (FormConfig(..), InputConfig(..), InputRef, FormId(..))
import Network.HTTP.Affjax (AJAX, get)

data Query v i r a
  = UpdateValue InputRef String a
  | Blur InputRef a
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
    -> String
    -> Either String String
  , handleRelate
    :: r
    -> InputRef
    -> H.ComponentDSL (State v i r) (Query v i r) Message (Aff eff) Unit
  }

type Input v i r = Either (FormConfig v i r) FormId

data Message

type State v i r =
  { config       :: FormConfig v i r
  , form         :: Map InputRef String
  , selectedForm :: FormId
  , fromDB       :: Boolean
  }

type Effects eff =
  ( console :: CONSOLE
  , ajax :: AJAX
  | eff )

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
        { config
        , form: Map.empty
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
        H.modify _ { config = config }
        pure a
      Receiver (Right _) a -> pure a

      GetForm i a -> a <$ do
        (res :: Json) <- H.liftAff $
           _.response <$> get ("http://localhost:3000/forms/" <> (show $ unwrap i))
        case decodeJson res of
          Left s -> H.liftAff $ Console.log s *> pure a
          Right form -> do
             H.modify \st -> st
               { config = form
               , form = const "" <$> _.inputs (unwrap form)
               }
             pure a

      UpdateValue ref str a -> a <$ do
        H.modify \st -> st { form = Map.insert ref str st.form }

      Blur ref a -> a <$ do
        runRelations ref handleRelate
        runValidations ref handleValidate

      Submit a -> a <$ do
        refs <- H.gets (Map.keys <<< _.form)
        traverse_ (flip runRelations $ handleRelate) refs

    render :: State v i r -> H.ComponentHTML (Query v i r)
    render st = HH.div_
      [ HH.div_
        $ Array.fromFoldable
        $ handleInput st <$> Map.keys (_.inputs $ unwrap st.config)
      , HH.button
          [ HE.onClick (HE.input_ Submit) ]
          [ HH.text "Submit" ]
      ]

-- Attempt to use the provided validation helper to run on the form validations
runValidations :: ∀ eff v i r m
  . MonadAff (Effects eff) m
 => InputRef
 -> (v -> String -> Either String String)
 -> H.ComponentDSL (State v i r) (Query v i r) Message m Unit
runValidations ref validate = do
  st <- H.get
  case Map.lookup ref st.form of
    Nothing -> pure unit
    Just val -> case Map.lookup ref (_.inputs $ unwrap st.config) of
      Nothing -> pure unit
      Just (InputConfig config) -> do
        let successive (Left str) arr = str : arr
            successive _ arr = arr
            res =
              case foldr (\v arr -> successive (validate v val) arr) [] config.validations of
                [] -> Right $ "Valid: " <> val
                arr -> Left arr
        _ <- H.liftAff $ Console.logShow res
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
    Nothing -> pure unit
    Just (InputConfig config) -> do
      traverse_ (flip runRelation $ ref) config.relations
      pure unit
