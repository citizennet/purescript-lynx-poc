module Lynx.Component where

import Prelude

import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as Console
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
import Lynx.Graph (FormConfig(..), InputConfig(..), InputRef, FormId(..))
import Network.HTTP.Affjax (AJAX, get)

data Query a
  = UpdateValue InputRef String a
  | Blur InputRef a
  | Submit a
  | GetForm FormId a

data Message

type State v i r =
  { config       :: FormConfig v i r
  , form         :: Map InputRef String
  , selectedForm :: FormId
  }

type Effects eff =
  ( console :: CONSOLE
  , ajax :: AJAX
  | eff )

component :: ∀ eff v i r
   . DecodeJson v
  => DecodeJson i
  => DecodeJson r
  => (v -> String -> Either String String)
  -> (State v i r -> InputRef -> H.ComponentHTML Query)
  -> (r -> InputRef -> H.ComponentDSL (State v i r) Query Message (Aff (Effects eff)) Unit)
  -> H.Component HH.HTML Query Unit Message (Aff (Effects eff))
component handleValidation handleInput handleRelations =
  H.lifecycleComponent
    { initialState:
        const
          { config: FormConfig
            { id: FormId 0
            , supply: 0
            , inputs: Map.empty
            }
          , form: Map.empty
          , selectedForm: FormId 0
          }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action (GetForm $ FormId 0)
    , finalizer: Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL (State v i r) Query Message (Aff (Effects eff))
    eval = case _ of
      GetForm i a -> a <$ do
        (res :: Json) <- H.liftAff $
           _.response <$> get ("http://localhost:3000/forms/" <> show 0)
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
        runRelations ref handleRelations
        runValidations ref handleValidation

      Submit a -> a <$ do
        refs <- H.gets (Map.keys <<< _.form)
        traverse_ (flip runRelations $ handleRelations) refs

    render :: State v i r -> H.ComponentHTML Query
    render st = HH.div_
      [ HH.div_ $ Array.fromFoldable $ (handleInput st) <$> Map.keys (_.inputs $ unwrap st.config)
      , HH.button
          [ HE.onClick (HE.input_ Submit) ]
          [ HH.text "Submit" ]
      ]

-- Attempt to use the provided validation helper to run on the form validations
runValidations :: ∀ eff v i r m
  . MonadAff (Effects eff) m
 => InputRef
 -> (v -> String -> Either String String)
 -> H.ComponentDSL (State v i r) Query Message m Unit
runValidations ref validate = do
  st <- H.get
  case Map.lookup ref st.form of
    Nothing -> pure unit
    Just val -> case Map.lookup ref (_.inputs $ unwrap st.config) of
      Nothing -> pure unit
      Just (InputConfig config) -> do
        let successive (Left str) arr = str : arr
            successive _ arr = arr
            res = case foldr (\v arr -> successive (validate v val) arr) [] config.validations of
              [] -> Right $ "Valid: " <> val
              arr -> Left arr
        _ <- H.liftAff $ Console.logShow res
        pure unit

-- Attempt to use the provided relations helper to run on form relations
runRelations :: ∀ eff v i r m
  . MonadAff (Effects eff) m
 => InputRef
 -> (r -> InputRef -> H.ComponentDSL (State v i r) Query Message m Unit)
 -> H.ComponentDSL (State v i r) Query Message m Unit
runRelations ref runRelation = do
  st <- H.get
  case Map.lookup ref (_.inputs $ unwrap st.config) of
    Nothing -> pure unit
    Just (InputConfig config) -> do
      traverse_ (flip runRelation $ ref) config.relations
      pure unit
