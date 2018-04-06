module Lynx.Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as Console
import Data.Array ((:))
import Data.Array (fromFoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lynx.Graph (FormConfig, InputConfig(..), InputRef)

data Query a
  = UpdateValue InputRef String a
  | Blur InputRef a
  | Submit a

data Message

type State v i r =
  { config :: FormConfig v i r
  , form :: Map.Map InputRef String
  }

type Effects eff =
  ( console :: CONSOLE
  | eff )

component :: ∀ eff v i r
   . FormConfig v i r
  -> (v -> String -> Either String String)
  -> (State v i r -> InputRef -> H.ComponentHTML Query)
  -> (r -> InputRef -> H.ComponentDSL (State v i r) Query Message (Aff (Effects eff)) Unit)
  -> H.Component HH.HTML Query Unit Message (Aff (Effects eff))
component form handleValidation handleInput handleRelations =
  H.component
    { initialState: const { config: form, form: (const "") <$> (_.inputs $ unwrap form) }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL (State v i r) Query Message (Aff (Effects eff))
    eval = case _ of
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
