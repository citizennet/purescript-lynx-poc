module Lynx.Components.Form where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as Console
import Control.Monad.State.Class (class MonadState)
import DOM (DOM)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Array (fromFoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lynx.Data.Graph (FormConfig(..), InputConfig(..), InputRef, FormId(..))
import Network.HTTP.Affjax (AJAX, get)
import Ocelot.Components.Typeahead as TA

-- `i` is the Input type
data Query v i r a
  = HandleTypeahead InputRef (TA.Message (Query v i r) String) a
  | UpdateValue InputRef (i -> i) a
  | Blur InputRef (i -> i) a -- reset function
  | GetForm FormId a
  | Submit a
  | Initialize a
  | Receiver (Input v i r) a

type ComponentConfig v i r eff m =
  { handleInput
    :: State v i r
    -> InputRef
    -> ComponentHTML v i r eff m
  , handleValidate
    :: v
    -> i
    -> i
  , handleRelate
    :: r
    -> InputRef
    -> ComponentDSL v i r eff m Unit
  , initialize
    :: ComponentDSL v i r eff m Unit
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
  , dom :: DOM
  , avar :: AVAR
  | eff )

inputAp :: ∀ i. (i -> i) -> InputRef -> Map InputRef i -> Map InputRef i
inputAp f ref orig
  = fromMaybe orig new
  where
    new = do
      type' <- Map.lookup ref orig
      pure $ Map.insert ref (f type') orig


----------
-- Component Types

type Component v i r m
  = H.Component HH.HTML (Query v i r) (Input v i r) Message m

type ComponentHTML v i r eff m
  = H.ParentHTML (Query v i r) (ChildQuery v i r eff m) ChildSlot m

type ComponentDSL v i r eff m
  = H.ParentDSL (State v i r) (Query v i r) (ChildQuery v i r eff m) ChildSlot Message m

type ChildSlot = String
type ChildQuery v i r eff m
  = TA.Query (Query v i r) String String eff m

component :: ∀ v i r eff m
   . DecodeJson v
  => DecodeJson i
  => DecodeJson r
  => MonadAff (Effects eff) m
  => ComponentConfig v i r (Effects eff) m
  -> Component v i r m
component { handleInput, handleValidate, handleRelate, initialize } =
  H.lifecycleParentComponent
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
        , selectedForm: FormId (-1)
        , fromDB: false
        }
      Right formId ->
        { config: FormConfig
          { id: FormId (-1)
          , supply: 0
          , inputs: Map.empty
          }
        , form: Map.empty
        , selectedForm: formId
        , fromDB: true
        }

    eval
      :: Query v i r
      ~> ComponentDSL v i r (Effects eff) m
    eval = case _ of
      Initialize a -> a <$ do
        state <- H.get
        if state.fromDB
          then eval (GetForm state.selectedForm a) *> initialize
          else initialize

      Receiver (Left config) a -> do
        H.modify _
          { config = config
          , form = (_.inputType <<< unwrap) <$> (_.inputs <<< unwrap $ config)
          }
        initialize
        pure a
      Receiver (Right _) a -> pure a

      GetForm i a -> a <$ do
        (res :: Json) <- H.liftAff $
           _.response <$> get ("http://localhost:3000/forms/" <> (show $ unwrap i))
        case decodeJson res of
          Left s -> H.liftAff $ Console.log s *> pure a
          Right config -> do
            H.modify _
              { config = config
              , form = (_.inputType <<< unwrap) <$> (_.inputs <<< unwrap $ config)
              }
            pure a

      HandleTypeahead ref m a -> case m of
        TA.Emit q -> eval q *> pure a
        TA.Searched _ -> pure a
        TA.SelectionsChanged _ _ items -> do
          -- Update the input with the new array
          let arr = case items of
               TA.Many xs -> xs
               TA.Limit _ xs -> xs
               TA.One x -> maybe [] pure x
          pure a
        TA.VisibilityChanged v -> pure a

      UpdateValue ref func a -> a <$ do
        H.modify \st -> st { form = inputAp func ref st.form }

      Blur ref f a -> do
        runRelations ref handleRelate
        runValidations ref f handleValidate
        pure a

      Submit a -> a <$ do
        refs <- H.gets (Map.keys <<< _.form)
        traverse_ (flip runRelations $ handleRelate) refs

    render
      :: State v i r
      -> ComponentHTML v i r (Effects eff) m
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
 -> H.ParentDSL
      (State v i r)
      (Query v i r)
      (ChildQuery v i r (Effects eff) m)
      ChildSlot
      Message
      m
      Unit
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
