module Lynx.Component where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as Console
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Graph (Edge(..), Form, InputConfig, InputRef)

data Query a
  = UpdateValue InputRef String a
  | Blur InputRef a
  | Submit a

type Message = Void

type State =
  { config :: Form
  , form :: Map.Map InputRef String
  }

component :: ∀ eff m
   . MonadAff (console :: CONSOLE | eff) m
  => Form
  -> H.Component HH.HTML Query Unit Message m
component form =
  H.component
    { initialState: const { config: form, form: (const "") <$> form.fields }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      UpdateValue ref str a -> a <$ do
        H.modify \st -> st { form = Map.insert ref str st.form }

      Blur ref a -> a <$ do
        runEdges ref

      Submit a -> a <$ do
        refs <- H.gets (Map.keys <<< _.form)
        traverse_ runEdges refs

    render :: State -> H.ComponentHTML Query
    render st = HH.div_
      [ HH.div_ $ renderInput st <$> Map.toAscUnfoldable st.config.fields
      , HH.button
          [ HE.onClick (HE.input_ Submit) ]
          [ HH.text "Submit" ]
      ]

    renderInput :: State -> Tuple InputRef InputConfig -> H.ComponentHTML Query
    renderInput st (Tuple ref { name, "type": t }) =
      let attr = HP.attr (HH.AttrName "data-inputref") (show $ unwrap ref)
       in case t of
        _ ->
          HH.div_
            [ HH.text name
            , HH.input
                [ attr
                , HE.onValueInput $ HE.input $ UpdateValue ref
                , HE.onBlur $ HE.input_ $ Blur ref
                , HP.value $ fromMaybe "field not found in form! nooo" $ Map.lookup ref st.form
                ]
            ]

runEdges :: ∀ eff m
  . MonadAff (console :: CONSOLE | eff) m
 => InputRef
 -> H.ComponentDSL State Query Message m Unit
runEdges ref = do
  st <- H.get
  case Map.lookup ref st.config.fields of
    Nothing -> pure unit
    Just config -> do
      traverse_ (handleEdge ref) config.edges
      pure unit

handleEdge :: ∀ eff m
  . MonadAff (console :: CONSOLE | eff) m
 => InputRef
 -> Edge
 -> H.ComponentDSL State Query Message m Unit
handleEdge refA = case _ of
  MustEqual refB -> do
    st <- H.get
    let equal = do
          v0 <- Map.lookup refA st.form
          v1 <- Map.lookup refB st.form
          pure $ v0 == v1
    case equal of
      Just true -> do
        H.liftAff $ Console.log $ show refA <> " is equal to " <> show refB
      otherwise -> do
        H.liftAff $ Console.log $ show refA <> " is NOT equal to " <> show refB
    pure unit

  Clear refB -> do
    H.modify \st -> st { form = Map.insert refB "" st.form }
    H.liftAff $ Console.logShow $ "Deleted " <> show refB
    pure unit

