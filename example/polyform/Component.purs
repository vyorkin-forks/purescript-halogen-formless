module Example.Polyform.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.Polyform.RenderForm (formless)
import Example.Polyform.Spec (User, formSpec, submitter, validator)
import Example.Polyform.Types (Query(..), Slots, State, _formless)
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css)
import Record (delete)

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  render :: State -> H.ComponentHTML Query Slots Aff
  render st =
    HH.div
    [ css "flex-1 container p-12" ]
    [ Format.heading_
      [ HH.text "Formless" ]
    , Format.subHeading_
      [ HH.text "A form using the composable validation toolkit Polyform." ]
    , Format.p_
      [ HH.text $
          "In Formless, you can use whatever validation library you prefer. The component provides "
          <> "helpers for working with "
      , HH.code_ [ HH.text "purescript-polyform" ]
      , HH.text "or the canonical "
      , HH.code_ [ HH.text "purescript-validation" ]
      , HH.text $
          " library, but you're not obligated to use either. This form demonstrates using Polyform "
          <> "as the underlying validation library, whereas the other examples use "
      , HH.code_ [ HH.text "purescript-validation" ]
      , HH.text "."
      ]
    , Format.p_
      [ HH.text $
        "Try watching the console output as you fill out the form, and notice how you can only reset the "
        <> "form if it is in a dirty state, and can only submit the form if it is valid."
      ]
    , HH.slot
        _formless
        unit
        Formless.component
        { formSpec
        , validator
        , submitter
        , render: formless
        }
        (HE.input HandleFormless)
    ]

  eval :: Query ~> H.HalogenM State Query Slots Void Aff
  eval = case _ of
    HandleFormless m a -> case m of
      Formless.Emit q -> eval q *> pure a
      Formless.Submitted user -> do
        H.liftEffect $ Console.log $ show (user :: User)
        pure a
      Formless.Changed fstate -> do
        H.liftEffect $ Console.log $ show $ delete (SProxy :: SProxy "form") fstate
        pure a
