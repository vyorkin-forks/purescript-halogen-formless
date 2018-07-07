module Example.ExternalComponents.RenderFormless where

import Prelude

import Data.Array (reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form, _email, _name)
import Example.ExternalComponents.Types (FCQ, FCS, Query(..))
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TA.Input
import Record as Record

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type.
renderFormless
  :: Formless.State Form
  -> Formless.HTML Query FCQ FCS Form Aff
renderFormless state =
  HH.div_
    [ Format.subHeading_
      [ HH.text "Fill out the form:" ]
    , renderName state
    , renderEmail state
    ]

----------
-- Helpers

-- | A helper function to render a form text input
renderName :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderName state =
  let field = unwrap $ Record.get sym $ unwrap state.form
      sym = _name
      label = "Name"
   in
      HH.div_
        [ HH.code_
          [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ field.input ]
        , HH.br_
        , if field.touched
            then HH.text "-- changed since form initialization --"
            else HH.text ""
        , FormField.field_
            { label: label
            , helpText: Just "Write your name."
            , error: case field.result of
                Just (Left str) -> Just str
                _ -> Nothing
            , inputId: label
            }
            [ Input.input
              [ HP.placeholder "Dale"
              , HP.id_ label
              , HP.value field.input
              , Formless.onBlurWith sym
              , Formless.onValueInputWith sym
              ]
            ]
        ]

renderEmail :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderEmail state =
  let field = unwrap $ Record.get sym $ unwrap state.form
      sym = _email
      label = "Email"
   in
      HH.div_
        [ HH.code_
          [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ field.input ]
        , HH.br_
        , if field.touched
            then HH.text "-- changed since form initialization --"
            else HH.text ""
        , FormField.field_
            { label: label
            , helpText: Just "Write your name."
            , error: case field.result of
                Just (Left str) -> Just str
                _ -> Nothing
            , inputId: label
            }
            [ HH.slot
                unit
                TA.component
                ( TA.Input.defSingle
                  [ HP.placeholder "Search email addresses..." ]
                  [ "not@anemail.org"
                  , "snail@utopia.snailutopia"
                  , "blue@jordans@blordans.pordens"
                  , "yea_that_won't_work@email.com"
                  , "standard@email.com"
                  ]
                  TA.Input.renderItemString
                )
                ( HE.input (Formless.Raise <<< H.action <<< HandleTypeahead) )
            ]
        ]