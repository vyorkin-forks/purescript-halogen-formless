module Example.ExternalComponents.Types where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form, User)
import Formless as Formless
import Ocelot.Components.Typeahead as TA

----------
-- Component

-- | This component manages several typeahead components, plus
-- | Formless. Because of the external components, it needs its
-- | own reset query to clear those components when Formless
-- | has been reset by the user.
data Query a
  = HandleFormless (Formless.Message Query Form User) a
  | HandleTypeahead Slot (TA.Message Query String) a
  | Reset a

type State = Unit

-- | Now we can create _this_ component's child query and child slot pairing.
type Slots =
  ( formless :: Formless.Slot Query FSlots Form User Aff Unit )
_formless = SProxy :: SProxy "formless"

----------
-- Formless

type FSlots =
  ( typeahead :: TA.Slot Query () String String Aff Slot )

_typeahead = SProxy :: SProxy "typeahead"

data Slot
  = EmailTypeahead
  | WhiskeyTypeahead
  | LanguageTypeahead

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot
