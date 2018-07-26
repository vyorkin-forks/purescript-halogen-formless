module Example.Polyform.Types where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.Polyform.Spec (Form, User)
import Formless as Formless

----------
-- Component

-- | This component just handles Formless
data Query a
  = HandleFormless (Formless.Message Query Form User) a

type State = Unit

type Slots =
  ( formless :: Formless.Slot Query () Form User Aff Unit )

_formless = SProxy :: SProxy "formless"
