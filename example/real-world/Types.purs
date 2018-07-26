module Example.RealWorld.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.RealWorld.Data.Group (Admin, Group, GroupForm)
import Example.RealWorld.Data.Options (Metric, Options, OptionsForm)
import Formless as Formless
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.Components.Typeahead as TA

----------
-- Component

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleGroupForm (Formless.Message Query GroupForm Group) a
  | HandleOptionsForm (Formless.Message Query OptionsForm Options) a
  | HandleGroupTypeahead GroupTASlot (TA.Message Query String) a
  | HandleAdminDropdown (Dropdown.Message Query Admin) a
  | HandleMetricDropdown (Dropdown.Message Query Metric) a
  | Select Tab a
  | Reset a
  | Submit a

-- | We'll keep track of both form errors so we can show them in tabs
-- | and our ultimate goal is to result in a Group we can send to the
-- | server.
type State =
  { focus :: Tab                 -- Which tab is the user on?
  , groupFormErrors :: Int       -- Count of the group form errors
  , groupFormDirty :: Boolean    -- Is the group form in a dirty state?
  , optionsFormErrors :: Int     -- Count of the options form errors
  , optionsFormDirty :: Boolean  -- Is the options form in a dirty state?
  , group :: Maybe Group         -- Our ideal result type from form submission
  }

type Slots =
  ( groupForm :: Formless.Slot Query GroupSlots GroupForm Group Aff Unit
  , optionsForm :: Formless.Slot Query OptionsSlots OptionsForm Options Aff Unit
  )

_groupForm = SProxy :: SProxy "groupForm"
_optionsForm = SProxy :: SProxy "optionsForm"

----------
-- Formless

type GroupSlots =
  ( typeahead :: TA.Slot Query () String String Aff GroupTASlot
  , dropdown :: Dropdown.Slot Query Admin Aff Unit
  )

_typeahead = SProxy :: SProxy "typeahead"
_dropdown = SProxy :: SProxy "dropdown"

type OptionsSlots =
  ( dropdown :: Dropdown.Slot Query Metric Aff Unit )

----------
-- Slots

data GroupTASlot
  = ApplicationsTypeahead
  | PixelsTypeahead
  | WhiskeyTypeahead
derive instance eqGroupTASlot :: Eq GroupTASlot
derive instance ordGroupTASlot :: Ord GroupTASlot

----------
-- Navigation

data Tab
  = GroupFormTab
  | OptionsFormTab
derive instance eqTab :: Eq Tab
derive instance ordTab :: Ord Tab
