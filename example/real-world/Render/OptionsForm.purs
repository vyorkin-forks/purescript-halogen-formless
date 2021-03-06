module Example.RealWorld.Render.OptionsForm where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.App.UI.Dropdown as Dropdown
import Example.App.UI.Element (css)
import Example.App.UI.Element as UI
import Example.RealWorld.Data.Options (Metric(..), Speed(..), _enable, _metric)
import Example.RealWorld.Data.Options as OP
import Example.RealWorld.Types (OptionsCQ, OptionsCS, Query(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | A convenience synonym for the group Formless state
type FormlessState
  = F.State OP.OptionsForm OP.Options Aff

-- | A convenience synonym for the group Formless HTML type
type FormlessHTML
  = F.HTML Query OptionsCQ OptionsCS OP.OptionsForm OP.Options Aff

-- | The form, grouped by sections.
render :: FormlessState -> FormlessHTML
render state =
  UI.formContent_
    [ renderEnabled state
    , HH.div
      [ if F.getInput _enable state.form then css "" else css "is-hidden" ]
      ( renderMetrics state
      <> renderOthers state
      )
    ]

-----
-- Form parts

renderMetrics :: FormlessState -> Array FormlessHTML
renderMetrics state =
    [ renderMetric state
    , renderMetricField (F.getInput _metric state.form)
    ]
  where
    renderMetricField = case _ of
      Just ViewCost -> renderViewCost state
      Just ClickCost -> renderClickCost state
      Just InstallCost -> renderInstallCost state
      Nothing -> HH.div_ []

renderOthers :: FormlessState -> Array FormlessHTML
renderOthers state =
  [ renderSize state
  , renderDimensions state
  , renderSpeed state
  ]

-----
-- Fields

renderEnabled :: FormlessState -> FormlessHTML
renderEnabled state =
  UI.field
    { label: "Enable"
    , help: Right "Do you want to enable this set of options?"
    }
    [ HH.label
      [ css "checkbox" ]
      [ HH.input
        [ css "checkbox"
        , HP.type_ InputCheckbox
        , HP.checked $ F.getInput _enable state.form
        , HE.onChange $ HE.input_ $ F.Modify (F.modifyInput _enable not)
        ]
      , HH.text " Enable extra options"
      ]
    ]

renderMetric :: FormlessState -> FormlessHTML
renderMetric state =
  UI.field
    { label: "Metric"
    , help: UI.resultToHelp "Choose a metric to optimize for." (F.getResult OP._metric state.form)
    }
    [ HH.slot unit Dropdown.component
        { placeholder: "Choose a metric"
        , items: [ ViewCost, ClickCost, InstallCost ]
        }
        ( HE.input $ F.Raise <<< H.action <<< MetricDropdown )
    ]

renderViewCost :: FormlessState -> FormlessHTML
renderViewCost =
  UI.formlessField UI.input
    { label: "View Cost"
    , placeholder: "100"
    , help: "Enter a dollar amount for view costs."
    , sym: OP._viewCost
    }

renderClickCost :: FormlessState -> FormlessHTML
renderClickCost =
  UI.formlessField UI.input
    { label: "Click Cost"
    , placeholder: "1"
    , help: "Enter a dollar amount you're willing to pay for a click."
    , sym: OP._clickCost
    }

renderInstallCost :: FormlessState -> FormlessHTML
renderInstallCost =
  UI.formlessField UI.input
    { label: "Install Cost"
    , placeholder: "10"
    , help: "Enter a dollar amount you're willing to pay for an app instal."
    , sym: OP._installCost
    }

renderSize :: FormlessState -> FormlessHTML
renderSize =
  UI.formlessField UI.input
    { label: "Size"
    , placeholder: "10.233"
    , help: "Enter a total campaign size."
    , sym: OP._size
    }

renderDimensions :: FormlessState -> FormlessHTML
renderDimensions =
  UI.formlessField UI.input
    { label: "Dimensions"
    , placeholder: "1.027"
    , help: "Enter a total campaign dimension set ratio buzzword."
    , sym: OP._dimensions
    }

renderSpeed :: FormlessState -> FormlessHTML
renderSpeed state =
  UI.field
    { label: "Speed"
    , help: Right "How fast do you want to go?"
    }
    [ HH.label
      [ css "radio" ]
      [ HH.input
        [ HP.name "speed"
        , css "radio"
        , HP.type_ InputRadio
        , HP.checked $ speed.input == Low
        , HE.onClick $ HE.input_ $ F.Modify $ F.setInput OP._speed Low
        ]
      , HH.text $ " " <> show Low
      ]
    , HH.label
      [ css "radio" ]
      [ HH.input
        [ HP.name "speed"
        , css "radio"
        , HP.type_ InputRadio
        , HP.checked $ speed.input == Medium
        , HE.onClick $ HE.input_ $ F.Modify $ F.setInput OP._speed Medium
        ]
      , HH.text $ " " <> show Medium
      ]
    , HH.label
      [ css "radio" ]
      [ HH.input
        [ HP.name "speed"
        , css "radio"
        , HP.type_ InputRadio
        , HP.checked $ speed.input == Fast
        , HE.onClick $ HE.input_ $ F.Modify $ F.setInput OP._speed Fast
        ]
      , HH.text $ " " <> show Fast
      ]
    ]
  where
    speed = view (F._Field OP._speed) state.form
