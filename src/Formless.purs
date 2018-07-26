-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Halogen.Data.Slot as Slot
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import Data.Eq (class EqRecord)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse, traverse_)
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormSpec, InputField, OutputField)
import Formless.Spec as FSpec
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.ChildQuery as CQ
import Prim.Row (class Cons)
import Prim.Row (class Cons) as Row
import Prim.RowList (class RowToList) as RL
import Record as Record
import Renderless.State (getState, modifyState, modifyState_, modifyStore_, putState)
import Type.Row (type (+))
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

data Query pq ps form out m a
  = HandleBlur (form InputField -> form InputField) a
  | HandleChange (form InputField -> form InputField) a
  | HandleReset (form InputField -> form InputField) a
  | Reset a
  | Reply (State' form -> a)
  | Validate a
  | Submit a
  | SubmitReply (Maybe out -> a)
  | Send (CQ.ChildQueryBox ps a)
  | Raise (pq Unit) a
  | Replace (Record (SpecRow form out m ())) a
  | Receive (Input pq ps form out m) a
  | AndThen (Query pq ps form out m Unit) (Query pq ps form out m Unit) a

-- | The slot type
type Slot pq ps form out m = H.Slot (Query pq ps form out m) (Message pq form out)

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore pq ps form out m =
  Store (State form out m) (HTML pq ps form out m)

-- | The component type
type Component pq ps form out m
  = H.Component
      HH.HTML
      (Query pq ps form out m)
      (Input pq ps form out m)
      (Message pq form out)
      m

-- | The component's HTML type, the result of the render function.
type HTML pq ps form out m
  = H.ComponentHTML (Query pq ps form out m) ps m

-- | The component's DSL type, the result of the eval function.
type DSL pq ps form out m
  = H.HalogenM
      (StateStore pq ps form out m)
      (Query pq ps form out m)
      ps
      (Message pq form out)
      m

-- | The component local state
type State form out m = Record (StateRow form (internal :: InternalState form out m))

-- | The component's public state
type State' form = Record (StateRow form ())

-- | The component's public state
type StateRow form r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form InputField
  | r
  )

-- | Values provided by the user but maintained by the component
type SpecRow form out m r =
  ( validator :: form InputField -> m (form InputField)
  , submitter :: form OutputField -> m out
  , formSpec :: form FormSpec
  | r
  )

-- | Values created and maintained by the component
type InternalStateRow form out =
  ( initialInputs :: form Internal.Input
  , formResult :: Maybe out
  , allTouched :: Boolean
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form out m = InternalState
  (Record (SpecRow form out m + InternalStateRow form out))
derive instance newtypeInternalState :: Newtype (InternalState form out m) _

-- | A type to represent validation status
data ValidStatus
  = Invalid
  | Incomplete
  | Valid
derive instance genericValidStatus :: Generic ValidStatus _
derive instance eqValidStatus :: Eq ValidStatus
derive instance ordValidStatus :: Ord ValidStatus
instance showValidStatus :: Show ValidStatus where
  show = genericShow

-- | The component's input type
type Input pq ps form out m = Record
  ( SpecRow form out m
  + (render :: State form out m -> HTML pq ps form out m)
  )

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end.
data Message pq form out
  = Submitted out
  | Changed (State' form)
  | Emit (pq Unit)

-- | The component itself
component
  :: ∀ pq ps form out m spec specxs field fieldxs output countxs count inputs inputsxs
   . Monad m
  => RL.RowToList spec specxs
  => RL.RowToList field fieldxs
  => RL.RowToList count countxs
  => RL.RowToList inputs inputsxs
  => EqRecord inputsxs inputs
  => Internal.FormSpecToInputField specxs spec field
  => Internal.InputFieldsToInput fieldxs field inputs
  => Internal.SetInputFieldsTouched fieldxs field field
  => Internal.InputFieldToMaybeOutput fieldxs field output
  => Internal.CountErrors fieldxs field count
  => Internal.AllTouched fieldxs field
  => Internal.SumRecord countxs count (Additive Int)
  => Newtype (form FormSpec) (Record spec)
  => Newtype (form InputField) (Record field)
  => Newtype (form OutputField) (Record output)
  => Newtype (form Internal.Input) (Record inputs)
  => Component pq ps form out m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState :: Input pq ps form out m -> StateStore pq ps form out m
  initialState { formSpec, validator, render, submitter } = store render $
    { validity: Incomplete
    , dirty: false
    , errors: 0
    , submitAttempts: 0
    , submitting: false
    , form: inputFields
    , internal: InternalState
      { formResult: Nothing
      , formSpec
      , allTouched: false
      , initialInputs: Internal.inputFieldsToInput inputFields
      , validator
      , submitter
      }
    }
    where
      inputFields = Internal.formSpecToInputFields formSpec

  eval :: Query pq ps form out m ~> DSL pq ps form out m
  eval = case _ of
    HandleBlur fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      eval $ Validate a

    HandleChange fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      pure a

    HandleReset fs a -> do
      modifyState_ \st -> st
        { form = fs st.form
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      eval $ Validate a

    Validate a -> do
      init <- getState
      let internal = unwrap init.internal
      form <- H.lift $ internal.validator init.form
      let errors = Internal.countErrors form

      -- At this point we can modify most of the state, except for the valid status
      modifyState_ _
        { form = form
        , errors = errors
          -- Dirty state is computed by checking equality of original input fields vs. current ones.
          -- This relies on input fields passed by the user having equality defined.
        , dirty = not $ unwrap (Internal.inputFieldsToInput form) == unwrap internal.initialInputs
        }

      -- Need to verify the validity status of the form.
      new <- case internal.allTouched of
        true -> modifyState _
          { validity = if not (errors == 0) then Invalid else Valid }
        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        _ -> case Internal.checkTouched form of
          -- The sync revealed all fields really have been touched
          true -> modifyState \st -> st
            { validity = if not (errors == 0) then Invalid else Valid
            , internal = over InternalState (_ { allTouched = true }) st.internal
            }
          -- The sync revealed that not all fields have been touched
          _ -> modifyState _
            { validity = Incomplete }

      H.raise $ Changed $ getPublicState new
      pure a

    -- Submit, also raising a message to the user
    Submit a -> a <$ do
      st <- runSubmit
      traverse_ (H.raise <<< Submitted) st

    -- Submit, without raising a message, but returning the result directly
    SubmitReply reply -> do
       st <- runSubmit
       pure $ reply st

    -- | Should completely reset the form to its initial state
    Reset a -> do
      new <- modifyState \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , form = Internal.formSpecToInputFields (_.formSpec $ unwrap st.internal)
        , internal = over InternalState (_
            { formResult = Nothing
            , allTouched = false
            }
          ) st.internal
        }
      H.raise $ Changed $ getPublicState new
      pure a

    Reply reply -> do
      st <- getState
      pure $ reply $ getPublicState st

    Send box -> H.HalogenM $ liftF $ H.ChildQuery box

    Raise query a -> do
      H.raise (Emit query)
      pure a

    Replace { formSpec, validator, submitter } a -> do
      let inputFields = Internal.formSpecToInputFields formSpec
          new =
            { validity: Incomplete
            , dirty: false
            , errors: 0
            , submitAttempts: 0
            , submitting: false
            , form: inputFields
            , internal: InternalState
              { formResult: Nothing
              , formSpec
              , allTouched: false
              , initialInputs: Internal.inputFieldsToInput inputFields
              , validator
              , submitter
              }
            }
      putState new
      H.raise $ Changed $ getPublicState new
      pure a

    Receive { render } a -> do
      modifyStore_ render identity
      pure a

    AndThen q1 q2 a -> do
      _ <- eval q1
      _ <- eval q2
      pure a

  ----------
  -- Effectful eval helpers

  -- Remove internal fields and return the public state
  getPublicState :: State form out m -> State' form
  getPublicState = Record.delete (SProxy :: SProxy "internal")

  -- Run submission without raising messages or replies
  runSubmit :: DSL pq ps form out m (Maybe out)
  runSubmit = do
    init <- modifyState \st -> st
      { submitAttempts = st.submitAttempts + 1
      , submitting = true
      }

    -- For performance purposes, avoid running this if possible
    let internal = unwrap init.internal
    when (not internal.allTouched) do
      modifyState_ _
       { form = Internal.setInputFieldsTouched init.form
       , internal = over InternalState (_ { allTouched = true }) init.internal
       }

    -- Necessary to validate after fields are touched, but before parsing
    _ <- eval $ Validate unit

    -- For performance purposes, only attempt to submit if the form is valid
    validated <- getState
    when (validated.validity == Valid) do
      output <- H.lift $
        traverse internal.submitter (Internal.inputFieldToMaybeOutput validated.form)
      modifyState_ _
        { internal = over InternalState (_ { formResult = output }) validated.internal }

    -- Ensure the form is no longer marked submitting
    result <- modifyState \st -> st { submitting = false }
    pure $ _.formResult $ unwrap result.internal


--------------------
-- Querying
--------------------

send
  :: ∀ sym pq ps cq cm slot form out m a t0
   . Row.Cons sym (Slot.Slot cq cm slot) t0 ps
  => IsSymbol sym
  => Ord slot
  => SProxy sym
  -> slot
  -> cq a
  -> Query pq ps form out m (Maybe a)
send sym slot query = Send $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k -> traverse k <<< Slot.lookup sym slot) query identity

sendAll
  :: ∀ sym pq ps cq cm slot form out m a t0
   . Row.Cons sym (Slot.Slot cq cm slot) t0 ps
  => IsSymbol sym
  => Ord slot
  => SProxy sym
  -> cq a
  -> Query pq ps form out m (Map slot a)
sendAll sym query = Send $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k -> traverse k <<< Slot.slots sym) query identity

--------------------
-- External to the component
--------------------

-- | Provided as a query
modify
  :: ∀ sym pq ps out m form form' i e o r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> (i -> i)
  -> Query pq ps form' out m Unit
modify sym f = HandleChange (modify' sym f) unit

-- | Allows you to modify a field rather than set its value
modify'
  :: ∀ sym form form' e i o r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> (i -> i)
  -> form'
  -> form'
modify' sym f = wrap <<< setInput f <<< setTouched true <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (InputField e i o)
    _sym = prop sym
    setInput =
      Lens.over (_sym <<< _Newtype <<< prop FSpec._input)
    setTouched =
      Lens.set (_sym <<< _Newtype <<< prop FSpec._touched)

-- | Handles resetting a single field, but is only possible if the field is
-- | a member of the Initial type class
handleReset
  :: ∀ pq ps m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => Initial i
  => SProxy sym
  -> Query pq ps form' out m Unit
handleReset sym = HandleReset (handleReset' sym) unit

handleReset'
  :: ∀ sym form' form i e o r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype form' (Record form)
  => Initial i
  => SProxy sym
  -> form'
  -> form'
handleReset' sym = wrap <<< unsetTouched <<< unsetResult <<< unsetValue <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (InputField e i o)
    _sym = prop sym
    unsetTouched = Lens.set (_sym <<< _Newtype <<< prop FSpec._touched) false
    unsetResult = Lens.set (_sym <<< _Newtype <<< prop FSpec._result) Nothing
    unsetValue = Lens.set (_sym <<< _Newtype <<< prop FSpec._input) initial

onClickWith
  :: ∀ pq ps m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onClick :: MouseEvent | props) (Query pq ps form' out m Unit)
onClickWith sym i =
  HE.onClick \_ -> Just (handleBlurAndChange sym i)

-- | Performs behaviors for both blur and change events
handleBlurAndChange
  :: ∀ pq ps m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> Query pq ps form' m out Unit
handleBlurAndChange sym val = HandleBlur (handleBlur' sym <<< handleChange' sym val) unit

-- | Given a proxy symbol, will trigger validation on that field using
-- | its validator and current input
onBlurWith
  :: ∀ pq ps m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onBlur :: FocusEvent | props) (Query pq ps form' out m Unit)
onBlurWith sym = HE.onBlur $ const $ Just $ handleBlur sym

handleBlur
  :: ∀ pq ps m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> Query pq ps form' m out Unit
handleBlur sym = HandleBlur (handleBlur' sym) unit

handleBlur'
  :: ∀ sym form' form i e o r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> form'
  -> form'
handleBlur' sym form = wrap <<< setTouched $ unwrap form
  where
    _sym :: Lens.Lens' (Record form) (InputField e i o)
    _sym = prop sym
    setTouched = Lens.set (_sym <<< _Newtype <<< prop FSpec._touched) true

-- | Replace the value at a given field with a new value of the correct type.
onValueInputWith
  :: ∀ pq ps m sym form' form e o out r props
   . IsSymbol sym
  => Cons sym (InputField e String o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onInput :: Event, value :: String | props) (Query pq ps form' out m Unit)
onValueInputWith sym =
  HE.onValueInput \str -> Just (handleChange sym str)

onValueChangeWith
  :: ∀ pq ps m sym form' form e o out r props
   . IsSymbol sym
  => Cons sym (InputField e String o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onChange :: Event, value :: String | props) (Query pq ps form' out m Unit)
onValueChangeWith sym =
  HE.onValueChange \str -> Just (handleChange sym str)

onChangeWith
  :: ∀ pq ps m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onChange :: Event | props) (Query pq ps form' out m Unit)
onChangeWith sym i =
  HE.onChange \_ -> Just (handleChange sym i)

handleChange
  :: ∀ pq ps m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> Query pq ps form' out m Unit
handleChange sym val = HandleChange (handleChange' sym val) unit

handleChange'
  :: ∀ sym form form' e i o r
   . IsSymbol sym
  => Cons sym (InputField e i o) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> i
  -> form'
  -> form'
handleChange' sym val = wrap <<< setInput val <<< setTouched true <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (InputField e i o)
    _sym = prop sym
    setInput =
      Lens.set (_sym <<< _Newtype <<< prop FSpec._input)
    setTouched =
      Lens.set (_sym <<< _Newtype <<< prop FSpec._touched)
