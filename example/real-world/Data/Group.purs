module Example.RealWorld.Data.Group where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Example.App.Validation (class ToText, Errs)
import Example.RealWorld.Data.Options (Options)
import Formless as F

-----
-- A custom ID type

newtype GroupId = GroupId Int
derive instance newtypeFBGroupId :: Newtype GroupId _
derive newtype instance eqGroupId :: Eq GroupId
derive newtype instance showGroupId :: Show GroupId

-----
-- A nested field type

newtype Admin = Admin { id :: Maybe GroupId }
derive instance newtypeAdmin :: Newtype Admin _
derive newtype instance eqAdmin :: Eq Admin
derive newtype instance showAdmin :: Show Admin

instance toTextAdmin :: ToText Admin where
  toText (Admin { id }) = case id of
    Just (GroupId n) -> "Administrator " <> show n
    Nothing -> "None"

-----
-- Our primary data type
type GroupRow f r =
  ( name         :: f Errs String         String
  , admin        :: f Errs (Maybe Admin)  Admin
  , applications :: f Errs (Array String) (Array String)
  , pixels       :: f Errs (Array String) (Array String)
  , whiskey      :: f Errs (Maybe String) String
  | r
  )

_name = SProxy :: SProxy "name"
_admin = SProxy :: SProxy "admin"
_applications = SProxy :: SProxy "applications"
_pixels = SProxy :: SProxy "pixels"
_whiskey = SProxy :: SProxy "whiskey"

-- | Here's the Group data type we'll use throughout our application. After we send
-- | a form result off to the server, this is what we'll get in return.
newtype Group = Group
  ( Record
    ( GroupRow F.OutputType
      ( id :: GroupId
      , secretKey :: String
      , options :: Maybe Options
      )
    )
  )
derive instance newtypeGroup :: Newtype Group _
derive newtype instance eqGroup :: Eq Group
derive newtype instance showGroup :: Show Group

_id = SProxy :: SProxy "id"
_secretKey = SProxy :: SProxy "secretKey"
_options = SProxy :: SProxy "options"

-- | Here's the Form type we'll use to run with Formless.
newtype GroupForm f = GroupForm (Record (GroupFormRow f))
derive instance newtypeGroupForm :: Newtype (GroupForm f) _

-- | In order to generate our fields automatically using mkFormSpecFromRow, we'll make
-- | sure to have the new row as a new type.
type GroupFormRow f = GroupRow f
  ( secretKey1 :: f Errs String String
  , secretKey2 :: f Errs String String
  )

_secretKey1 = SProxy :: SProxy "secretKey1"
_secretKey2 = SProxy :: SProxy "secretKey2"
