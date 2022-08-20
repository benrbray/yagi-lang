module State where

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- json
import qualified Data.Aeson as Aeson
import Data.Aeson
    ( FromJSON (..)
    , (.!=)
    , (.:)
    , (.:?)
    )

-- misc
import           Data.Default (Default (def))

-- monad transformers
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Except         (ExceptT)
import           Control.Monad.Trans.State.Strict   (StateT)

-- lsp
import qualified Language.LSP.Server as LSP

------------------------------------------------------------

data ServerConfig = ServerConfig (Maybe String) deriving Show

instance Default ServerConfig where
  def = ServerConfig Nothing

-- We need to derive the FromJSON instance manually in order to provide defaults
-- for absent fields.
instance Aeson.FromJSON ServerConfig where
  parseJSON = Aeson.withObject "settings" $ \v -> do
    s <- v .: "yagi"
    flip (Aeson.withObject "yagi") s $ \o -> ServerConfig
      <$> o .:? "wibble" .!= Nothing

------------------------------------------------------------

data ServerState = ServerState deriving Show

instance Default ServerState where
  def = ServerState

------------------------------------------------------------

data Severity
  = Error   -- ^ error displayed to the user
  | Warning -- ^ warning displayed to the user
  | Info    -- ^ information displayed to the user
  | Log     -- ^ log message, not displayed by default

type HandlerM =
  ExceptT (Severity, Text) (StateT ServerState (LSP.LspT ServerConfig IO))

liftLSP :: LSP.LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)
