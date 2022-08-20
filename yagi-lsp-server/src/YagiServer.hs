{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators #-}

module YagiServer (
  main,
  HandlerM,
  ServerConfig
) where

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

-- system
import           System.Exit (ExitCode(..))
import qualified System.Exit as Exit

-- monad transformers
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Except         (ExceptT)
import           Control.Monad.Trans.State.Strict   (StateT)
import qualified Control.Monad.Trans.Except       as Except
import qualified Control.Monad.Trans.State.Strict as State

-- concurrent
import qualified Control.Concurrent.MVar as MVar

-- lsp
import qualified Language.LSP.Types as J
import Language.LSP.Server
    ( ServerDefinition(..), type (<~>)(..) )
import qualified Language.LSP.Server as LSP

-- yagi-lsp-server
import Handlers
import State

------------------------------------------------------------

main :: IO ()
main = run

run :: IO ()
run = do

  state <- MVar.newMVar (def :: ServerState)

  let defaultConfig = def :: ServerConfig

  let onConfigurationChange _oldConfig json =
        case Aeson.fromJSON json of
          Aeson.Success config -> Right config
          Aeson.Error   string -> Left (T.pack string)

  let doInitialize environment _request = do
          return (Right environment)

  let staticHandlers =
        mconcat
          [ initializedHandler
          ]

  let interpretHandler :: LSP.LanguageContextEnv ServerConfig -> (HandlerM <~> IO)
      interpretHandler environment = Iso{..}
        where forward :: HandlerM a -> IO a
              forward handler =
                MVar.modifyMVar state (update handler)

              update :: HandlerM a -> ServerState -> IO (ServerState, a)
              update handler oldState = do
                LSP.runLspT environment $ do
                  (e, newState) <- State.runStateT (Except.runExceptT handler) oldState
                  result <- case e of
                    -- write message to log output
                    Left (Log, _message) -> do
                      let _xtype = J.MtLog
                      LSP.sendNotification J.SWindowLogMessage J.LogMessageParams{..}
                      liftIO (fail (T.unpack _message))
                    -- display error message to user 
                    Left (severity, _message) -> do
                      let _xtype = case severity of
                            Error   -> J.MtError
                            Warning -> J.MtWarning
                            Info    -> J.MtInfo
                            Log     -> J.MtLog
                      LSP.sendNotification J.SWindowShowMessage J.ShowMessageParams{..}
                      liftIO (fail (T.unpack _message))
                    -- success
                    Right a ->
                      return a

                  return (newState, result)

              backward = liftIO

  let options = def

  exitCode <- LSP.runServer ServerDefinition{..}

  case exitCode of
    0 -> return ()
    n -> Exit.exitWith (ExitFailure n)
