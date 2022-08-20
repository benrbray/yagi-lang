{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators #-}

module Handlers (
    hoverHandler
  , initializedHandler
) where

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- monad transformers
import Control.Monad.IO.Class (liftIO)

-- control
import Control.Lens (assign, modifying, use, (^.))

-- lsp
import Language.LSP.Types.Lens
import qualified Language.LSP.Types as J
import qualified Language.LSP.Server as LSP

-- yagi
import State

------------------------------------------------------------

initializedHandler :: LSP.Handlers HandlerM
initializedHandler =
  LSP.notificationHandler J.SInitialized $ \_msg -> do
    let _xtype = J.MtInfo
    let _message = "server initialized"
    liftLSP $ LSP.sendNotification J.SWindowShowMessage J.ShowMessageParams{..}
    return ()

hoverHandler :: LSP.Handlers HandlerM
hoverHandler =
  LSP.requestHandler J.STextDocumentHover $ \request respond -> do
    let uri_ = request^.params.textDocument.uri
    pure ()

------------------------------------------------------------

-- didOpenTextDocumentHandler :: LSP.Handlers HandlerM
-- didOpenTextDocumentHandler =
--     LSP.notificationHandler J.STextDocumentDidOpen $ \notification -> do
--       let _uri = notification^.params.textDocument.uri
--       diagnosticsHandler _uri

--       let _xtype = J.MtInfo
--       let _message = "server initialized"
--       liftLSP $ LSP.sendNotification J.SWindowShowMessage J.ShowMessageParams{..}
--       return ()

-- diagnosticsHandler :: J.Uri -> HandlerM ()
-- diagnosticsHandler uri = do
--   pure ()