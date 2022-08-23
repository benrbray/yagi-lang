{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers (
    hoverHandler
  , initializedHandler
) where

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- monad transformers
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (catchE, throwE)

-- control
import Control.Lens ((^.))

-- lsp
import Language.LSP.Types.Lens
    ( HasParams(params), HasTextDocument(textDocument), HasUri(uri) )
import qualified Language.LSP.Types as J
import Language.LSP.Types (Uri)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types.Lens       as J -- todo
import           Control.Lens hiding (Iso) -- todo
import qualified Language.LSP.VFS as LSP

-- yagi-lsp-server
import State
import Files

-- yagi-lang
import qualified TextSpan as TS
--import qualified TextSpanLineCol as TS

------------------------------------------------------------

initializedHandler :: LSP.Handlers HandlerM
initializedHandler =
  LSP.notificationHandler J.SInitialized $ \_msg -> do
    let _xtype = J.MtInfo
    let _message = "server initialized"
    liftLSP $ LSP.sendNotification J.SWindowShowMessage J.ShowMessageParams{..}
    return ()


-- NOTE: for TextSpan
-- positionLsp2Yagi :: TS.ParseResult -> J.Position -> Int
-- positionLsp2Yagi (J.Position l c) = 0
--   where x = TS.offsetFromPos
-- positionYagi2Lsp :: Int -> J.Position
-- positionYagi2Lsp (TS.Pos l c) = J.Position (fromIntegral l) (fromIntegral c)


-- NOTE: for TextSpanLineCol
positionLsp2Yagi :: J.Position -> TS.Pos
positionLsp2Yagi (J.Position l c) = TS.Pos (fromIntegral l) (fromIntegral c)
positionYagi2Lsp :: TS.Pos -> J.Position
positionYagi2Lsp (TS.Pos l c) = J.Position (fromIntegral l) (fromIntegral c)

hoverHandler :: LSP.Handlers HandlerM
hoverHandler =
  LSP.requestHandler J.STextDocumentHover $ \request respond ->
    handleErrorWithDefault respond Nothing $ do
      let J.HoverParams _doc pos _workDone = request ^. J.params
          uri_ = _doc^.uri
      
      result@(TS.ParseResult expr _) <- loadFile uri_
      let off = TS.offsetFromPos result (positionLsp2Yagi pos)
      let TS.GetSpan a b term = TS.termAtPos off expr
      let leftPos = TS.posFromOffset result a
      let rightPos = TS.posFromOffset result b
      let _range = Just $ J.Range (positionYagi2Lsp leftPos) (positionYagi2Lsp rightPos)
      let _contents = J.HoverContents (J.MarkupContent J.MkPlainText (T.pack $ show term))

      respond (Right (Just J.Hover{ _contents, _range }))

------------------------------------------------------------

handleErrorWithDefault :: forall a1 a2 b. 
  (Either a1 b -> HandlerM a2)  -- action to attempt
  -> b                             -- default response value
  -> HandlerM a2                   -- 
  -> HandlerM a2
handleErrorWithDefault respond _default = flip catchE handler
  where
    handler :: (Severity, Text) -> HandlerM a2
    handler (Log, _message) = do
      let _xtype = J.MtLog
      liftLSP $ LSP.sendNotification J.SWindowLogMessage J.LogMessageParams{..}
      respond (Right _default)
    handler (severity_, _message) = do
      let _xtype = case severity_ of
            Error   -> J.MtError
            Warning -> J.MtWarning
            Info    -> J.MtInfo
            Log     -> J.MtLog

      liftLSP $ LSP.sendNotification J.SWindowShowMessage J.ShowMessageParams{..}
      respond (Right _default)

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