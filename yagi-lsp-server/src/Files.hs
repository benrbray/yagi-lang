module Files where

-- text
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Utf16.Rope as Rope

-- monad transformers
import Control.Monad.IO.Class             (liftIO)
import Control.Monad.Trans.Class          (lift)
import Control.Monad.Trans.Except       (catchE, throwE)

-- lsp
import qualified Language.LSP.Server     as LSP
import           Language.LSP.Types      (Uri)
import qualified Language.LSP.Types      as LSP.Types
import qualified Language.LSP.VFS        as LSP

-- yagi-lsp-server
import State

-- yagi-lang
import TextSpan (parse, ExprWithSpan)

------------------------------------------------------------

readUri :: Uri -> HandlerM Text
readUri uri = do
  virtualFile <- liftLSP (LSP.getVirtualFile (LSP.Types.toNormalizedUri uri))
  case virtualFile of
    Just (LSP.VirtualFile _ _ rope) -> return (Rope.toText rope)
    Nothing -> throwE (Error, "Path" <> T.pack (show uri) <> " not found in VFS")

loadFile :: Uri -> HandlerM ExprWithSpan
loadFile uri = do
  txt <- readUri uri
  case parse txt of
    Right expr -> return expr
    Left _ -> throwE (Error, "Parse failure")