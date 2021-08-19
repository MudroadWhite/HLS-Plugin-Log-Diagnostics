{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.RecordErrors
  (
    descriptor
  ) where

import           Control.DeepSeq            (NFData)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Binary
import           Data.Functor
import qualified Data.HashMap.Strict        as Map
import           Data.Hashable
import qualified Data.Text                  as T
import           Data.Typeable
import           Development.IDE            as D
import           Development.IDE.Core.Shake (getDiagnostics,
                                             getHiddenDiagnostics)
import           Development.IDE.GHC.Compat (ParsedModule (ParsedModule))
import           GHC.Generics
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           Text.Regex.TDFA.Text       ()

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler SWorkspaceDidChangeWatchedFiles didChangeWatchedFiles
                      -- mkPluginHandler STextDocumentHover          hover
  }

didChangeWatchedFiles :: PluginMethodHandler IdeState WorkspaceDidChangeWatchedFiles
didChangeWatchedFiles ide _ _ = undefined



-- hover :: PluginMethodHandler IdeState TextDocumentHover
-- hover ide _ HoverParams{..} = liftIO $ request "Hover" blah (Right Nothing) foundHover ide TextDocumentPositionParams{..}

-- blah :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
-- blah _ (Position line col)
--   = return $ Just (Just (Range (Position line col) (Position (line+1) 0)), ["Test hover 1\n"])

-- foundHover :: (Maybe Range, [T.Text]) -> Either ResponseError (Maybe Hover)
-- foundHover (mbRange, contents) =
--   Right $ Just $ Hover (HoverContents $ MarkupContent MkMarkdown
--                         $ T.intercalate sectionSeparator contents) mbRange

-- -- | Respond to and log a hover or go-to-definition request
-- request
--   :: T.Text
--   -> (NormalizedFilePath -> Position -> Action (Maybe a))
--   -> Either ResponseError b
--   -> (a -> Either ResponseError b)
--   -> IdeState
--   -> TextDocumentPositionParams
--   -> IO (Either ResponseError b)
-- request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do
--     mbResult <- case uriToFilePath' uri of
--         Just path -> do
--           logInfo (ideLogger ide) "Hover Request"
--           -- logAndRunRequest label getResults ide pos path
--         Nothing   -> pure Nothing
--     pure $ maybe notFound found mbResult

-- logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> Action b)
--                   -> IdeState -> Position -> String -> IO b
-- logAndRunRequest label getResults ide pos path = do
--   let filePath = toNormalizedFilePath path
--       labeltext = (T.unpack label) <> ".hover-typecheck"

--   logInfo (ideLogger ide) "Trying logging typecheck info..."

  -- tcresult <- runIdeAction labeltext (shakeExtras ide) $ runMaybeT $ useE TypeCheck filePath
  -- case tcresult of 
  --   Just (tcmr, _posm) -> 
  --     logInfo (ideLogger ide) "Typecheck error founded?"
  --     logInfo (ideLogger ide) (T.pack $ ("Typecheck error(?):" <> show tcmr))
  --   Nothing -> logInfo (ideLogger ide) "Typecheck: Nothing"

  -- logInfo (ideLogger ide) $
  --   label <> " request at position " <> T.pack (showPosition pos) <>
  --   " in file: " <> T.pack path

  -- runAction "Guandatest" ide $ getResults filePath pos