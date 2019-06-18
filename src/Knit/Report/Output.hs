{-# LANGUAGE OverloadedStrings #-}
module Knit.Report.Output
  (
    -- * Pandoc Writer Configuration
    PandocWriterConfig(..)
  , TemplateVariables
  , WriterOptionsF
  , mkPandocWriterConfig

    -- * Pandoc Template Types
  , TemplatePath(..)
  , CssPath(..)
  , addCss
  )
where

import qualified Paths_knit_haskell            as Paths
import qualified Data.Map                      as M
import qualified Text.Pandoc                   as PA
import qualified Data.Text                     as T

type TemplateVariables = M.Map String String
type WriterOptionsF = PA.WriterOptions -> PA.WriterOptions

data PandocWriterConfig =
  PandocWriterConfig
  {
    templateFP :: Maybe FilePath
    -- ^ optional path to pandoc <https://pandoc.org/MANUAL.html#templates template>
  , templateVars :: TemplateVariables
  -- ^ variable substitutions for the <https://pandoc.org/MANUAL.html#templates template>
  , optionsF :: WriterOptionsF
  -- ^ change default <https://pandoc.org/MANUAL.html#general-writer-options options>
  }

-- | Make a 'PandocWriterConfig' from a PandocTemplate specification
mkPandocWriterConfig
  :: TemplatePath
  -> TemplateVariables
  -> WriterOptionsF
  -> IO PandocWriterConfig
mkPandocWriterConfig tp tv wf = do
  templateFPM <- pandocTemplatePath tp
  return $ PandocWriterConfig templateFPM tv wf

-- | Type to specify path to template,
-- which may be in a directory installed with knit-haskell.
data TemplatePath = DefaultTemplate
                  | FromIncludedTemplateDir T.Text
                  | FullySpecifiedTemplatePath T.Text

-- | get correct path to give Pandoc, depending on how things are installed
pandocTemplatePath :: TemplatePath -> IO (Maybe String)
pandocTemplatePath DefaultTemplate                = return Nothing
pandocTemplatePath (FullySpecifiedTemplatePath x) = return $ Just (T.unpack x)
pandocTemplatePath (FromIncludedTemplateDir x) =
  fmap (Just . (++ "/" ++ (T.unpack x))) Paths.getDataDir

-- | Type to specify path to Css,
-- which may be in a directory installed with knit-haskell or not.
data CssPath = FromIncludedCssDir T.Text
             | FullySpecifiedCssPath T.Text

-- | Add a CssPath to an existing TemplateVariables
-- which may already have Css paths specified
addCss :: CssPath -> TemplateVariables -> IO TemplateVariables
addCss (FullySpecifiedCssPath x) pt = return $ appendCss x pt
addCss (FromIncludedCssDir    x) pt = do
  dir <- Paths.getDataDir
  let fp = (T.pack dir) <> "/" <> x
  return $ appendCss fp pt


-- | Append a filepath (given as Text) to the existing Css paths in TemplateVariables
appendCss :: T.Text -> TemplateVariables -> TemplateVariables
appendCss x tv =
  let curValM = M.lookup "css" tv
      newVal  = maybe (T.unpack x) (\y -> y ++ "," ++ T.unpack x) curValM
  in  M.insert "css" newVal tv
