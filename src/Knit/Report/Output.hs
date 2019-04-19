module Knit.Report.Output
  (
    -- * Pandoc Writer Configuration
    PandocWriterConfig(..)
  )
where

import qualified Data.Map                      as M
import qualified Text.Pandoc                   as PA

data PandocWriterConfig =
  PandocWriterConfig
  {
    templateFP :: Maybe FilePath
    -- ^ optional path to pandoc <https://pandoc.org/MANUAL.html#templates template>
  , templateVars :: M.Map String String
  -- ^ variable substitutions for the <https://pandoc.org/MANUAL.html#templates template>
  , optionsF :: (PA.WriterOptions -> PA.WriterOptions)
  -- ^ change default <https://pandoc.org/MANUAL.html#general-writer-options options>
  }
