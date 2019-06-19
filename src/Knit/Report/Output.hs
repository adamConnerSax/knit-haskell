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

    -- * file writing helpers
  , writeAllPandocResultsWithInfo
  , writePandocResultWithInfo
  , writeAndMakePathLT
  , writeAndMakePath
  )
where

import qualified Paths_knit_haskell            as Paths
import qualified Data.Map                      as M
import qualified Text.Pandoc                   as PA
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified System.Directory              as SD

import qualified Knit.Effect.Docs              as KD
import qualified Knit.Effect.Pandoc            as KP

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
  fmap (Just . (++ "/knit-haskell-templates/" ++ (T.unpack x))) Paths.getDataDir

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
  let fp = (T.pack dir) <> "/knit-haskell-css/" <> x
  return $ appendCss fp pt


-- | Append a filepath (given as Text) to the existing Css paths in TemplateVariables
appendCss :: T.Text -> TemplateVariables -> TemplateVariables
appendCss x tv =
  let curValM = M.lookup "css" tv
      newVal  = maybe (T.unpack x) (\y -> y ++ "," ++ T.unpack x) curValM
  in  M.insert "css" newVal tv

-- utilities for file output

-- | Write each lazy text from a list of 'KD.DocWithInfo'
-- to disk. File names come from the 'KP.PandocInfo'
-- Directory and file extension are function arguments.
writeAllPandocResultsWithInfo
  :: T.Text -> T.Text -> [KP.DocWithInfo KP.PandocInfo TL.Text] -> IO ()
writeAllPandocResultsWithInfo dir extension =
  fmap (const ()) . traverse (writePandocResultWithInfo dir extension) -- fmap (const ()) :: IO [()] -> IO ()

-- | Write the Lazy Text in a 'KD.DocWithInfo' to disk
-- Name comes from the 'KP.PandocInfo'
-- Directory and file extection are arguments to the function
-- Create the parent directory or directories, if necessary.
writePandocResultWithInfo
  :: T.Text -- ^ directory
  -> T.Text -- ^ extension
  -> KD.DocWithInfo KP.PandocInfo TL.Text
  -> IO ()
writePandocResultWithInfo dir extension (KD.DocWithInfo (KP.PandocInfo n _) x)
  = do
    let fPath = dir <> "/" <> n <> "." <> extension
    writeAndMakePathLT fPath x

-- | Write Lazy Text (Pandoc's Html result) to disk.
-- Create the parent directory or directories, if necessary.
writeAndMakePathLT :: T.Text -> TL.Text -> IO ()
writeAndMakePathLT fPath = writeAndMakePath fPath TL.toStrict

-- | Write (to disk) something which can be converted to text.
-- Create the parent directory or directories, if necessary.
writeAndMakePath :: T.Text -> (a -> T.Text) -> a -> IO ()
writeAndMakePath fPath toStrictText x = do
  let (dirPath, fName) = T.breakOnEnd "/" fPath
  putStrLn
    $  T.unpack
    $  "If necessary, creating "
    <> dirPath
    <> " (and parents), and writing "
    <> fName
  SD.createDirectoryIfMissing True (T.unpack dirPath)
  T.writeFile (T.unpack fPath) $ toStrictText x

