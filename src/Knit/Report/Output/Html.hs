{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

{-|
Module      : Knit.Report.Output.Html
Description : Output Pandoc as Html
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to produce Html output for a Pandoc report.
-}
module Knit.Report.Output.Html
  (
    -- * Default Options
    htmlWriterOptions

    -- * Formatted output
  , toBlazeDocument
  , pandocWriterToBlazeDocument

    -- * Options helper  
  , mindocOptionsF

    -- * Other helpers
  , markDownTextToBlazeFragment

    -- * File writing helpers
  , writeAllPandocResultsWithInfoAsHtml
  , writePandocResultWithInfoAsHtml
  )
where

import qualified Control.Monad.Except           as X
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Map                      as M
import qualified Text.Blaze.Html               as BH
import qualified Text.Pandoc                   as PA


import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM

import           Knit.Report.Input.MarkDown.PandocMarkDown
                                                ( markDownReaderOptions )
import           Knit.Report.Output            as KO

#if MIN_VERSION_pandoc (2,8,0)
import qualified Text.DocTemplates             as DT
#endif

-- | Base Html writer options, with support for MathJax
htmlWriterOptions :: PA.WriterOptions
htmlWriterOptions = PA.def
  { PA.writerExtensions     = PA.extensionsFromList [PA.Ext_raw_html]
  , PA.writerHTMLMathMethod = PA.MathJax ""
  }

-- | Full writer options which use pandoc monad for template access
#if MIN_VERSION_pandoc (2,8,0)
htmlFullDocWriterOptions
  :: forall m. (PA.PandocMonad m, DT.TemplateMonad m)
  => Maybe FilePath -- ^ path to template to include, @Nothing@ for no template.
  -> M.Map String String -- ^ template Variable substitutions
  -> m PA.WriterOptions
htmlFullDocWriterOptions pathM tVars = do
  let tContext = DT.Context $ M.mapKeys T.pack $ fmap (DT.toVal . T.pack) tVars
      makeTemplateM :: FilePath -> m T.Text -> m (Maybe (DT.Template T.Text))
      makeTemplateM pfp getText = do
        txt <- getText
        compiled <- PA.compileTemplate pfp txt
        case compiled of
          Left msg -> X.throwError $ PA.PandocTemplateError $  T.pack msg
          Right tmplt -> return $ Just $ fmap T.pack tmplt
      defaultTemplateM = makeTemplateM "default.Html5" (PA.getDefaultTemplate "Html5")
  templateM <- case pathM of
    Nothing -> defaultTemplateM
    Just fp -> do
      exists <- PA.fileExists fp
      if exists
        then makeTemplateM "" $ fmap (T.pack . BS.unpack) (PA.readFileStrict fp)
        else
          PA.logOutput
              (PA.IgnoredIOError
                (PM.textToPandocText $ "Couldn't find " <> (T.pack $ show fp))
              )
            >> defaultTemplateM
  return $ htmlWriterOptions { PA.writerTemplate      = templateM
                             , PA.writerVariables     = tContext --M.toList tVars
                             , PA.writerSetextHeaders = True
                             }

-- Incudes support for template and template variables and changes to the default writer options
toBlazeDocument
  :: PM.PandocEffects effs
  => KO.PandocWriterConfig
  -> PE.PandocWithRequirements -- ^ Document and union of input requirements 
  -> P.Sem effs BH.Html
toBlazeDocument writeConfig pdocWR = PM.absorbTemplateMonad $ PM.absorbPandocMonad $ do
  writerOptions <- htmlFullDocWriterOptions (templateFP writeConfig)
                                            (templateVars writeConfig)
  PE.fromPandoc PE.WriteHtml5 (optionsF writeConfig writerOptions) pdocWR

#else  
htmlFullDocWriterOptions
  :: PA.PandocMonad m
  => Maybe FilePath -- ^ path to template to include, @Nothing@ for no template.
  -> M.Map String String -- ^ template Variable substitutions
  -> m PA.WriterOptions
htmlFullDocWriterOptions pathM tVars = do
  template <- case pathM of
    Nothing -> PA.getDefaultTemplate "Html5"
    Just fp -> do
      exists <- PA.fileExists fp
      if exists
        then fmap BS.unpack (PA.readFileStrict fp)
        else
          PA.logOutput
              (PA.IgnoredIOError
                (PM.textToPandocText $ "Couldn't find " <> (T.pack $ show fp))
              )
            >> PA.getDefaultTemplate "Html5"
  return $ htmlWriterOptions { PA.writerTemplate      = Just template
                             , PA.writerVariables     = M.toList tVars
                             , PA.writerSetextHeaders = True
                             }

-- Incudes support for template and template variables and changes to the default writer options
toBlazeDocument
  :: PM.PandocEffects effs
  => KO.PandocWriterConfig
  -> PE.PandocWithRequirements -- ^ Document and union of input requirements 
  -> P.Sem effs BH.Html
toBlazeDocument writeConfig pdocWR = PM.absorbPandocMonad $ do
  writerOptions <- htmlFullDocWriterOptions (templateFP writeConfig)
                                            (templateVars writeConfig)
  PE.fromPandoc PE.WriteHtml5 (optionsF writeConfig writerOptions) pdocWR
#endif

-- | Convert markDown to Blaze
markDownTextToBlazeFragment
  :: PM.PandocEffects effs
  => T.Text -- ^ markDown Text
  -> P.Sem effs BH.Html
markDownTextToBlazeFragment =
  PE.fromPandocE PE.WriteHtml5 htmlWriterOptions
    . PE.addFrom PE.ReadMarkDown markDownReaderOptions




-- | Convert given Pandoc to Blaze Html.

-- | Convert current Pandoc document (from the ToPandoc effect) into a Blaze Html document.
-- Incudes support for template and template variables and changes to the default writer options. 
pandocWriterToBlazeDocument
  :: PM.PandocEffects effs
  => KO.PandocWriterConfig -- ^ Configuration info for the Pandoc writer  
  -> P.Sem (PE.ToPandoc ': effs) () -- ^ Effects stack to run to get Pandoc
  -> P.Sem effs BH.Html -- ^ Blaze Html (in remaining effects)
pandocWriterToBlazeDocument writeConfig pw =
  PE.runPandocWriter pw >>= toBlazeDocument writeConfig

-- | options for the mindoc template
mindocOptionsF :: PA.WriterOptions -> PA.WriterOptions
mindocOptionsF op = op { PA.writerSectionDivs = True }


-- file output
-- | Write each lazy text from a list of 'KD.DocWithInfo'
-- to disk. File names come from the 'KP.PandocInfo'
-- Directory is a function arguments.
-- File extension is "html"
writeAllPandocResultsWithInfoAsHtml
  :: T.Text -> [PE.DocWithInfo PE.PandocInfo TL.Text] -> IO ()
writeAllPandocResultsWithInfoAsHtml dir =
  KO.writeAllPandocResultsWithInfo dir "html"

-- | Write the Lazy Text in a 'KD.DocWithInfo' to disk,
-- Name comes from the 'KP.PandocInfo'
-- Directory is an argument to the function
-- File extension is "html"
-- Create the parent directory or directories, if necessary.
writePandocResultWithInfoAsHtml
  :: T.Text -> PE.DocWithInfo PE.PandocInfo TL.Text -> IO ()
writePandocResultWithInfoAsHtml dir dwi =
  KO.writePandocResultWithInfo dir "html" dwi
