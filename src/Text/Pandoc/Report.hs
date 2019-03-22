{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
module Text.Pandoc.Report where

import           Control.Monad.Trans           (liftIO)
import qualified Data.ByteString.Char8         as BS
--import           Data.Monoid                 ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Map                      as M
import qualified Graphics.Vega.VegaLite        as GV
import qualified Text.Blaze.Html               as BH
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.Extensions        as P
import qualified Lucid                         as LH
import qualified Control.Monad.Freer.Pandoc   as P
import qualified Control.Monad.Freer.PandocMonad   as PM
import qualified Control.Monad.Freer.Html     as H
import qualified Control.Monad.Freer  as FR

htmlReaderOptions = P.def { P.readerExtensions = P.extensionsFromList [P.Ext_raw_html] }

htmlReaderOptionsWithHeader = htmlReaderOptions { P.readerStandalone = True }

htmlWriterOptions = P.def
                    {
                      P.writerExtensions = P.extensionsFromList [P.Ext_raw_html]
                    , P.writerHTMLMathMethod = P.MathJax ""
                    }

htmlFullDocWriterOptions :: P.PandocMonad m => Maybe FilePath -> M.Map String String -> m P.WriterOptions
htmlFullDocWriterOptions pathM templateVars = do
  template <- case pathM of
    Nothing -> P.getDefaultTemplate "Html5"
    Just fp -> do
      exists <- P.fileExists fp
      if exists
        then fmap BS.unpack (P.readFileStrict fp)
        else P.logOutput  (P.IgnoredIOError ("Couldn't find " ++ show fp)) >> P.getDefaultTemplate "Html5"
  return $
    htmlWriterOptions
    {
      P.writerTemplate = Just template
    , P.writerVariables = M.toList templateVars
    , P.writerSetextHeaders = True
    }

markdownReaderOptions =
  P.def {
     P.readerStandalone = True
  ,  P.readerExtensions= P.extensionsFromList
                      [
                        P.Ext_auto_identifiers
                      , P.Ext_backtick_code_blocks
                      , P.Ext_fancy_lists
                      , P.Ext_footnotes
                      , P.Ext_simple_tables
                      , P.Ext_multiline_tables
                      , P.Ext_tex_math_dollars
                      , P.Ext_header_attributes
                      , P.Ext_implicit_header_references
                      ]
  }

addMarkDown :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs) => T.Text -> FR.Eff effs ()
addMarkDown = P.addFrom P.ReadMarkDown markdownReaderOptions

addHtml :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs) => T.Text -> FR.Eff effs ()
addHtml = P.addFrom P.ReadHtml htmlReaderOptions 

addBlaze :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs) => BH.Html -> FR.Eff effs ()
addBlaze = addHtml . LT.toStrict . BH.renderHtml

addLucid :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs) => LH.Html () -> FR.Eff effs ()
addLucid = addHtml . LT.toStrict . LH.renderText

markDownTextToBlazeFragment :: PM.PandocEffects effs => T.Text -> FR.Eff effs BH.Html 
markDownTextToBlazeFragment = P.fromPandocE P.WriteHtml5 htmlWriterOptions . P.addFrom P.ReadMarkDown markdownReaderOptions 

toBlazeDocument :: PM.PandocEffects effs => Maybe FilePath -> M.Map String String -> (P.WriterOptions -> P.WriterOptions) -> P.Pandoc -> FR.Eff effs BH.Html 
toBlazeDocument templatePathM templateVars optionsF pdoc = do
  writerOptions <- htmlFullDocWriterOptions templatePathM templateVars
  P.fromPandoc P.WriteHtml5 (optionsF writerOptions) pdoc

pandocWriterToBlazeDocument ::  PM.PandocEffects effs
  => Maybe FilePath -> M.Map String String -> (P.WriterOptions -> P.WriterOptions) -> FR.Eff (P.ToPandoc ': effs) () -> FR.Eff effs BH.Html
pandocWriterToBlazeDocument templatePathM templateVars optionsF pw = P.runPandocWriter pw >>= toBlazeDocument templatePathM templateVars optionsF


mindocOptionsF op = op { P.writerSectionDivs = True }
