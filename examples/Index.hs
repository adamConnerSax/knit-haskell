{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}
module Main where

import qualified Knit.Report                   as K

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.String.Here               ( here )

templateVars :: M.Map String String
templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell example index")
--  , ("tufte","True")
  ]

main :: IO ()
main = do
  let template = K.FromIncludedTemplateDir "pandoc-adaptive-bootstrap-KH.html" 
  pandocWriterConfig <- K.mkPandocWriterConfig template
                        templateVars
                        K.mindocOptionsF
                   
  let knitConfig = (K.defaultKnitConfig Nothing)
        { K.outerLogPrefix = Just "Index.Main"
        , K.logIf = K.logAll
        , K.pandocWriterConfig = pandocWriterConfig
        }
  resE <- K.knitHtml knitConfig makeDoc
  case resE of
    Right htmlAsText ->
      K.writeAndMakePathLT "docs/index.html" htmlAsText
    Left err -> putStrLn $ "Pandoc Error: " ++ show err


mdIndex :: T.Text
mdIndex = [here|
## Examples
- [Simple][SimpleSource] demonstrates the bare bones features of the library.
Creating a document from a few fragments and then "knitting" it
into HTML text and writing that to a file. This includes hvega, diagrams and plots examples.
Result [here][SimpleExample]

- [MultiDoc][MultiDocSource] demonstrates how to build multiple documents.
Results [here][MultiDocExample1] and [here][MultiDocExample2].

- [Mtl][MtlSource] demonstrates the same simple features as above,
but runs them atop an example mtl stack,
allowing access to the mtl stack's functionality during document assembly.
Result [here][MtlExample].

- [Random][RandomSource] builds on the mtl example to show how you can also add
an additional polysemy effect (in this case,
Polysemy.RandomFu from polysemy-RandomFu) to your document-building.
This one also demonstrates a use of colonnade for adding a formatted table to the document.
Result [here][RandomExample].

- [Error][ErrorSource]: Similar to "Simple" but throws a user error during document assembly.
Result [here][ErrorExample].

- [Async][AsyncSource]: Similar to "SimpleExample" but uses Polysemy's
sequenceConcurrently to run some example computations
concurrently (as long as you compile with "-threaded").
Result [here][AsyncExample]

- [Cache][CacheSource]: Similar to "SimpleExample" but uses the "AtomicCache"
effect to store the result of a computation.
Demonstrates the behavior of the cache when multiple threads
attempt to access the same item--the first thread loads/creates the
data while the other blocks until the data is in-memory.
Also demonstrates use of time-stamps to force rebuilding when tracked inputs change.
Result [here][CacheExample].

- [CustomCache][CacheSource2]: Similar to "CacheExample" but implements and uses
a different serializer and persistence layer than the default.
Result [here][CacheExample2].

[SimpleExample]: <https://adamconnersax.github.io/knit-haskell/simple_example.html>
[SimpleSource]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/SimpleExample.hs>
[MultiDocExample1]: <https://adamconnersax.github.io/knit-haskell/multi_doc1.html>
[MultiDocExample2]: <https://adamconnersax.github.io/knit-haskell/multi_doc2.html>
[MultiDocSource]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/MultiDocExample.hs>
[MtlExample]: <https://adamconnersax.github.io/knit-haskell/mtl_example.html>
[MtlSource]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/MtlExample.hs>
[RandomExample]: <https://adamconnersax.github.io/knit-haskell/random_example.html>
[RandomSource]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/RandomExample.hs>
[ErrorSource]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/ErrorExample.hs>
[AsyncExample]: <https://adamconnersax.github.io/knit-haskell/async_example.html>
[AsyncSource]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/AsyncExample.hs>
[CacheExample]: <https://adamconnersax.github.io/knit-haskell/cache_example.html>
[CacheSource]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample.hs>
[CacheExample2]: <https://adamconnersax.github.io/knit-haskell/cache_example2.html>
[CacheSource2]: <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample2.hs>
|]

makeDoc :: K.KnitOne effs => K.Sem effs ()
makeDoc = K.wrapPrefix "makeDoc" $ do
  K.logLE K.Info "Building example index"
  K.addMarkDown mdIndex
  return ()

