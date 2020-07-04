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
  let template = K.DefaultTemplate
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
- [Simple][SimpleExample] demonstrates the bare bones features of the library.
Creating a document from a few fragments and then "knitting" it
into HTML text and writing that to a file. This includes hvega, diagrams and plots examples.

- [MultiDoc][MultiDocExample] demonstrates how to build multiple documents.

- [Mtl][MtlExample] demonstrates the same simple features as above,
but runs them atop an example mtl stack,
allowing access to the mtl stack's functionality during document assembly.

- [Random][RandomExample] builds on the mtl example to show how you can also add
an additional polysemy effect (in this case,
Polysemy.RandomFu from polysemy-RandomFu) to your document-building.
This one also demonstrates a use of colonnade for adding a formatted table to the document.

- [Error][ErrorExample]: Similar to "Simple" but throws a user error during document assembly.

- [Async][AsyncExample]: Similar to "SimpleExample" but uses Polysemy's
sequenceConcurrently to run some example computations
concurrently (as long as you compile with "-threaded")

- [Cache][CacheExample]: Similar to "SimpleExample" but uses the "AtomicCache"
effect to store the result of a computation.
Demonstrates the behavior of the cache when multiple threads
attempt to access the same item--the first thread loads/creates the
data while the other blocks until the data is in-memory.
Also demonstrates use of time-stamps to force rebuilding when tracked inputs change.

[CustomCache][CacheExample2]: Similar to "CacheExample" but implements and uses
a different serializer and persistence layer than the default.

[SimpleExample]: <https://adamconnersax.github.io/knit-haskell/simple_example.html>
[MultiDocExample]: <https://adamconnersax.github.io/knit-haskell/multidoc_example.html>
[MtlExample]: <https://adamconnersax.github.io/knit-haskell/mtl_example.html>
[RandomExample]: <https://adamconnersax.github.io/knit-haskell/random_example.html>
[ErrorExample]: <https://adamconnersax.github.io/knit-haskell/error_example.html>
[AsyncExample]: <https://adamconnersax.github.io/knit-haskell/async_example.html>
[CacheExample]: <https://adamconnersax.github.io/knit-haskell/cache_example.html>
[CacheExample2]: <https://adamconnersax.github.io/knit-haskell/cache_example2.html>
|]

makeDoc :: K.KnitOne effs => K.Sem effs ()
makeDoc = K.wrapPrefix "makeDoc" $ do
  K.logLE K.Info "Building example index"
  K.addMarkDown mdIndex
  return ()

