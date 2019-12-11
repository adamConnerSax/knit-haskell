# knit-haskell v0.8.0.0

[![Build Status][travis-badge]][travis]
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

## Breaking Change
To move from v0.7.x.x to v0.8.x.x requires a change in how configuration parameters given to ```knit-html``` and ```knit-htmls``` are 
handled: they are now all placed inside a ```KnitConfig```.  
It's a trivial change to make and should make the configuration more future-proof as long as you build your ```KnitConfig``` like, e.g., 
```haskell
myConfig :: KnitConfig
myConfig = defaultKnitConfig { outerLogPrefix = Just "MyReport", cacheDir = "myCache" }
```

Also note that a new major version of Pandoc has been released (2.8.x).  knit-haskell can be compiled against this as
well as the older versions but there are some major changes which may affect you if you use any of the pandoc 
functions directly.  In particular, Pandoc has now switched to using ```Text``` instead of ```String``` for
most things.

## Introduction
knit-haskell is an attempt to emulate parts of the RMarkdown/knitR experience in haskell. 
The idea is to be able to build HTML (or, perhaps, some other things [Pandoc](http://hackage.haskell.org/package/pandoc) can write) 
inside a haskell executable.  
This package wraps Pandoc and the 
[PandocMonad](http://hackage.haskell.org/package/pandoc-2.7.2/docs/Text-Pandoc-Class.html#t:PandocMonad), 
has logging facilities and support for inserting [hvega](http://hackage.haskell.org/package/hvega), 
[diagrams](https://hackage.haskell.org/package/diagrams), and 
[plots](https://hackage.haskell.org/package/plots) based 
visualizations.  
All of that is handled via writer-like effects, so additions to the documents can be interspersed with regular haskell code. 

As of version 0.8.0.0, the effect stack includes:
* An "Async" effect ([Polysemy.Async](https://hackage.haskell.org/package/polysemy-1.2.3.0/docs/Polysemy-Async.html)) 
for running computations concurrently. Combinators for launching a concurrent action (```async```), 
awaiting (```await```) it's result and running some traversable structure of concurrent actions
(```sequenceConcurrently```) are re-exported via ```Knit.Report```.  NB: Polysemy returns a ```Maybe a``` where
the traditional interface returns an ```a```. 
From the docs "The Maybe returned by async is due to the fact that we can't be sure an Error effect didn't fail locally."
* A persistent (using disk) cache for "shelving" the results of computations between report runs.  Anything which has
a ```Serialize``` instance from the [cereal](https://hackage.haskell.org/package/cereal) 
package can be cached. If you use the cache, and you are running in a version-controlled directory,
you probably want to add your cache directory, specified in the ```knit-hmtl``` call, to ".gitignore" or equivalent.
There are two ways to interact with the cache:
    - A direct interface, where data can be put into the cache via ```store```, retrieved via ```retrieve``` and 
retrieved or created via ```retrieveOrMake``` which will attempt to retrieve the data and run a given action 
to produce the data if the retrieval fails.  This is often how you might introduce cached data into the report:
it will get made the first time, and any time after that if the cache is deleted, but from then on will be 
loaded from disk. Entries can be cleared from the cache via ```clear```.
    - A "lazy" interface which wraps those operations in a GADT, ```Cached es a```,
which can be passed around instead of the ```a``` itself.  All cache retrieval or running of computations
is deferred until ```useCached``` is called on a ```Cached a```.  This means that a cached computation
which is never used will never be run or retrieved.  This is useful, e.g., if you cache some parsed data
which is passed to a long-running computation whose results are cached and used for analysis.  If you
only want to work on the results of the analysis you can load them from cache and never reload the data.
The disadvantage is the ```es``` parameter of 
```Cached es a```, which is a list of polysemy effects required to run the action.  That will need 
to be specified when the action is built--it can't be inferred from the action itself--and can cause some 
confusing inference issues when used.  
    - Please see  [CacheExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample.hs) for more.

## Supported Inputs
* [markdown](https://pandoc.org/MANUAL.html#pandocs-markdown)
* HTML ([blaze](http://hackage.haskell.org/package/blaze-html), [lucid](http://hackage.haskell.org/package/lucid) or Text)
* [latex](https://en.wikipedia.org/wiki/LaTeX)
* [colonnade tables](https://hackage.haskell.org/package/colonnade)
* [hvega](http://hackage.haskell.org/package/hvega) visualizations (via [blaze](http://hackage.haskell.org/package/blaze-html) HTML) 
* [Diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) (via Diagrams SVG backend, inserted as HTML) 

## Examples
There are a few examples in the "examples" directory.  
* [SimpleExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/SimpleExample.hs) 
demonstrates the bare bones features of the library.  Creating a document from a few fragments and then 
"knitting" it into HTML text and writing that to a file. This includes hvega, diagrams and plots examples.
* [MultiDocExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/MultiDocExample.hs) 
demonstrates how to build multiple documents.
* [MtlExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/MtlExample.hs) 
demonstrates the same simple features as above, but runs them atop an example mtl stack, 
allowing access to the mtl stack's functionality during document assembly.
* [RandomExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/RandomExample.hs) 
builds on the mtl example to show how you can also add an additional polysemy effect (in this case, 
Polysemy.RandomFu from [polysemy-RandomFu](https://hackage.haskell.org/package/polysemy-RandomFu))
to your document-building. 
This one also demonstrates a use of [colonnade](https://hackage.haskell.org/package/colonnade) 
for adding a formatted table to the document.
* [ErrorExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/ErrorExample.hs).  
Similar to "SimpleExample" but throws a user error during document assembly.
* [AsyncExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/AsyncExample.hs).  
Similar to "SimpleExample" but uses Polysemy's ```sequenceConcurrently``` to run some example
computations concurrently (as long as you compile with "-threaded")
* [CacheExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample.hs).  
Similar to "SimpleExample" but uses the "AtomicCache" effect to store the result of a computation.  On 
first run it will create the cache and store the result but when you run it again, it will load
the result from the cache.

## Notes
* You should be able to get everything you need by just importing the 
[Knit.Report](https://github.com/adamConnerSax/knit-haskell/blob/master/src/Knit/Report.hs) 
module.  This re-exports the main functions for "knitting" documents and re-exports 
all the required functions to input the supported fragment types and create/write Html, as well as various utilties and
combinators for logging, using the cache facility, or throwing errors.
* This uses [polysemy](https://github.com/isovector/polysemy#readme) for its effect management rather than mtl.  
Polysemy's inference and performance are greatly improved if you enable the [polysemy-plugin](https://hackage.haskell.org/package/polysemy-plugin),
which involves:
1. adding "polysemy-plugin" in build-depends and
2. Add "ghc-options: -fplugin=Polysemy.Plugin" to your package configuration.
Pandoc effects and writer effects for document building are also provided.
* Polysemy is capable of "absorbing" some mtl-style monad constraints.  This is demonstrated in
[RandomExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/RandomExample.hs#L113) and
composable absorbers for MonadReader, MonadWriter, MonadState and MonadError
can be found in the [polysemy-zoo](https://github.com/isovector/polysemy-zoo).
* Pandoc templates are included for HTML output.  See the examples for how to access them
or specify others.
* If you use knit-haskell via an installed executable, it will find the templates that 
cabal installs.  But if you use from a local build directory and use "cabal new-" or "cabal v2-"
style commands, you will need to run the executable via some "cabal v2-" command as well, e.g.,
"cabal v2-run" (but not "cabal v2-exec") otherwise the 
templates--installed in the nix-style-build store--won't be found.
* Though you can theoretically output to any format Pandoc can 
write--and it would be great to add some output formats!--some 
features only work with some output formats. 
My goal was the production of Html and that is the only output format that supports the hvega charting 
since hvega itself is just a wrapper that builds javascript to render in a browser.  
And so far that is the only supported output format.

* This is very much a WIP. So it's rough around the edges and in the middle.  
If you find it useful but have suggestions, please submit issues on github.

* I'm very interested in adding to the "zoo" of input fragments.  Any PRs of that sort would be most welcome!
* I'm also interested in widening the possible output types--currently only HTML is supported--but 
that is quite limited now by hvega which only works in html output.  
But support could be added for other output types if hvega input is not required.

[travis]:        <https://travis-ci.org/adamConnerSax/knit-haskell>
[travis-badge]:  <https://travis-ci.org/adamConnerSax/knit-haskell.svg?branch=master>
[hackage]:       <https://hackage.haskell.org/package/knit-haskell>
[hackage-badge]: <https://img.shields.io/hackage/v/knit-haskell.svg>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/knit-haskell.svg>
[hackage-deps]: <http://packdeps.haskellers.com/feed?needle=knit-haskell>
