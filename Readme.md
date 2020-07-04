# knit-haskell v0.8.0.0

[![Build Status][travis-badge]][travis]
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

## Breaking Changes
To move from v0.7.x.x to v0.8.x.x requires a change in how configuration parameters given to ```knit-html``` and ```knit-htmls``` are 
handled: they are now all placed inside a ```KnitConfig```.  
It's a trivial change to make and should make the configuration more future-proof as long as you build your ```KnitConfig``` like, e.g., 
```haskell
myConfig :: KnitConfig
myConfig = (defaultKnitConfig $ Just "myCache") { outerLogPrefix = Just "MyReport"}
```

Also note that newer versions of Pandoc (2.9+) have their own breaking changes.  knit-haskell can be 
compiled against these as
well as the older versions, but there are some major changes which may affect you should you use any of the pandoc 
functions directly.  In particular, Pandoc has now switched to using ```Text``` instead of ```String``` for
most (all ?) things.

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

As of version 0.8.0.0, the effect stack includes a couple of new features. 
Firstly, an "Async" effect ([Polysemy.Async](https://hackage.haskell.org/package/polysemy-1.2.3.0/docs/Polysemy-Async.html)) 
for running computations concurrently. Combinators for launching a concurrent action (```async```), 
awaiting (```await```) it's result and running some traversable structure of concurrent actions
(```sequenceConcurrently```) are re-exported via ```Knit.Report```.  NB: Polysemy returns a ```Maybe a``` where
the traditional interface returns an ```a```. 
From the docs "The Maybe returned by async is due to the fact that we can't be sure an Error effect didn't fail locally."

A persistent (using memory and disk) cache for "shelving" the results of computations during and between runs.  
Using the default setup, anything which has
a ```Serialize``` instance from the [cereal](https://hackage.haskell.org/package/cereal) 
package can be cached. You can use a different serializer if you so choose, but you will have write
a bit of code to bridge the serializer's interface and, depending on what the serializer encodes to, 
you may also  have to write your own persistence functions for saving/loading that type to/from disk.  See 
```Knit.Effect.Serialize``` and ```Knit.Effect.AtomicCache``` for details.

If you use the cache, and you are running in a version-controlled directory,
you probably want to add your cache directory, specified in ```KnitConfigure``` and defaulting to
".knit-haskell-cache", to ".gitignore" or equivalent.

Once data has been loaded from disk/produced once, it remains available in memory (in serialized form) via its key. 
The cache handles multi-threading gracefully.  The in-memory cache is stored in a TVar so only one thread 
may make requests at a time.
If multiple threads request the same item, one not currently in-memory--a 
relatively common pattern if multiple analyses of the same data are 
run asynchronously--the first request will fetch or create the data and the rest will block until the first one
gets a result, at which point the blocked threads will received the now in-memory data and proceed.

Data can be put into the cache via ```store```, and retrieved via ```retrieve```. Retrieval from cache
does not actually retrieve the data, but a structure with a time-stamp 
([```Maybe Time.Clock.UTCTime```](https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Clock.html#t:UTCTime))
and a monadic computation which can produce the data:
```haskell
data WithCacheTime m a where
  WithCacheTime :: Maybe Time.UTCTime -> m a -> WithCacheTime m a
```

To get the data from a ```WithCacheTime``` you can use functions from the 
library to "ignore" the time and bind the result:
```ignoreCacheTime :: WithCacheData m a -> m a```
or
```haskell
ignoreCacheTimeM :: m (WithCacheData m a) -> ma 
ignoreCacheTimeM = join . ignoreCacheTime
```

Though direct storage and retrieval is useful, typically, one would use the cache to store 
the result of a long-running computation so it need only be run once.  This pattern is 
facilitated via 
```
retrieveOrMake :: k -> WithCacheTime m b -> (b -> m a) -> m (WithCacheTime m a)
```

which takes a key, a set of dependencies, of type ```b```, with a time-stamp,
a (presumably expensive) function taking those dependencies and producing a 
time-stamped monadic computation for the desired result. If the requested
data is cached, the time stamp (modification time of the file in cache, more or less) 
is compared to the time-stamp on the dependencies.  As long as the dependencies are older
than the cached data, an action producing the cached result is returned.  If there is no
data in the cache for that key or the in-cache data is too old, the action producing the dependencies
is "run" and those dependencies are fed to the computation given, producing the data and 
caching the result.

NB: The returned monadic computation is *not* simply the result of applying the dependencies to
the given function.  That computation is run, if necessary, in order to produce the data, which
is then serialized and cached.  The returned monadic computation is either the data produced
by the given computation, put into the monad via ```pure```, or the result pulled from the 
cache *before* it is deserialized.  Running the returned computation performs the deserialization
so the data can be used.  This allows checking the time-stamp of data without deserializing it
in order to make the case where it's never actually used more efficient.  

```WithCacheTime``` is an applicative functor, which facilitates its primary use, to store 
a set of dependencies *and* the latest time at which something which depends on them could
have been computed and still be valid. As an example, suppose you have three long-running
computations, the last of which depends on the first two:
```haskell
longTimeA :: AData
longTimeB :: BData
longTimeC :: AData -> BData -> CData
```

You might approach caching this sequence thusly:
```
cachedA <- retrieveOrMake "A.bin" (pure ()) (const longTimeA)
cachedB <- retrieveOrMake "B.bin" (pure ()) (const longTimeB)
let cDeps = (,) <$> cachedA <*> cachedB
cachedC <- retrieveOrMake "C.bin" cDeps $ \(a, b) -> longTimeC a b
```
and each piece of data will get cached when this is first run.  Now suppose you change the computation
```longTimeA```.  You realize that the cached data is invalid, so you delete "A.bin" from the
cache.  The next time this code runs, it will recompute and cache the result of ```longTimeA```, 
load the ```BData```  (serialized) from cache, see that
the cached version of ```CData``` is out of date, 
and then deserialize ```BData```, and use it and the new ```AData``` 
to recompute and re-cache ```CData```.  This doesn't eliminate the 
need for user intervention: the user still had to manually delete "A.bin" to force re-running ```longTimeA```, 
but it handles the downstream work of tracking the uses of that data and recomputing where required. 
I've found this extremely useful.

Entries can be cleared from the cache via ```clear```.

The cache types are flexible:

-The default key type is ```Text``` but you may use anything with an ```Ord``` and ```Show``` 
instance (the latter for logging).  The persistence layer will need to be able to turn the key
into a key in that layer, e.g., a ```FilePath```.

-The default serializer is the 
[cereal](https://hackage.haskell.org/package/cereal) package but you may
use another (e.g., the 
[binary](https://hackage.haskell.org/package/binary-0.8.7.0/docs/Data-Binary.html) 
package or 
[store](https://hackage.haskell.org/package/store).

-The default in-memory storage is a [streamly](https://hackage.haskell.org/package/streamly) 
[array](https://hackage.haskell.org/package/streamly-0.7.2/docs/Streamly-Memory-Array.html) of bytes 
([```Word8```](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Word.html)) but this can
also be changed.

To change these, the user must provide a serializer capable of serializing any data-type to be stored into
the desired in-memory storage type,  and a persistence layer which can persist that in-memory type.

Please see  [CacheExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample.hs) for an example using
the default serializer and in-memory storage type. 
See [CacheExample2](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample2.hs) for an
identical example, but with a custom serializer (based on the 
[store](https://hackage.haskell.org/package/store)) package and using strict ```ByteStreams``` as the in-memory cache type. 

Notes:
1. Using Streamly requires some additional support for both Cereal and Polysemy.  The encoding/decoding 
for Cereal are in this library, in ```Streamly.External.Cereal```. The Polysemy issue is more complex.
Since concurrent streamly streams can only be run over a monad with instances of ```MonadCatch``` and 
```MonadBaseControl```. The former is 
[complex](https://hackage.haskell.org/package/polysemy-zoo-0.7.0.0/docs/Polysemy-ConstraintAbsorber-MonadCatch.html) 
in Polysemy and the latter impossible, [for good reason](https://github.com/polysemy-research/polysemy/issues/73). 
So knit-haskell contains some helpers for Streamly streams: basically a wrapper over IO which allows use of
knit-haskell logging.  Concurrent streaming operations can be done over this monad and then, once the stream
is serial or the result computed, that monad can be lifted into the regular knit-haskell Polysemy stack.  
See ```Knit.Utilities.Streamly``` for more details.

2. ```Knit.Report```, the main import, provides constraint helpers to use these effects.  The clearest way to
see how they are used is to look at the examples.  The Cache effects are split into their own constraint helper
because they have type-parameters and thus add a lot of inference complications.  If you don't need them in a 
function, you need not specify them.  Some of that inference can be improved using the 
[polysemy-plugin](https://hackage.haskell.org/package/polysemy-plugin) in the source files where you
have issues.  Otherwise, you may need to use 
[type applications](https://gitlab.haskell.org/ghc/ghc/-/wikis/type-application)
when calling some functions in 
```Knit.Report.Cache```.

- ```KnitEffects r```: All effects except caching or the addition of document fragments.  
Includes logging, error handling, and any direct use of funtions in PandocMonad or IO.  
This is often useful to wrap computations that need logging and perhaps IO but that don't 
write any part of your document.  This is a constraint on the polysemy ```EffectRow```, 
```r```, typically part of the return type of the function: ```Sem r a```.

- ```CacheEffects sc ct k r```: Effects related to caching. ```sc``` is the Serializer constraint, e.g.,
```Serialize``` for cereal (the default) or ```Store``` for store. ```ct``` is the in-memory data type,
```Streamly.Memory.Array.Array Word8``` by default but some flavor of ```ByteStream``` could also make sense. 
```k``` is the key type, ```Text``` by default but anything with ```Ord``` and ```Show``` instances will do.

- ```CacheEffectsD r```: provides the same effects as ```CacheEffects``` but sets the various types to their defaults.

- ```KnitOne r```: ```KnitEffects r``` and the additional effect required to write Pandoc fragments.

- ```KnitMany r```: ```KnitEffects r``` and the additional effects required to write multiple Pandoc 
documents.


## Supported Inputs
* [markdown](https://pandoc.org/MANUAL.html#pandocs-markdown)
* HTML ([blaze](http://hackage.haskell.org/package/blaze-html), [lucid](http://hackage.haskell.org/package/lucid) or Text)
* [latex](https://en.wikipedia.org/wiki/LaTeX)
* [colonnade tables](https://hackage.haskell.org/package/colonnade)
* [hvega](http://hackage.haskell.org/package/hvega) visualizations (via [blaze](http://hackage.haskell.org/package/blaze-html) HTML) 
* [Diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) (via Diagrams SVG backend, inserted as HTML) 

## Examples
There are a few [examples](https://adamconnersax.github.io/knit-haskell/) in the "examples" directory.  
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
Similar to "SimpleExample" but uses the "AtomicCache" effect to store the result of a computation. 
Demonstrates the behavior of the cache when multiple threads attempt to access the same item--the first
thread loads/creates the data while the other blocks until the data is in-memory.  Also demonstrates
use of time-stamps to force rebuilding when tracked inputs change.
* [CacheExample2](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample2.hs).  
Similar to "CacheExample" but implements and uses a different serializer and persistence layer than the
default.


## Notes
* You can often get everything you need by just importing the 
[Knit.Report](https://github.com/adamConnerSax/knit-haskell/blob/master/src/Knit/Report.hs) 
module. It's meant to be "batteries included."  
This re-exports the main functions for "knitting" documents and re-exports 
all the required functions to input the supported fragment types and create/write Html, 
as well as various utilties and
combinators for logging, using the cache facility, or throwing errors.
* This uses [polysemy](https://github.com/polysemy-research/polysemy#readme) for its effect management rather than mtl.  
Polysemy's inference (and performance?) are greatly improved if you enable 
the [polysemy-plugin](https://hackage.haskell.org/package/polysemy-plugin),
which involves:
1. adding "polysemy-plugin" in build-depends and
2. Add "ghc-options: -fplugin=Polysemy.Plugin" to your package configuration or
```{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}``` at the top of any source file
with inference issues. 

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
