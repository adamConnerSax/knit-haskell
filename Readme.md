# knit-haskell
## Introduction
knit-haskell is the beginning of an attempt to emulate parts of the RMarkdown/knitR experience in haskell. The idea is to be able to build HTML (or, perhaps, some other things [Pandoc](http://hackage.haskell.org/package/pandoc) can write) inside a haskell executable.  This package has some wrapping around Pandoc and the [PandocMonad](http://hackage.haskell.org/package/pandoc-2.7.2/docs/Text-Pandoc-Class.html#t:PandocMonad) as well as some logging facilities and some support for inserting [hvega](http://hackage.haskell.org/package/hvega) visualizations.  All of that is handled via writer-like monads so additions to the documents can be interspersed with regular haskell code.

## Features
* Insert markdown, html ([blaze](http://hackage.haskell.org/package/blaze-html), [lucid](http://hackage.haskell.org/package/lucid) or Text) or [latex](https://en.wikipedia.org/wiki/LaTeX) into a document.
* Insert [hvega](http://hackage.haskell.org/package/hvega) visualizations (via [blaze](http://hackage.haskell.org/package/blaze-html) HTML) into a document


## Notes
* This uses [freer-simple](http://hackage.haskell.org/package/freer-simple) for its effect management rather than mtl.  This may well be converted to use [polysemy](https://github.com/isovector/polysemy#readme) in the near future.  Effects are provided for logging and generating [random-fu](http://hackage.haskell.org/package/random-fu) style random numbers.  If the ```Random``` effect is included in the effect list, the effect monad is an instance of [MonadRandom](http://hackage.haskell.org/package/random-fu-0.2.7.0/docs/Data-Random.html#t:MonadRandom). Pandoc effects and writer effects for document building are also provided.

* Though you can theoretically output to any format Pandoc can write, some features only work with some output formats. My goal was the production of Html and that is the only output format that supports the hvega charting since hvega itself is just a wrapper that builds javascript to render in a browser.

* This is very much a WIP. So it's rough around the edges and in the middle.  If you find it useful but have suggestions, please submit issues on github.


