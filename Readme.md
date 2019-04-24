# knit-haskell

[![Build Status][travis-badge]][travis]
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

## Introduction
knit-haskell is the beginning of an attempt to emulate parts of the RMarkdown/knitR experience in haskell. The idea is to be able to build HTML (or, perhaps, some other things [Pandoc](http://hackage.haskell.org/package/pandoc) can write) inside a haskell executable.  This package has some wrapping around Pandoc and the [PandocMonad](http://hackage.haskell.org/package/pandoc-2.7.2/docs/Text-Pandoc-Class.html#t:PandocMonad) as well as some logging facilities and some support for inserting [hvega](http://hackage.haskell.org/package/hvega) visualizations.  All of that is handled via writer-like monads so additions to the documents can be interspersed with regular haskell code.

## Features
* Insert [markdown](https://pandoc.org/MANUAL.html#pandocs-markdown), html ([blaze](http://hackage.haskell.org/package/blaze-html), [lucid](http://hackage.haskell.org/package/lucid) or Text), [latex](https://en.wikipedia.org/wiki/LaTeX), or [colonnade tables](https://hackage.haskell.org/package/colonnade) into a document.
* Insert [hvega](http://hackage.haskell.org/package/hvega) visualizations (via [blaze](http://hackage.haskell.org/package/blaze-html) HTML) into a document

## Examples
There are a few examples in the "examples" directory.  
* [SimpleExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/SimpleExample.hs) demonstrates the bare bones features of the library.  Creating a document from a few fragments and then "knitting" it into HTML text and writign that to a file.
* [MtlExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/MtlExample.hs) demonstrates the same simple features as above, but runs them atop an example mtl stack, allowing access to the mtl stack's functionality during document assembly, to show how you can do that.
* [RandomExample](https://github.com/adamConnerSax/knit-haskell/blob/master/examples/RandomExample.hs) builds on the mtl example to show how you can also add a polysemy effect to your document-building. This one also demonstrates a use of colonnade for adding a formatted table to the document.

## Notes
* You should be able to get everything you need by just importing the "Knit.Report" module.  That has the main functions for "knitting" documents from fragments and re-exports all the required functions to input the supported fragment types and export Html.
* This uses [polysemy](https://github.com/isovector/polysemy#readme) for its effect management rather than mtl.  Effects are provided for logging and generating [random-fu](http://hackage.haskell.org/package/random-fu) style random numbers.  If the ```Random``` effect is included in the effect list, the effect monad is an instance of [MonadRandom](http://hackage.haskell.org/package/random-fu-0.2.7.0/docs/Data-Random.html#t:MonadRandom). Pandoc effects and writer effects for document building are also provided.

* Though you can theoretically output to any format Pandoc can write--and it would be great to add some output formats!--some features only work with some output formats. 
My goal was the production of Html and that is the only output format that supports the hvega charting since hvega itself is just a wrapper that builds javascript to render in a browser.  And so far that is the only supported output format.

* This is very much a WIP. So it's rough around the edges and in the middle.  If you find it useful but have suggestions, please submit issues on github.


[travis]:        <https://travis-ci.org/adamConnerSax/knit-haskell>
[travis-badge]:  <https://travis-ci.org/adamConnerSax/knit-haskell.svg?branch=master>
[hackage]:       <https://hackage.haskell.org/package/knit-haskell>
[hackage-badge]: <https://img.shields.io/hackage/v/knit-haskell.svg>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/knit-haskell.svg>
[hackage-deps]: <http://packdeps.haskellers.com/feed?needle=knit-haskell>
