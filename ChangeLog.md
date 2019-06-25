v 0.6.0.1 - (Released 2019-06-21)
* Moved the Pandoc default templates so they will be installed where Pandoc expects them. This is hard to test!

v 0.6.0.0 - (Released: 2019-06-19)
* updated to use polysemy-zoo version of constraint absorbers in PandocMonad.
* Hackage download should now include templates and associated css
* Added mkPandocWriterConfig and addCss function in Knit.Report.Output to handle loading things in 
the included directory, which may be installed in a platform independent way.
* Added two more html templates, pandoc-bootstrap-KH.html and pandoc-adaptive-bootstrap-KH.html
with pandoc-bootstrap.css and used them in some of the examples.
* Added the default html templates into the pandoc-templates directory where Pandoc will expect
them if the given template fails.
* Reorganized data directory with "pandoc-data" "knit-haskell-templates" and "knit-haskell-css"
* Added output helpers for Pandoc results which are lazy text (html).  These create the
required parent directories if necessary. See examples for details.
* Removed ```Knit.Effect.RandomFu``` and modified the example which uses it to use the
"polysemy-RandomFu" package instead.

v 0.5.0.0
* Added plots example back since there is a version of plots on hackage with 
a relaxed upper bound on containers.
* Changed the Doc effect so that instead of Text name, it carries a polymorphic info type.  
This is then specified in the Pandoc case to be a Text name and set of template var overrides.
This creates several other changes and will break any multi-doc examples since now 
"newPandoc" takes a first argument of the type ```PandocInfo``` (which is just a 
product of a ```Text``` and a ```Map String String```).
* raised lower bound and relaxed upper bound on polysemy

v 0.4.0.0 
* Added  
```absorbPandocMonad :: PandocEffects r => (forall m. PandocMonad m => m a) -> Sem r a``` 
to ```Knit.Effect.PandocMonad```
* Removed (orphan) instances: ```PandocMonad (Sem r)``` and 
```MonadError PandocError (Sem r)``` in favor  of using 
```absorbPandocMonad``` when required.
* Deprecated name "Random" in favor of "RandomFu" for clarity and 
eventual consistency with Polysemy
* Added 
```absorbMonadRandom :: Member RandomFu r => (forall m. MonadRandom m => m a) -> Sem r a``` 
to allow some
interoperation with actions constrained by ```MonadRandom```
* Removed orphan ```Random.MonadRandom``` instance from 
```Knit.Effect.RandomFu``` because orphan instances are bad.
* Changed return type of ```Knit.Report.knitError``` to ```Sem r a``` 
(from ```Sem r ()```)
* Bumped lower bound on polysemy-plugin (because of a buggy version)
* Bumped lower bound on polysemy
* Removed plots example in "SimpleExample" and added a diagrams one.  
Will add plots back once a version issue  with containers is resolved.

v 0.3.0.1
* Examples were "executables" and are now "tests" so that depending on 
knit-haskell does not pull in the dependencies of the examples.

v 0.3.0.0 
* Added exports of "Colonnade" and "Text.Blaze.Colonnade" to 
"Knit.Report.Input.Table.Colonnade"
* Added "knitError" function to Knit.Report to allow user throwing of errors.  
These will become PandocSomeError and handled as a PandocError.
* Lowered bound on containers (to 0.5.0) to accomodate use of Plots.
* Added Knit.Report.Input.Visualization.Diagrams.  
Adds Diagrams from diagrams-lib via SVG backend.
* Added some more re-exports (Colonnade, Diagrams.Prelude) 
from Knit.Report to simplify imports on use.
* Added an optional figure caption argument to Hvega and Diagrams inputs. 
(this is a breaking change since it requires another argument).
* Added a state effect wrapper to facilitate getting unused ids for figures, etc.
* Changed id argument for Hvega and Diagrams 
visualizations to "Maybe Text" from Text.  
Will use built-in facilities for unique ids when set to Nothing.
* Visualization "addXXX" functions now return the figure id (as "Sem r Text") 
so it can be referred to elsewhere.
* Added KnitOne and KnitMany constraint-type-aliases to Knit.Report 
to simplify constraining doc producing functions.
* Updated examples.  Added a diagrams example to SimpleExample 
and fixed id and caption arguments throughout.
* Bumped upper bounds (network)

v 0.2.0.0
* Documentation Fixes
* Added export of newPandoc and NamedDoc to Knit.Report to 
facilitate multi-doc use.
* Removed redundant imports from Knit.Report.Input.Table.Colonnade
* Added a multi-doc example
* (internal) Removed odd, and no longer necessary, "LastMember" 
constraint from knit functions. 
* Updated effects for polysemy 1.2

v 0.1.0.0  
* Initial version

