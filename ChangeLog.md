v 0.3.0.0 
* Added exports of "Colonnade" and "Text.Blaze.Colonnade" to "Knit.Report.Input.Table.Colonnade"
* Added "knitError" function to Knit.Report to allow user throwing of errors.  These will become PandocSomeError and handled as a PandocError.
* Lowered bound on containers (to 0.5.0) to accomodate use of Plots.
* Added Knit.Report.Input.Visualization.Diagrams.  Adds Diagrams from diagrams-lib via SVG backend.
* Added some more re-exports (Colonnade, Diagrams.Prelude) from Knit.Report to simplify imports on use.
* Added an optional figure caption argument to Hvega and Diagrams inputs. (this is a breaking change since it requires another argument).
* Added a state effect wrapper to facilitate getting unused ids for figures, etc.
* Changed id argument for Hvega and Diagrams visualizations to "Maybe Text" from Text.  Will use built-in facilities for unique ids when set to Nothing.
* Added KnitOne and KnitMany constraint-type-aliases to Knit.Report to simplify constraining doc producing functions.
* Updated examples.  Added a diagrams example to SimpleExample and fixed id and caption arguments throughout.
* Bumped upper bounds (network)

v 0.2.0.0
* Documentation Fixes
* Added export of newPandoc and NamedDoc to Knit.Report to facilitate multi-doc use.
* Removed redundant imports from Knit.Report.Input.Table.Colonnade
* Added a multi-doc example
* (internal) Removed odd, and no longer necessary, "LastMember" constraint from knit functions. 
* Updated effects for polysemy 1.2

v 0.1.0.0  
* Initial version

