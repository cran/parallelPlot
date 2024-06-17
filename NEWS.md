# parallelPlot 0.4.0

* Added:
  * new API for `refColumnDim` to specify the column used to determine the color used for each row
  * fire an event when `refColumnDim` is changed
  * new API for `highlightRow` to specify the highlighted row
  * fire an event when a row is highlighted
  * new `adjusting` attribute for `cutoff` event
  * fire an event when an axis orientation is changed
  * fire an event when a row is clicked
  * a named list can also be provided for `categorical` and `cutoffs` arguments
  * a list of column names can also be provided for `inputColumns`, `keptColumns`, `histoVisibility` and `invertedAxes` arguments

* Changed:
  * *breaking change*, if `eventInputId` argument is `NULL`, it means no event is sent (before, it meant a default value was used)
  * *breaking change*, `cutoff` event has a new attribute `adjusting`, `TRUE` when pointer is moving, changing a cutoff (before, the `cutoff` event was only sent once the mouse gesture had been completed)
  * javascript version of the graph is now available in three formats: [iife](https://esbuild.github.io/api/#format-iife), [CommonJS](https://esbuild.github.io/api/#format-commonjs) and [ECMAScript module](https://esbuild.github.io/api/#format-esm)
  * the default background color is changed from grey to white
  * sliders are more attractive (use a frame rather than a central axis; use rounded edges)

* Fixed:
  * the area reserved to the plot is reduced to take into account the height of `controlWidgets`

# parallelPlot 0.3.1

* Added:
  * ability to reorder categories by using the mouse
  * new argument and API `arrangeMethod` to set the method (`fromLeft`, `fromRight`, `fromBoth` or `fromNone`) used to arrange lines position in category boxes
  * new argument and API `categoriesRep` to set the method (`EquallySizedBoxes` or `EquallySpacedLines`) used to calculate the height assigned to each category box
  * when a column axis is inverted, a sign '↓' is added at the beginning of the column header 

* Fixed:
  * `cssRules` argument, styling is lost when `sliderPosition` is changed
  * `cssRules` argument, styling is lost after `saveWidget`

# parallelPlot 0.2.0

* Added:
  * new argument `cssRules` to apply CSS rules
  * new argument and API `invertedAxes` to set orientation of axes
  * new argument `sliderPosition` to set initial position of slider
  * `getPlotConfig` API to have an exported plot with same configuration
  * `controlWidgets` argument to tell if some widgets must be available to control plot
  * display a tooltip when mouse is over a trace
  * new color palettes coming from `d3-scale` (including `viridis`, the default palette used by matlab)

* Fixed:
  * improve spreading when lines go through a category

# parallelPlot 0.1.0

* Added a `NEWS.md` file to track changes to the package.
