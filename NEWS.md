# parallelPlot 0.3.1

* Added:
  * ability to reorder categories by using the mouse
  * new argument and API 'arrangeMethod' to set the method ('fromLeft', 'fromRight', 'fromBoth' or 'fromNone') used to arrange lines position in category boxes
  * new argument and API 'categoriesRep' to set the method ('EquallySizedBoxes' or 'EquallySpacedLines') used to calculate the height assigned to each category box
  * when a column axis is inverted, a sign '↓' is added at the beginning of the column header 

* Fixed:
  * 'cssRules' argument, styling is lost when 'sliderPosition' is changed
  * 'cssRules' argument, styling is lost after 'saveWidget'

# parallelPlot 0.2.0

* Added:
  * new argument 'cssRules' to apply CSS rules
  * new argument and API 'invertedAxes' to set orientation of axes
  * new argument 'sliderPosition' to set initial position of slider
  * 'getPlotConfig' API to have an exported plot with same configuration
  * 'controlWidgets' argument to tell if some widgets must be available to control plot
  * display a tooltip when mouse is over a trace
  * new color palettes coming from 'd3-scale' (including 'viridis', the default palette used by matlab)

* Fixed:
  * improve spreading when lines go through a category

# parallelPlot 0.1.0

* Added a `NEWS.md` file to track changes to the package.
