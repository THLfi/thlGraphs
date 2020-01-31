# thlGraphs 1.1.0

New improved version of thlGraphs released!
Plots can now be built with THL's own layering functions.
Wrappers use sensible defaults to match the graphical guidelines. 
This allows more flexibility and options to built all different kinds of plots.


## Breaking changes

The following functions are now replaced and deprecated:

* `thlTheme`-> `theme_thl`
    - defines now much more details
* `thlColors`
    - Replaced now with multiple different functions which allow many ways to 
    refer to the colors.
    - `palette_thl`: refer to a color palette with a name and n
    - `colors_thl`: refer to predefined colors with a name
    - `colorset_thl`: refer to predefined groups of colors
* `thlColorsDisplay`    
    - `plot_colors`: general color plotting function, shows also the hex values
    - `plot_palette_thl`: plots a named color palette
* `thlLinePlot` -> `plot_line_thl` 
    - Uses now other thlGraphs built-in layer functions


## New functions

These new functions can be used just like the common ggplot2 layer elements.

* `geom_point_thl`: sets point type to white filled point and size
* `geom_line_thl`: sets line size
* `geom_col_thl`: sets border color white and column width
* `geom_bar_thl`: sets border color white and column width
* `scale_y_continuous_thl`: sets breaks nicely and formats numbers on breaks, 
removes expanding from y-scale
* `scale_x_continuous_thl`: sets breaks nicely and formats numbers on breaks

The following functions help with other plotting related things.

* `format_thl`: number formatting in different languages
* `waiver_thl`: switch in some functions for using the default values
* `colornames_thl`: list names of predefined colors


## Colors

To make it easies to add new colors and palettes or change existing ones,
thlgraphs uses now internal data to define colors and palettes.



