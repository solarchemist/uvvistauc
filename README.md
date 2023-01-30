# uvvistauc

<!-- badges: start -->
[![DOI](https://raw.githubusercontent.com/solarchemist/uvvistauc/master/man/figures/badge-doi.svg)](https://doi.org/10.5281/zenodo.5814054)
[![Vignette](https://raw.githubusercontent.com/solarchemist/uvvistauc/master/man/figures/badge-vignette.svg)](https://htmlpreview.github.io/?https://github.com/solarchemist/uvvistauc/blob/master/doc/intro.html)
[![Shiny app](https://raw.githubusercontent.com/solarchemist/uvvistauc/master/man/figures/badge-shiny.svg)](https://shiny.solarchemist.se/uvvistauc-reactive)
<!-- badges: end -->

This R package calculates the optical band gap of semiconductors from UV-Vis spectra
using Tauc plots.

![Our Tauc fitting algorithm, animated](https://raw.githubusercontent.com/solarchemist/uvvistauc/master/man/figures/animation.gif)

The Tauc plot is commonly used for semiconductors with either direct or indirect,
forbidden or allowed, band gaps; such as oxides, sulfides, and many others.

A Tauc plot is usually performed manually, that is, the band edge region is identified
visually, and then a linear fit to the linear part of the band edge is performed manually
and the intersect with the abscissa is calculated to arrive at the optical band gap.

Here I have developed a semi-automatic algorithm, that only requires you to supply
four *x* values: two values that define the mostly linear part on the low-energy side
of the band edge, and two values that define the high-energy side of the band edge.

This algorithm returns the calculated optical band gap,
the fitted Tauc line, as well as the adjusted R-square of the Tauc fit and
its number of fitted data points.

Note that this package expects you to supply your UV-Vis data as
absorbance (unitless) *vs* energy (in electronvolts).

So far, this package has only been tested for ZnO.
If you use it with other materials, I would love it if you would let me know!

To learn more about how this package implements the Tauc algorithm,
**[read the vignette](https://htmlpreview.github.io/?https://github.com/solarchemist/uvvistauc/blob/master/doc/intro.html)**.

This package also includes a dataset with a time-series of UV-Vis spectra from an experiment
on growing colloidal ZnO nanoparticles.


## Install this package

To use this package, install it directly from this repo:

```
install.packages("remotes")
remotes::install_github("solarchemist/uvvistauc")
```

If you encounter bugs or have questions
[please open an issue](https://github.com/solarchemist/uvvistauc/issues).


## Develop this package

Check out the source code from this repo:
```
git clone https://github.com/solarchemist/uvvistauc.git
```

I suggest the following package rebuild procedure (in RStudio IDE):

+ Run `devtools::check()` (in the console or via the **Build** pane).
  Should complete with one note about "installed package size" being large
  and no warnings or errors (be warned, the step `creating vignettes ...` may
  take several minutes to finish):
```
── R CMD check results ───────────────────────────────────────────── uvvistauc 0.5.1.9000 ────
Duration: 6m 0.1s

❯ checking installed package size ... NOTE
    installed size is 10.0Mb
    sub-directories of 1Mb or more:
      doc    8.0Mb
      help   1.8Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```
+ Run `devtools::build_vignettes()` in the console. This recompiles the vignettes
  and populates the `doc/` directory.
+ Manually remove the line `doc` from `.gitignore` (the build step keeps adding it).



## Contributions welcome

If you would like to add another example dataset, or report errors in the code
of offer improvements or any other contribution, you're very welcome.
Feel free to [contact me](https://solarchemist.se/contact/).



## Citation

To cite uvvistauc in publications use:

Taha Ahmed (2022). The uvvistauc package: calculate optical band gap of semiconductors
from UV-Vis spectra using the Tauc method.
DOI: [10.5281/zenodo.5814054](https://doi.org/10.5281/zenodo.5814054).

Or see the `CITATION.cff` ([citation file format](https://citation-file-format.github.io/))
file in this repo or in the sidebar.
