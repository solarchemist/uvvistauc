# uvvistauc

This R package calculates the optical band gap of semiconductors from UV-Vis spectra
using the Tauc plot method.

The Tauc plot is commonly used for semiconductors with either direct or indirect,
forbidden or allowed, band gaps; such as oxides, sulfides, and many others.

A Tauc plot is usually performed manually, that is, the band edge region is identified
visually, and then a linear fit to the linear part of the band edge is performed manually
and the intersect with the abscissa is calculated to arrive at the optical band gap.

Here I have developed a semi-automatic algorithm, that only requires you to supply
four *x* values: two values that define the mostly linear part on the low-energy side
of the band edge, and two values that define the high-energy side of the band edge.

This algorithm returns the calculated optical band gap, as well as the fitted Tauc line
itself.

Note that this package expects you to supply the UV-Vis spectrum as 
absorbance (unitless) vs energy (in electronvolts).

As of yet, this package has only been tested for ZnO. 
If you use it with other materials, I would love it if you would let me know!

To learn more about how this package implements this algorithm in order to calculate
optical band gaps for your UV-Vis spectra, 
[read the vignette](https://htmlpreview.github.io/?https://github.com/solarchemist/uvvistauc/blob/master/doc/intro.html).

This package also includes a dataset with over a 100 UV-Vis spectra in a series from an experiment
on growing colloidal ZnO nanoparticles.


## Install this package

To use this package in your own R installation:

```
install.packages("remotes")
remotes::install_github("solarchemist/periodicdata")
```



## Modify or develop this package

To check out the source code of this package:

```
git clone https://github.com/solarchemist/uvvistauc.git
```



## Contributions welcome

If you would like to add another example dataset, or report errors in the code
of offer improvements or any other contribution, you're very welcome.

Please do so by opening an issue or a pull request.
