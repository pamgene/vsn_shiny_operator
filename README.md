# vsn_shiny operator

##### Description

The `vsn_shiny operator` applies normalization using the VSN method and displays the result in a chart.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, y value per cell 

Input parameters|.
---|---
`Normalization` | "affine" (dft) apply normalization and transformation or "none", apply transformation only. Set this proporty to "affine" unless you know what you are doing.

Output relations|.
---|---
`H`          | numeric, the VSN scaled and transformed data

##### Details

Apply Array Normalization using the VSN method. This can typically be used to correct for overall differences between samples. Applies the VSN R-package from [Bioconductor](http://bioconductor.org/packages/release/bioc/vignettes/vsn/inst/doc/vsn.pdf).
The operator assumes 1 value per cell, the columns will be scaled towards each other.
If you use a data color, the color will be used to stratify the columns into subgroups for which the scaling is performed separately.
