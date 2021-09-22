# vsn_shiny operator

##### Description

The `vsn_shiny operator` applies normalization using the VSN method, displays the result in a chart and returns the normalized data.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, y value per cell 

Input parameters| Input parameters are set in the shiny interface 
---|---
`Normalization` | "affine" (dft) apply normalization and transformation or "none", apply transformation only. Set this proporty to "affine" unless you know what you are doing.
`Use reference data`| "FALSE" (dft). When checked this allows the option to use a subset of the data as a reference for normlization.

Output relations|.
---|---
`H`          | numeric, the VSN scaled and transformed data

##### Details

Apply Array Normalization using the VSN method. This can typically be used to correct for overall differences between samples. Applies the VSN R-package from [Bioconductor](https://bioconductor.org/packages/release/bioc/manuals/vsn/man/vsn.pdf).
The operator assumes 1 value per cell, the columns will be scaled towards each other, or towards the reference data when defined.
If you use a data color, the color will be used to stratify the columns into subgroups for which the scaling is performed separately.
