# Figure Code

Author: Ting Zhai

## Overview

This repository contains R scripts for the main and supplementary figures.

## Requirements

R >= 4.4 with the following packages:

``` r
install.packages(c(
  "ggh4x", "ggplot2", "ggrepel", "ggtext", "patchwork",
  "ragg", "scales", "svglite"
))
```

The scripts were run with R 4.4.1, ggplot2 4.0.2, ggh4x 0.3.1, ggrepel 0.9.5,
ggtext 0.1.2, patchwork 1.3.1, ragg 1.3.2, scales 1.4.0 and svglite 2.1.3.

## Inputs

Input data are not distributed in this repository. Place the input CSV files
in `data/`; figures are written to `output/`. Each script header lists the
files it reads, and scripts stop with an explicit message when a required file
or column is absent.

Each panel reads one or more data files and, where needed, up to three
supporting files:

| File | Columns | Contents |
|---|---|---|
| `Figure_<panel>.csv`, `Figure_<panel>_<part>.csv` | Panel-specific | Values to plot |
| `Figure_<panel>_text.csv` | `key`, `value` | Titles, axis and legend text |
| `Figure_<panel>_levels.csv` | `variable`, `code`, `label` | Category order and display labels |
| `Figure_<panel>_settings.csv` | `key`, `value` | Numeric settings; vectors separated by `;` |

`00_helpers.R` also reads `Labels_series_levels.csv` and
`Labels_groups_levels.csv`, in the levels format. Numeric fields must be stored
as numeric values.

## Run

Run any script from the repository root; it loads `00_helpers.R`
automatically.

``` sh
Rscript fig1.R
Rscript fig2.R
Rscript fig3.R
Rscript fig4.R
Rscript fig5.R
Rscript supp_fig3.R
Rscript supp_fig4.R
Rscript supp_fig5.R
Rscript supp_fig6.R
Rscript supp_fig7.R
Rscript supp_fig8.R
```

Outputs are written as SVG and PNG.

## File manifest

| Script | Figure |
|---|---|
| `00_helpers.R` | Shared settings and functions |
| `fig1.R` | Figure 1 |
| `fig2.R` | Figure 2 |
| `fig3.R` | Figure 3 |
| `fig4.R` | Figure 4 |
| `fig5.R` | Figure 5 |
| `supp_fig3.R` | Supplementary Figure 3 |
| `supp_fig4.R` | Supplementary Figure 4 |
| `supp_fig5.R` | Supplementary Figure 5 |
| `supp_fig6.R` | Supplementary Figure 6 |
| `supp_fig7.R` | Supplementary Figure 7 |
| `supp_fig8.R` | Supplementary Figure 8 |

Figure panels not listed and multipanel layouts are assembled separately.
