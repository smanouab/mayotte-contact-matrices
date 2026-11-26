# MayCov — Mayotte contact matrices

Replication code accompanying *Estimating age-specific contact matrices from
a short survey in Mayotte*, manuscript V12. The archive contains one
executable R script. See [inputs](data/README.md) for the workbooks.

## Run

From R, install the dependencies once:

```r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "survey",
                   "patchwork", "numDeriv", "scales"))
```

From a terminal opened in this directory:

```sh
Rscript reproduce_maycov.R "data/BDD_FINALE_REDRESS_POND_VF(9).xlsx" "data/Donnees_Commune_INSEE(9).xlsx" "data/SERIE-COVID-Mayotte(1).xlsx"
```

The defaults are 1,000 household bootstrap draws and 300 Monte Carlo
replicates per simulation setting. The seed is 2021; the household bootstrap
uses seed 2022. The last three optional arguments specify the output directory,
the number of bootstrap draws and the number of simulation replicates.
For an initial quick check of the stochastic stages:

```sh
Rscript reproduce_maycov.R "data/BDD_FINALE_REDRESS_POND_VF(9).xlsx" "data/Donnees_Commune_INSEE(9).xlsx" "data/SERIE-COVID-Mayotte(1).xlsx" results-check 20 5
```

The divergence fits and SEIR optimizations still run in this check.
Run the default command for manuscript-scale intervals and simulation summaries.

## Outputs

| Manuscript item | Output under `results/` |
| --- | --- |
| Figures 1–2 | `figures/figure_01_heaping.*`, `figures/figure_02_sensitivity.*` |
| Figure 3 | `figures/figure_03_reciprocity.*` and `tables/figure_03_reciprocity.csv` |
| Figure 4 | `figures/figure_04_matrices.*` and `tables/contact_matrix_*.csv` |
| Figure 5 | `figures/figure_05_grid.*`, `tables/serology_grid.csv` and `tables/serology_grid_minima.csv` |
| Figure 6 | `figures/figure_06_serology.*`, `tables/figure_06_*.csv` and `tables/serology_paired_contrasts.csv` |
| Figures 7–9 | `figures/figure_07_dpd_theory.*`, `figures/figure_08_serology.*`, `figures/figure_09_seir.*` |
| Tables 1–14 | `tables/table_01.csv` through `tables/table_14.csv`. TeX versions are also exported for the tables retained from the original R notebook. |
| Exclusion sensitivity and manuscript checks | `tables/sensitivity_excluded.csv` and `tables/verification_V12.csv` |
| R and package versions | `session_R.txt` |

Figures are exported as PDF and PNG. Table 5 contains numeric columns.
Several CSVs inherited from the notebook preserve the displayed table cells
with estimates and confidence intervals formatted together.

## Reproduction status

This script brings together the survey and supplementary analyses from the
original R notebook and ports the joint serological grid and paired bootstrap
to R. The grid starts the final-size iteration from one and fits above the
epidemic threshold as described in V12. The same resampled households are
used across candidate matrices. Pointwise percentile intervals and Monte
Carlo results can differ from the manuscript because the stochastic runs use
R's random-number generator and serial execution. Graphics are regenerated
with ggplot2.

The aggregate CSVs under `reference/` are comparison outputs and are never
read as inputs by the R script. `grid_point_check.csv` was independently
recomputed from the supplied workbooks using the original Python analysis
extended over kappa. The other reference CSVs come from the supplied
revision archive and use its original numbering. The checks confirmed
3,396 complete questionnaires, 3,002 E100 and 2,969 E50, and the published
joint grid minima at kappa 1 and 2. Full execution of the consolidated R
script remains to be verified in an environment with R installed. Compare
`tables/verification_V12.csv` against the manuscript after running.
