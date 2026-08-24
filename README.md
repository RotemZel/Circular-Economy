# Circular Economy: by-product exchange among micro-businesses

Analysis code for a study of by-product exchange among 200 micro-businesses in three towns in Arab society in Israel. The study asks what distinguishes a business that passes its by-products to another firm from one that does not, why a stated willingness to supply so often fails to become practice, and why the receiving side of the exchange stays so much thinner than the supplying side.

The analysis is written in R. Every recoding decision (material groups, container volumes, sector groups, owner roles, size and education labels, age-band midpoints) is held in `inputs/codebook.xlsx`, one sheet per decision, so it can be read and checked without reading any code.

## Data availability

The raw survey file is not published. It was collected from named small businesses under an assurance of confidentiality, and the combination of town, sector and business characteristics would permit individual businesses to be identified.

A de-identified file is published instead, at `shareable/waste_tidy.RData`.
Two changes were made to it:

- the town names are replaced by the labels A, B and C used throughout the paper, keeping the original order, so settlement A is the same settlement A;
- the free-text field `found_via_text` is removed. It was already coded into `found_via`, which is the variable the scripts use, so no result depends on it.

Nothing else was altered. All 200 records are present, every other column is unchanged, and all figures reported in the paper reproduce from this file.

## Running the analysis

Scripts run in order, 00 to 05.

| Script | Does | Reads | Writes |
| --- | --- | --- | --- |
| `00_make_shareable_data.R` | Produces the de-identified data file (not shared) | `inputs/waste_tidy.RData` |  |
| `01_make_waste_tidy.R` | Turns the raw survey export into three tidy tables ((not shared)) | raw survey export, `inputs/codebook.xlsx` | `inputs/waste_tidy.RData` |
| `02_build_analysis_frame.R` | Builds the business-level table the models use | `inputs/waste_tidy.RData`, `inputs/codebook.xlsx` | `inputs/analysis_df.rds` |
| `03_waste_analysis.R` | The regression analysis: bivariate screen, material streams, logistic models of transfer and willingness, multilevel model, intention-behaviour gap, receiving side | `inputs/analysis_df.rds` | model tables and the figure used in the paper |
| `04_descriptive_stats.R` | Descriptive-statistics tables for the appendix | `inputs/analysis_df.rds` | `outputs/tab_descriptives_*.tex` |
| `05_discovery_tables.R` | How existing arrangements were found, and whether the receiving business is in the same town | `inputs/analysis_df.rds` | `outputs/tab_discovery.tex` |

### Where to start

**Start at `02_build_analysis_frame.R`.**

Scripts 00 and 01 both require the raw, non-public survey data and cannot be run from this repository. Skip them.

To reproduce the analysis, copy `shareable/waste_tidy.RData` into an `inputs/` folder in the project root, then run scripts 02 to 05 in order. The file name is unchanged, so no script needs editing.

```
inputs/
  waste_tidy.RData     <- copied from shareable/
  codebook.xlsx
```

Script 02 also needs `inputs/codebook.xlsx`, which holds the sector-group, owner-role and waste-volume-band lookup tables. It contains no personal data.

### Packages

`dplyr`, `tidyr`, `stringr`, `forcats`, `readxl`, `here`, `broom`, `sandwich`, `modelsummary`, and the modelling packages called in `03_waste_analysis.R`.

Paths are handled with `here`, so the scripts run from the project root without editing. If `here()` reports the wrong folder, run
`here::set_here("<full path to the project folder>")` once and restart R.
