# ADWeaveR

ADWeaveR is an R package for transparent and reproducible integration of multi-source longitudinal Alzheimer's disease cohort data.

The package is designed for researchers who work with raw, locally stored cohort files and need a clear workflow for:

- reviewing source files and candidate ID/date fields,
- choosing merge settings such as timeline file, time window, and overlap behavior,
- aligning multimodal measurements to participant visit timelines, and
- checking post-merge coverage and complete-case availability.

ADWeaveR is intended to complement general-purpose data tools such as `dplyr`, `tidyverse`, and `data.table`. It is also different from curated study-specific data packages such as `ADNIMERGE2` in the `alzverse` ecosystem. Rather than distributing pre-processed datasets, ADWeaveR provides a configurable workflow that researchers apply to their own local cohort files.

This repository currently lives at `Shijia1997/ADMerge`, while the package and manuscript-facing name used in documentation is ADWeaveR.

## Installation

Install from GitHub:

```r
library(devtools)
install_github("Shijia1997/ADMerge")
library(ADMerge)
```

## What ADWeaveR Does

ADWeaveR supports a workflow with four main steps:

1. Build a source table from local CSV or Excel files.
2. Review and customize detected identifiers, date variables, window sizes, and overlap settings.
3. Merge all selected files to a user-chosen reference timeline.
4. Review the merged output with summaries, plots, and complete-case checks.

The package has been used in workflows motivated by AD cohort settings such as BIOCARD, ADNI, and AIBL-style longitudinal data organization.

## What You Need Before Using It

- Local access to the cohort files you want to merge.
- At least one participant identifier column in each file.
- For longitudinal files, at least one usable date or visit-number variable.
- A reference file that provides the timeline used to anchor the merge.

Recommended preprocessing before building the source table:

- standardize date formats,
- harmonize participant identifiers across files,
- separate true longitudinal files from cross-sectional files, and
- apply quality-control or version restrictions before merging when needed.

## Minimal Workflow

### 1. Generate a source table

```r
src_table <- get_src_table("path_to_biomarker_files")
```

This step scans the input directory and summarizes each file, including:

- candidate ID fields,
- candidate date fields,
- selected merge fields,
- overlap settings, and
- matching windows.

Common arguments for `get_src_table()` include:

- `path`: directory containing input files.
- `FILE_pattern`: file types to scan. Default includes `.csv`, `.xls`, and `.xlsx`.
- `ID_pattern`: pattern used to detect candidate ID variables.
- `DATE_pattern`: pattern used to detect candidate date or visit variables.
- `ID_usr_list`: optional user-specified ID variables.
- `DATE_usr_list`: optional user-specified date variables.
- `IS_overlap_list`: whether file-specific windows are allowed to overlap.
- `WINDOW_list`: file-specific matching windows.
- `file`: optional path to save the source table as a CSV.

### 2. Review and edit the source table

After generating `src_table`, review it carefully before merging.

In particular, check:

- `ID_in_file`
- `DATE_in_file`
- `ID_for_merge`
- `DATE_for_merge`
- `longitudinal`
- `IS_overlap`
- `WINDOW`

You can either:

1. rerun `get_src_table()` with user-specified settings, or
2. save the table, edit it externally, and load it back for merging.

### 3. Merge to a reference timeline

```r
ad_data <- ad_merge(
  path = "path_to_biomarker_files",
  DATE_type = "Date",
  dict_src = src_table
)
```

Key arguments for `ad_merge()`:

- `path`: directory containing the input files.
- `DATE_type`: `"Date"` for calendar dates or `"Number"` for visit-style time variables.
- `dict_src`: source table stored in R.
- `dict_src_path`: path to a saved source-table file.
- `timeline_file`: the file used as the reference timeline.
- `timeline_path`: optional explicit path to the reference file.

The merged result contains:

- `analysis_data`: the merged long-format dataset.
- `dict_src`: the source-table configuration used for the merge.

### 4. Inspect the merged output

```r
summary(ad_data)
plot(ad_data, distn = "SCF_m1", group = "SEX")
```

Useful post-merge tools include:

- `summary()`: reports core information about the merged result.
- `plot()`: visualizes follow-up distributions and grouping patterns.
- `review_complete()`: checks complete visits for a chosen set of variables.
- `plot_files()`: creates interactive file-coverage plots before merging.

Example complete-case review:

```r
complete_visits <- review_complete(
  ad_data,
  check_cols = c("ID_merged", "Date_timeline", "SEX", "APOECODE")
)
```

## Practical Notes

- A non-overlapping window is often a good starting point for standard longitudinal analyses.
- Narrower windows are usually better for dense short-term follow-up.
- Wider windows may improve retention, but they also increase temporal displacement and may require sensitivity analysis.
- If multiple processing versions exist for the same modality, it is usually safer to restrict to one version unless version comparison is part of the analysis plan.
- Saving the merged output together with the source table makes the workflow easier to audit and reproduce.

## Repository Resources

This repository includes:

- package source code,
- function documentation,
- selected workflow examples and R Markdown files,
- unit tests, and
- GitHub Pages visualizations for public-facing coverage plots.

Public pages:

- ADNI coverage page: [adni_plot_files.html](https://shijia1997.github.io/ADMerge/adni_plot_files.html)
- BIOCARD coverage page: [biocard_plot_files.html](https://shijia1997.github.io/ADMerge/biocard_plot_files.html)

Raw cohort data are not distributed in this repository. ADNI data require the standard ADNI access process. BIOCARD data are restricted and may require separate approval through the study framework.
