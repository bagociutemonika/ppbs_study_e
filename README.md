# Study E · reproducible construct reports (PPBS practical exercise)

Six reports on the Study E panel extract (500 respondents, 5 waves):
right-wing authoritarianism, anomie, relative deprivation, support for rehabilitation,
causal attribution of crime, and vote intention.

Public site (aggregate results only): https://ppbsemb.netlify.app

## Rebuild everything

1. Put the source extract somewhere **outside** this folder and write its location in
   `source_file` at the top of `R/prepare_data.R`.
2. Install the package versions used here: `renv::restore()`
3. Run: `source("run.R")` in RStudio (or `Rscript run.R` in a terminal).

Nothing in the code assumes 500 respondents; it runs on the full dataset as long as the
column names are the same.

## How the project is organised

```
run.R                      the one rebuild command: runs the three steps below
spec/item_spec.csv         the item rules: wording, waves, range, reversal, special codes
R/prepare_data.R           step 1: source file -> data/processed/study_e_processed.rds
R/report_setup.R           loaded by every report: packages, data, small helper functions
site/_construct_template.qmd  the report layout shared by Reports 1-5
site/rwa.qmd ... vote.qmd  one page per report (which items to use + report-specific checks)
site/index.qmd             start page (workflow proposal, status, key decisions)
R/check_public_site.R      step 3: scans the built website for data files and respondent IDs
data/processed/            the processed respondent file (private repository only)
checks/                    aggregate check output (counts, source-file fingerprint)
docs/                      decision log, AI disclosure, handover and deletion plan
```

The flow: **source CSV + item rules → prepare_data.R → processed file → Quarto reports →
site/_site → safety check → Netlify**.

## Data protection

- The source extract is never in this repository (`.gitignore` blocks all CSV files
  except our own aggregate check files).
- The processed file is only in this **private** repository.
- Only `site/_site` is published, after `check_public_site.R` has passed. Groups smaller
  than 10 respondents are hidden in all public tables.
- No respondent records were given to any AI service (see `docs/ai_disclosure.md`).
