# Handover and deletion plan


## Where everything is

| Item | Location | Who can see it |
|---|---|---|
| Source extract `Study_E_500.csv` | My computer only, `C:\PPBS\source\` (unsynced folder) | Me |
| Code, item rules, processed file, decision log | Private GitHub repository `bagociutemonika/ppbs_study_e` | Me and @flavioazevedo |
| Public reports (aggregate only) | https://ppbsemb.netlify.app | Public |
| AI conversation | claude.ai (aggregate outputs only, no respondent records) | Me |

## How to rerun on the approved full dataset

1. Clone the repository; run `renv::restore()` to install the same package versions.
2. Put the full source file outside the repository and set `source_file` in `R/prepare_data.R`.
3. Run `source("run.R")`. Nothing assumes 500 respondents.
4. Before reuse, review: `spec/item_spec.csv` (special codes, reversals, waves).
5. Publish only `site/_site`, and only after `check_public_site.R` has passed.

## Deletion steps (after PPBS confirms the handover)

1. Delete `C:\PPBS\source\` (including `Study_E_500.csv`) and empty the Recycle Bin.
2. Delete my local clone `C:\PPBS\ppbs_study_e\` (it contains the processed file).
3. Delete the email attachment with the source extract, and check that no copy is left in
   Downloads or in OneDrive (including the OneDrive recycle bin).
4. Delete the private GitHub repository (Settings > Delete this repository), once PPBS
   confirms they have their own copy, and revoke my GitHub access token.
5. Ask PPBS whether to keep or delete the public Netlify site; delete it if asked.
6. Delete the AI conversation from claude.ai.

