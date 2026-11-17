# AI disclosure

DRAFT: check and complete.

**Service and model:** Claude (Anthropic), model Claude Opus 5.5, used through claude.ai
on 16-17 November 2026.

**What it was used for**
- Planning the work and explaining methods as a supervisor (reading order, requirements checklist).
- Drafting all R and Quarto code: `R/prepare_data.R`, `R/report_setup.R`,
  `R/check_public_site.R`, `run.R`, the report pages and the shared template.
- Drafting `spec/item_spec.csv` from the supplied item guide.
- Drafting the text of the README, the decision log and this disclosure.
- Suggesting checks (item direction, factor structure, deprivation vs. income), which
  I ran on my own computer.

**What was shared with the AI service**
- The exercise briefs, the item guide, the starter and legacy report files.
- Aggregate output only: counts of observed values per item, correlation matrices,
  factor-analysis output, vote frequencies, analytic N tables.
- **No respondent records** and no row-level data were shared.

**How the output was checked**
- Claude tested the code on made-up data with the same structure before giving it to me.
- I ran every step on the real data on my own computer and checked the results:
  key uniqueness, observed values against the item guide, analytic N per wave,
  item directions, and the rendered reports.
- I checked `spec/item_spec.csv` row by row against the item guide.
- The data decisions in the decision log are mine, based on output I produced locally.
- Mistakes found along the way: Claude's expected counts were wrong twice (31 vs 30 spec
  rows; 125 vs 148 columns); the anomie reversal from the item guide had to be undone after
  the direction check.

**Time spent:** (fill in)
