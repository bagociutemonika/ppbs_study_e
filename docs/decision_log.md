# Decision log

DRAFT: check every entry and correct anything that is not right.

| # | Topic | Decision | Alternatives considered | Reason / evidence | Status |
|---|---|---|---|---|---|
| 1 | Source file | Read unchanged from outside the repository; MD5 fingerprint written to `checks/source_fingerprint.txt` on every run | Copy into the repository | The brief requires the source to stay unchanged and out of the repository | Done |
| 2 | Import | All columns read as text; special codes handled per item in code | Let R guess the column types | Avoids silent conversion of codes such as 97 or 99 | Done |
| 3 | Respondent key | `exercise_id`: 500 unique values, 0 duplicates; checked on every run | - | Key check | Done |
| 4 | Panel structure | All 500 respondents took part in all 5 waves (`Welle_w1-w5`) | - | So balanced vs. available-case differences come only from item non-response; results describe panel stayers, not the population | Done |
| 5 | Item rules | All rules in `spec/item_spec.csv`; the scripts read them from there | Rules written into the code | One place to check and correct; a change reaches every report on rebuild | Done |
| 6 | Score rule | Mean of recoded items if at least 2/3 of the items were answered | Complete cases only; any one item | Keeps respondents with one missing item without scoring on too little information | Done |
| 7 | Anomie items 4-5 | Not reversed in the pipeline | Reverse as the item guide says | After reversal they correlated negatively with items 1-3 and 6 (e.g. r = -.60 with item 6, wave 1); without reversal all correlations are positive. The extract seems to store them already reversed | **Confirm with PPBS** |
| 8 | Anomie straight-lining | 16 respondents gave the same answer to all six items in wave 1; kept | Exclude | No rule in the brief; kept in the main analysis | Open: robustness check |
| 9 | Relative deprivation | Single item reversed so that higher = more deprived | Keep original direction | Spearman rho with household income -.61 to -.66 in waves 1-5 confirms the direction | Done |
| 10 | Deprivation (offenders) | `deprivation_offender` not part of the score | Combine | Different construct (treatment of offenders) | Done |
| 11 | Rehabilitation special code | 97 (waves 4-5) and blank (wave 3) both treated as "not able to answer" = missing; frequency reported | Treat wave-3 blanks as ordinary missing | Similar counts (item 2: 55 blank in W3 vs 45 and 32 coded 97 in W4-W5) suggest the code was lost in wave 3 | Done; **confirm with PPBS** |
| 12 | Causal attribution | Two scores: societal (items 1, 4, 5, 6) and individual (7, 8, 9); items 2 and 3 left out | One score of all 9 items | 9-12 negative inter-item correlations per wave; wave-1 EFA: two factors, r = -.07; RMSR = .04, RMSEA = .066, TLI = .91; items 2-3 cross-load or fit neither factor clearly. Structure checked in waves 2-5 | Done; robustness with item 2 to add |
| 13 | Attention flags | `att1-3_flag` assumed to mean "failed attention check"; everyone kept; robustness line shows respondents never flagged | Exclude flagged respondents | Meaning not documented in the item guide | **Confirm meaning with PPBS** |
| 14 | Income | Text labels mapped to the guide's order 1-13; grouped into 4 bands for tables | Use labels as they are | Text sorts alphabetically, which would scramble the order; some categories have fewer than 10 people | Done |
| 15 | Left-right self-placement | Number at the start of the label; values of 90 or above set to missing | - | Stored as text labels | **Check the range against the item guide** |
| 16 | Vote intention coding | German labels mapped to English; grouped into 6 categories for switching and models | Keep all 9 categories | Avoid very small cells | Done |
| 17 | Vote wave 1 | Described only; switching uses waves 2-5 | Treat wave 1 like the others | "Would not vote" and "Don't know yet" were only offered from wave 2 (66 missing in W1 vs about 10 later) | Done |
| 18 | Vote model | Construct scores from wave t predict vote in wave t+1 (W4 to W5; replication W3 to W4); multinomial logistic regression; bootstrap CIs | Same-wave model | A later measure must not explain an earlier answer | Done |
| 19 | Small cells | Groups below 10 respondents hidden in public tables and left out of models | No suppression | Protect respondents on a public site | Done |
| 20 | Empty columns | `VOTE_*`, `QOFF_*`, `QSCM_*` are empty for all respondents; ignored | - | No information | Done |
| 21 | Public site | Only `site/_site` published (Netlify Drop), after `check_public_site.R`; live site checked after deployment | GitHub Pages | Private repository; only reviewed aggregate files leave the computer | Done |
| 22 | Local storage | On receipt the files were in an employer-synced OneDrive folder; moved to unsynced local storage on 16 Nov and removed from the OneDrive recycle bin | - | The brief requires the data to stay local | Done |

## Open questions for PPBS

1. Are anomie items 4-5 already reverse-scored in the extract? (entry 7)
2. Was "not able to answer" for rehabilitation stored as blank in wave 3? (entry 11)
3. What do the `att*_flag` variables mean? (entry 13)
