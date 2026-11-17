
# check_public_site.R
# Checks the built website (site/_site) before it is published.
# Stops with an error if it finds anything that looks like respondent data:
#   1. data files (csv, rds, xlsx, ...)
#   2. the ID column name "exercise_id"
#   3. respondent IDs (only IDs of 5+ characters can be searched reliably)
#   4. very long tables (aggregate tables are short; a respondent list is not)

site_files <- list.files("site/_site", recursive = TRUE, full.names = TRUE)
if (length(site_files) == 0) stop("No website found in site/_site - run run.R first.")

# 1. data files
data_files <- grep("\\.(csv|rds|rdata|rda|xlsx|xls|sav|dta)$", site_files,
                   ignore.case = TRUE, value = TRUE)
if (length(data_files) > 0) stop("Data files in the website: ", paste(data_files, collapse = ", "))

text_files <- grep("\\.(html|js|json)$", site_files, value = TRUE)
site_text  <- paste(unlist(lapply(text_files, readLines, warn = FALSE)), collapse = "\n")

# 2. the ID column name
if (grepl("exercise_id", site_text, fixed = TRUE)) stop("'exercise_id' appears in the website. Do not publish.")

# 3. respondent IDs
ids   <- unique(as.character(readRDS("data/processed/study_e_processed.rds")$exercise_id))
long  <- ids[nchar(ids) >= 5]
found <- long[vapply(long, function(id) grepl(id, site_text, fixed = TRUE), logical(1))]
if (length(found) > 0) stop(length(found), " respondent ID(s) found in the website. Do not publish.")

# 4. table length
tables   <- regmatches(site_text, gregexpr("(?s)<table.*?</table>", site_text, perl = TRUE))[[1]]
rows     <- lengths(regmatches(tables, gregexpr("<tr", tables)))
max_rows <- if (length(rows) > 0) max(rows) else 0
if (max_rows > 100) stop("A table in the website has ", max_rows, " rows - check that it is aggregate.")

cat("Public-site check passed:", length(site_files), "files; no data files;",
    "no 'exercise_id'; longest table", max_rows, "rows;",
    length(long), "of", length(ids), "IDs long enough to search, none found.\n")

