# =====================================================================
# check_public_site.R
# Scans the BUILT public site (site/_site) before it is deployed:
#  1. no data-like files (csv, rds, RData, xlsx, ...)
#  2. no respondent IDs anywhere in the HTML/JS/JSON files
# Stops with an error if anything is found.
# =====================================================================
site  <- "site/_site"
files <- list.files(site, recursive = TRUE, full.names = TRUE)
if (length(files) == 0) stop("No built site found in ", site)

bad <- grepl("\\.(csv|rds|rdata|rda|xlsx|xls|sav|dta|parquet|feather)$", files, ignore.case = TRUE)
if (any(bad)) stop("Data-like files in the public site: ", paste(files[bad], collapse = ", "))

d   <- readRDS("data/processed/study_e_processed.rds")
ids <- unique(as.character(d$exercise_id))
long_ids <- ids[nchar(ids) >= 5]   # very short IDs would match ordinary numbers
text_files <- files[grepl("\\.(html|js|json|txt|md)$", files, ignore.case = TRUE)]
txt <- paste(unlist(lapply(text_files, readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n")

hits <- long_ids[vapply(long_ids, function(i) grepl(i, txt, fixed = TRUE), logical(1))]
if (length(hits)) stop(length(hits), " respondent ID(s) found in the public site. Do not deploy.")

cat("Public-site check passed:", length(files), "files scanned; no data files;",
    length(long_ids), "of", length(ids), "IDs long enough to search, none found.\n")
if (length(long_ids) < length(ids))
  cat("Note:", length(ids) - length(long_ids), "IDs are too short to search reliably; check the site manually.\n")
