# =====================================================================
# prepare_data.R
# Turns the supplied source extract into ONE processed file:
#   one row per respondent per wave (long format), with
#   - the original (raw) item values, unchanged, for traceability
#   - recoded items (suffix _r): special codes -> NA, reversed where needed
#   - construct scores
# Rules come from spec/item_spec.csv, not from this script.
#
# Run from the project folder:  source("R/prepare_data.R")
# =====================================================================

# ---- 0. Settings (the only lines a reviewer should need to change) ----
src_path <- "C:/PPBS/source/Study_E_500.csv"  # location of the source extract
min_prop <- 2/3   # a score needs at least 2/3 of its items answered

# ---- 1. Read the item specification and the source file -------------
spec <- read.csv("spec/item_spec.csv", stringsAsFactors = FALSE)

# All columns read as text, so nothing is converted silently.
# Blank cells and literal "NA" both become R NA (as in the item guide).
src <- read.csv(src_path, colClasses = "character", na.strings = c("", "NA"),
                check.names = FALSE, encoding = "UTF-8")

# Key check: stop immediately if respondent IDs are not unique
stopifnot(!anyDuplicated(src$exercise_id))

# ---- 2. Helper: recode one item ---------------------------------------
# - text -> number
# - anything outside the valid range (97, 99, ...) -> NA
# - reverse if the spec says so: new = (min + max) - old  (1<->7, 2<->6 ...)
clean_item <- function(x, lo, hi, rev) {
  x <- suppressWarnings(as.numeric(x))
  x[!(x %in% lo:hi)] <- NA
  if (isTRUE(rev == 1)) x <- (lo + hi) - x
  x
}

# column name without the wave, e.g. "rwa_3" or "deprivation"
spec$base <- ifelse(is.na(spec$item_no), spec$item_stem,
                    paste0(spec$item_stem, "_", spec$item_no))
scale_rows <- spec[spec$construct != "vote_intention", ]

# ---- 3. Build the long file: one block of rows per wave --------------
long <- do.call(rbind, lapply(1:5, function(w) {
  out <- data.frame(exercise_id = src$exercise_id, wave = w)

  # construct items: raw value + recoded value
  for (i in seq_len(nrow(scale_rows))) {
    r     <- scale_rows[i, ]
    col   <- paste0(r$base, "_w", w)
    asked <- w %in% as.integer(strsplit(r$waves, ";")[[1]]) && col %in% names(src)
    out[[r$base]]                <- if (asked) src[[col]] else NA_character_
    out[[paste0(r$base, "_r")]]  <- if (asked) clean_item(src[[col]], r$scale_min,
                                                          r$scale_max, r$reverse)
                                    else NA_real_
  }

  # vote intention: kept as text, recoded later in the vote report
  out$vote <- src[[paste0("vote_w", w)]]

  # attention flags: 1 if ANY attention flag is set in this wave
  flags <- intersect(paste0("att", 1:3, "_flag_w", w), names(src))
  out$att_flag_any <- as.integer(rowSums(!is.na(src[flags]) & src[flags] == "1") > 0)

  # covariates (raw; cleaned in the reports where used)
  get <- function(nm) if (nm %in% names(src)) src[[nm]] else NA_character_
  out$gender         <- get("gender")
  out$education      <- get("education")
  out$age            <- suppressWarnings(as.numeric(get(paste0("resp_age_w", w))))
  out$income         <- get(paste0("income_w", w))
  out$ideology       <- get(paste0("ideology_w", w))
  out$ideology_soc   <- get(paste0("ideology_social_w", w))
  out$ideology_econ  <- get(paste0("ideology_economic_w", w))
  out$east_west      <- get(paste0("ostwest_w", w))
  out
}))

# ---- 4. Scores: mean of available items, if enough items answered ----
items_for <- function(con, sub = NULL) {
  s <- scale_rows[scale_rows$construct == con &
                  (is.null(sub) | scale_rows$subscale %in% sub), ]
  paste0(s$base, "_r")
}
score <- function(items) {
  m    <- as.matrix(long[items])
  s    <- rowMeans(m, na.rm = TRUE)
  s[rowSums(!is.na(m)) < ceiling(min_prop * length(items))] <- NA
  s
}

long$rwa                <- score(items_for("rwa"))
long$rwa_aggression     <- score(items_for("rwa", "aggression"))
long$rwa_submission     <- score(items_for("rwa", "submission"))
long$rwa_conventionalism<- score(items_for("rwa", "conventionalism"))

long$anomie             <- score(items_for("anomie"))
long$anomie_fabric      <- score(items_for("anomie", "social_fabric"))
long$anomie_leadership  <- score(items_for("anomie", "leadership"))

long$rel_deprivation    <- long$deprivation_r   # single item, higher = more deprived

long$rehabilitation     <- score(items_for("rehabilitation"))   # waves 3-5 only

# Structure from wave-1 EFA; confirmed across waves in the attribution report
long$attr_societal   <- score(items_for("attribution", "societal"))
long$attr_individual <- score(items_for("attribution", "individual"))

# ---- 5. Save -----------------------------------------------------------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(long, "data/processed/study_e_processed.rds")

# ---- 6. Quick check: analytic N per score per wave (aggregate only) ---
score_cols <- c("rwa", "anomie", "rel_deprivation", "rehabilitation",
                "attr_societal", "attr_individual")
check <- aggregate(long[score_cols], by = list(wave = long$wave),
                   FUN = function(x) sum(!is.na(x)))
dir.create("checks", showWarnings = FALSE)
write.csv(check, "checks/analytic_n_by_wave.csv", row.names = FALSE)

cat("Processed file saved:", nrow(long), "rows x", ncol(long), "columns\n")
cat("Rows per wave:", paste(table(long$wave), collapse = ", "), "\n\n")
print(check)
