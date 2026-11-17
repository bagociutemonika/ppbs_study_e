# prepare_data.R
# Step 1 of the pipeline: from the source extract to one processed file.
#
#  - reads the source CSV (the file itself is never changed)
#  - recodes the items using the rules in spec/item_spec.csv
#  - cleans the background variables (income, ideology, vote, ...)
#  - computes the construct scores
#  - saves data/processed/study_e_processed.rds: one row per respondent per wave
#
# Run it from the project folder: source("R/prepare_data.R")


# ---- Settings --------------------------------------------------------------

source_file <- "C:/PPBS/source/Study_E_500.csv"   # change this line on another computer
min_share   <- 2/3                                 # a score needs at least 2/3 of its items


# ---- 1. Read the source file and the item rules -----------------------------

# Every column is read as text, so R does not change any value on its own.
src <- read.csv(source_file, colClasses = "character", na.strings = c("", "NA"),
                check.names = FALSE, encoding = "UTF-8")
spec <- read.csv("spec/item_spec.csv", stringsAsFactors = FALSE)

stopifnot(!anyDuplicated(src$exercise_id))   # one row per respondent
cat("Source file:", nrow(src), "respondents,", ncol(src), "columns\n")

# Fingerprint of the source file, so we can show later that it was not changed
dir.create("checks", showWarnings = FALSE)
writeLines(c(paste("file:", basename(source_file)),
             paste("md5: ", unname(tools::md5sum(source_file))),
             paste("date:", Sys.Date())),
           "checks/source_fingerprint.txt")

# Item name without the wave ending, e.g. "rwa_3" or "deprivation"
spec$item <- ifelse(is.na(spec$item_no), spec$item_stem,
                    paste0(spec$item_stem, "_", spec$item_no))
items <- spec[spec$construct != "vote_intention", ]


# ---- 2. A small helper to recode one item -----------------------------------

recode_item <- function(x, min, max, reverse) {
  x <- suppressWarnings(as.numeric(x))
  x[!(x %in% min:max)] <- NA          # special codes such as 97 or 99 become missing
  if (reverse == 1) x <- (min + max) - x
  x
}

column <- function(name) {            # a source column, or NA if it does not exist
  if (name %in% names(src)) src[[name]] else NA
}


# ---- 3. Long format: one block of 500 rows per wave -------------------------

blocks <- list()

for (w in 1:5) {
  dw <- data.frame(exercise_id = src$exercise_id, wave = w)

  # construct items: the original value (text) and the recoded value (_r)
  for (i in seq_len(nrow(items))) {
    it    <- items[i, ]
    col   <- paste0(it$item, "_w", w)
    asked <- col %in% names(src) && w %in% as.integer(strsplit(it$waves, ";")[[1]])

    dw[[it$item]] <- if (asked) src[[col]] else NA
    dw[[paste0(it$item, "_r")]] <- if (asked) {
      recode_item(src[[col]], it$scale_min, it$scale_max, it$reverse)
    } else NA
  }

  # attention checks: 1 if any attention flag is set in this wave
  flags <- intersect(paste0("att", 1:3, "_flag_w", w), names(src))
  dw$att_flag <- 0
  for (f in flags) dw$att_flag[src[[f]] %in% "1"] <- 1

  # background variables (still raw here, cleaned in step 4)
  dw$gender        <- column("gender")
  dw$education     <- column("education")
  dw$age           <- suppressWarnings(as.numeric(column(paste0("resp_age_w", w))))
  dw$income_raw    <- column(paste0("income_w", w))
  dw$ideology_raw  <- column(paste0("ideology_w", w))
  dw$ideology_soc_raw  <- column(paste0("ideology_social_w", w))
  dw$ideology_econ_raw <- column(paste0("ideology_economic_w", w))
  dw$east_west     <- column(paste0("ostwest_w", w))
  dw$vote_raw      <- column(paste0("vote_w", w))

  blocks[[w]] <- dw
}

d <- do.call(rbind, blocks)


# ---- 4. Clean the background variables ---------------------------------------

# Age groups
d$age_group <- cut(d$age, c(-Inf, 29, 44, 59, Inf),
                   labels = c("18-29", "30-44", "45-59", "60+"))

# Income is stored as text; the order comes from the item guide (1-13).
income_levels <- c("< 500\u20ac", "500\u20ac bis unter 750\u20ac", "750\u20ac bis unter 1000\u20ac",
                   "1000\u20ac bis unter 1250\u20ac", "1250\u20ac bis unter 1500\u20ac",
                   "1500\u20ac bis unter 2000\u20ac", "2000\u20ac bis unter 2500\u20ac",
                   "2500\u20ac bis unter 3000\u20ac", "3000\u20ac bis unter 4000\u20ac",
                   "4000\u20ac bis unter 5000\u20ac", "5000\u20ac bis unter 7500\u20ac",
                   "7500\u20ac bis unter 10000\u20ac", "10000\u20ac und mehr")
d$income <- match(trimws(d$income_raw), income_levels)
d$income_group <- cut(d$income, c(0, 5, 8, 10, 13),
                      labels = c("< 1500 EUR", "1500-2999 EUR", "3000-4999 EUR", "5000+ EUR"))

# Left-right self-placement: take the number at the start of the label.
# Codes of 90 and above (e.g. 99 "not specified") become missing.
first_number <- function(x) {
  v <- suppressWarnings(as.numeric(sub("^\\D*(\\d+).*$", "\\1", x)))
  v[v >= 90] <- NA
  v
}
d$ideology      <- first_number(d$ideology_raw)
d$ideology_soc  <- first_number(d$ideology_soc_raw)
d$ideology_econ <- first_number(d$ideology_econ_raw)

# Vote intention: German labels -> English categories
vote_labels <- c("CDU/CSU"                          = "CDU/CSU",
                 "SPD"                              = "SPD",
                 "FDP"                              = "FDP",
                 "B\u00fcndnis 90/Die Gr\u00fcnen"  = "Greens",
                 "Die Linke"                        = "Die Linke",
                 "AfD"                              = "AfD",
                 "Andere Partei"                    = "Other party",
                 "Ich w\u00fcrde nicht w\u00e4hlen" = "Would not vote",
                 "Ich wei\u00df es noch nicht"      = "Don't know yet")
d$vote <- unname(vote_labels[trimws(d$vote_raw)])
d$vote[is.na(d$vote) & !is.na(d$vote_raw)] <- "UNRECOGNISED"
d$vote[is.na(d$vote_raw)] <- "No answer"
if (any(d$vote == "UNRECOGNISED")) warning("Some vote labels were not recognised - check vote_labels.")

# Grouped vote categories (fewer, larger groups for switching tables and models)
vote_groups <- c("CDU/CSU" = "CDU/CSU", "SPD" = "SPD", "Greens" = "Greens", "AfD" = "AfD",
                 "FDP" = "Other parties", "Die Linke" = "Other parties", "Other party" = "Other parties",
                 "Would not vote" = "Undecided / non-voter", "Don't know yet" = "Undecided / non-voter")
d$vote_group <- factor(unname(vote_groups[d$vote]),
                       levels = c("CDU/CSU", "SPD", "Greens", "AfD",
                                  "Other parties", "Undecided / non-voter"))


# ---- 5. Construct scores -----------------------------------------------------

# Items of a construct (and optionally of one subscale), as listed in the spec
items_in <- function(construct, subscale = NULL) {
  keep <- items$construct == construct
  if (!is.null(subscale)) keep <- keep & items$subscale == subscale
  items$item[keep]
}

# Mean of the recoded items, if at least min_share of them were answered
score <- function(item_names) {
  m <- as.matrix(d[paste0(item_names, "_r")])
  s <- rowMeans(m, na.rm = TRUE)
  s[rowSums(!is.na(m)) < ceiling(min_share * ncol(m))] <- NA
  s
}

d$rwa                 <- score(items_in("rwa"))
d$rwa_aggression      <- score(items_in("rwa", "aggression"))
d$rwa_submission      <- score(items_in("rwa", "submission"))
d$rwa_conventionalism <- score(items_in("rwa", "conventionalism"))

d$anomie            <- score(items_in("anomie"))
d$anomie_fabric     <- score(items_in("anomie", "social_fabric"))
d$anomie_leadership <- score(items_in("anomie", "leadership"))

d$rel_deprivation <- d$deprivation_r        # single item; higher = more deprived

d$rehabilitation  <- score(items_in("rehabilitation"))   # asked in waves 3-5 only

d$attr_societal   <- score(items_in("attribution", "societal"))
d$attr_individual <- score(items_in("attribution", "individual"))


# ---- 6. Save and print a short check ------------------------------------------

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(d, "data/processed/study_e_processed.rds")

scores <- c("rwa", "anomie", "rel_deprivation", "rehabilitation",
            "attr_societal", "attr_individual")
n_valid <- sapply(scores, function(s) tapply(!is.na(d[[s]]), d$wave, sum))
n_valid <- data.frame(wave = 1:5, n_valid)
write.csv(n_valid, "checks/analytic_n_by_wave.csv", row.names = FALSE)

cat("Processed file saved:", nrow(d), "rows (respondents x waves)\n")
print(n_valid, row.names = FALSE)
