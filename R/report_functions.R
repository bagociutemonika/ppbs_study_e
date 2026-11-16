# =====================================================================
# report_functions.R
# Shared functions used by ALL construct reports.
# One source: fix a function here and every report changes on rebuild.
# Every function returns AGGREGATE results only (no respondent rows).
# =====================================================================

suppressPackageStartupMessages({
  library(ggplot2)
})

MIN_CELL <- 10   # groups smaller than this are suppressed in public tables
options(knitr.kable.NA = "")   # suppressed / hidden values show as empty cells

# ---- loading ----------------------------------------------------------
# Reports live in site/, so paths go one folder up.
load_processed <- function(path = "../data/processed/study_e_processed.rds") readRDS(path)
load_spec      <- function(path = "../spec/item_spec.csv") read.csv(path, stringsAsFactors = FALSE)

# ---- small helpers ------------------------------------------------------
r2 <- function(x, d = 2) round(x, d)

kable_out <- function(x, ...) knitr::kable(x, row.names = FALSE, ...)

# Text label -> number, for 1-7 items stored as labels (e.g. ideology).
# Takes the first number found in the label; anything outside lo:hi -> NA.
num_from_label <- function(x, lo = 1, hi = 7) {
  v <- suppressWarnings(as.numeric(sub("^\\D*(\\d+).*$", "\\1", x)))
  v[!(v %in% lo:hi)] <- NA
  v
}

# Household income: text labels -> ordered 1-13 (order from the item guide)
INCOME_LEVELS <- c("< 500€", "500€ bis unter 750€", "750€ bis unter 1000€",
                   "1000€ bis unter 1250€", "1250€ bis unter 1500€", "1500€ bis unter 2000€",
                   "2000€ bis unter 2500€", "2500€ bis unter 3000€", "3000€ bis unter 4000€",
                   "4000€ bis unter 5000€", "5000€ bis unter 7500€", "7500€ bis unter 10000€",
                   "10000€ und mehr")
income_num <- function(x) match(trimws(x), INCOME_LEVELS)

# Merge categories smaller than MIN_CELL into one group (for tables and models)
lump_small <- function(x, label = "Other (small groups)") {
  x <- as.character(x)
  t <- table(x)
  x[x %in% names(t)[t < MIN_CELL]] <- label
  factor(x)
}

# Set categories smaller than MIN_CELL to missing (used in models, so that no
# coefficient describes a group of fewer than MIN_CELL people)
drop_small <- function(x) {
  x <- as.character(x)
  t <- table(x)
  x[x %in% names(t)[t < MIN_CELL]] <- NA
  factor(x)
}

age_group <- function(age) cut(age, c(-Inf, 29, 44, 59, Inf),
                               labels = c("18-29", "30-44", "45-59", "60+"))

waves_present <- function(d, score) sort(unique(d$wave[!is.na(d[[score]])]))

# ---- 1. measurement ---------------------------------------------------
item_table <- function(spec, construct) {
  s <- spec[spec$construct == construct, ]
  data.frame(
    Item       = ifelse(is.na(s$item_no), s$item_stem, paste0(s$item_stem, "_", s$item_no)),
    Wording    = s$wording,
    Waves      = gsub(";", ", ", s$waves),
    Range      = paste0(s$scale_min, "-", s$scale_max),
    `Reversed in pipeline` = ifelse(s$reverse == 1, "yes", "no"),
    `Special codes`        = s$documented_missing_codes,
    Subscale   = s$subscale,
    check.names = FALSE)
}

# ---- 2. data quality --------------------------------------------------
# analytic N per score per wave
n_table <- function(d, score_names) {
  w <- sort(unique(d$wave))
  out <- data.frame(Wave = w)
  for (s in score_names) out[[s]] <- sapply(w, function(x) sum(!is.na(d[[s]][d$wave == x])))
  out
}

# per item and wave: how many raw answers were blank, and how many were
# special/out-of-range codes recoded to missing
item_quality <- function(d, items_r, lo, hi) {
  rows <- list()
  for (i in items_r) {
    raw <- sub("_r$", "", i)
    for (w in sort(unique(d$wave))) {
      v <- d[[raw]][d$wave == w]
      if (all(is.na(v))) next                      # not asked in this wave
      rows[[length(rows) + 1]] <- data.frame(
        Item = raw, Wave = w,
        Blank = sum(is.na(v)),
        `Special code -> missing` = sum(!is.na(v) & !(v %in% as.character(lo:hi))),
        Valid = sum(d[[i]][d$wave == w] %in% lo:hi),
        check.names = FALSE)
    }
  }
  do.call(rbind, rows)
}

# ---- 3. distributions ---------------------------------------------------
dist_plot <- function(d, score, label, lo = 1, hi = 7) {
  dd <- d[!is.na(d[[score]]), c("wave", score)]
  names(dd) <- c("wave", "value")
  dd$wave <- factor(paste("Wave", dd$wave))
  ggplot(dd, aes(value)) +
    geom_histogram(binwidth = 0.5, boundary = lo, fill = "#4C78A8", colour = "white") +
    facet_wrap(~ wave, nrow = 1) +
    scale_x_continuous(limits = c(lo - 0.25, hi + 0.25), breaks = lo:hi) +
    labs(x = label, y = "Respondents") +
    theme_minimal(base_size = 11)
}

describe_by_wave <- function(d, score) {
  do.call(rbind, lapply(waves_present(d, score), function(w) {
    v <- d[[score]][d$wave == w]; v <- v[!is.na(v)]
    data.frame(Wave = w, n = length(v), Mean = r2(mean(v)), SD = r2(sd(v)),
               Median = r2(median(v)),
               Skew = r2(psych::skew(v)), Kurtosis = r2(psych::kurtosi(v)))
  }))
}

# ---- 4. psychometrics ---------------------------------------------------
reliability_table <- function(d, scores) {
  rows <- list()
  for (s in names(scores)) {
    items <- scores[[s]]
    if (length(items) < 2) next
    for (w in waves_present(d, s)) {
      x <- d[d$wave == w, items]
      a <- suppressWarnings(psych::alpha(x, check.keys = FALSE, warnings = FALSE))
      om <- tryCatch(suppressWarnings(suppressMessages(
              psych::omega(x, nfactors = 1, plot = FALSE)$omega.tot)),
              error = function(e) NA)
      rows[[length(rows) + 1]] <- data.frame(
        Score = s, Wave = w, Items = length(items),
        alpha = r2(a$total$raw_alpha), omega = r2(om),
        `Mean inter-item r` = r2(a$total$average_r),
        `Lowest item-rest r` = r2(min(a$item.stats$r.drop)),
        check.names = FALSE)
    }
  }
  do.call(rbind, rows)
}

# Exploratory factor analysis loadings (one wave), rounded, small loadings blanked
efa_loadings <- function(d, items, nfactors, wave) {
  x <- d[d$wave == wave, items]
  f <- suppressWarnings(suppressMessages(
         psych::fa(x, nfactors = nfactors, rotate = "oblimin", fm = "minres")))
  L <- unclass(f$loadings)
  out <- data.frame(Item = sub("_r$", "", rownames(L)), round(L, 2), check.names = FALSE)
  out[-1][abs(out[-1]) < .30] <- NA
  list(table = out,
       fit = data.frame(Wave = wave, Factors = nfactors,
                        RMSR = r2(f$rms), RMSEA = r2(unname(f$RMSEA[1]), 3),
                        TLI = r2(f$TLI),
                        `Factor r` = if (nfactors > 1) r2(f$Phi[1, 2]) else NA,
                        check.names = FALSE))
}

# ---- 5. groups and correlates -----------------------------------------
group_means <- function(d, score, var, wave) {
  dd <- d[d$wave == wave & !is.na(d[[score]]) & !is.na(d[[var]]), ]
  g  <- split(dd[[score]], as.character(dd[[var]]))
  n  <- sapply(g, length)
  out <- data.frame(Group = names(g), n = n,
                    Mean = r2(sapply(g, mean)), SD = r2(sapply(g, sd)))
  small <- out$n < MIN_CELL
  out$Mean[small] <- NA; out$SD[small] <- NA
  out$n <- ifelse(small, paste0("<", MIN_CELL), as.character(out$n))
  rownames(out) <- NULL
  out
}

cor_row <- function(x, y, name) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 20) return(NULL)
  ct <- cor.test(x[ok], y[ok])
  data.frame(Variable = name, n = sum(ok), r = r2(ct$estimate),
             `95% CI` = paste0("[", r2(ct$conf.int[1]), ", ", r2(ct$conf.int[2]), "]"),
             check.names = FALSE)
}

correlates_table <- function(d, score, others, wave) {
  dd <- d[d$wave == wave, ]
  rows <- lapply(names(others), function(nm) cor_row(dd[[score]], others[[nm]](dd), nm))
  do.call(rbind, rows)
}

# ---- 6. adjusted model --------------------------------------------------
adjusted_model <- function(d, score, wave) {
  dd <- d[d$wave == wave, ]
  dd$y         <- dd[[score]]
  dd$age10     <- dd$age / 10
  dd$gender_f  <- drop_small(dd$gender)
  dd$edu_f     <- drop_small(dd$education)
  dd$ideology_n<- num_from_label(dd$ideology)
  dd$income_n  <- income_num(dd$income)
  # use a predictor only if it has at least two distinct observed values
  preds <- c("age10", "gender_f", "edu_f", "income_n", "ideology_n")
  preds <- preds[sapply(preds, function(p) length(unique(na.omit(dd[[p]]))) >= 2)]
  f <- reformulate(preds, response = "y")
  m <- lm(f, data = dd)
  ci <- confint(m)
  co <- summary(m)$coefficients
  tab <- data.frame(Term = rownames(co), b = r2(co[, 1]),
                    `95% CI` = paste0("[", r2(ci[, 1]), ", ", r2(ci[, 2]), "]"),
                    p = formatC(co[, 4], format = "f", digits = 3),
                    check.names = FALSE)
  list(table = tab, n = nobs(m), r2 = r2(summary(m)$r.squared),
       adj_r2 = r2(summary(m)$adj.r.squared))
}

# ---- 7. longitudinal ----------------------------------------------------
# respondents x waves matrix for one score (rows aligned by exercise_id)
wide_scores <- function(d, score) {
  w <- waves_present(d, score)
  ids <- sort(unique(d$exercise_id))
  x <- sapply(w, function(k) {
    dk <- d[d$wave == k, ]
    dk[[score]][match(ids, dk$exercise_id)]
  })
  colnames(x) <- paste0("W", w)
  x
}

mean_ci_by_wave <- function(d, score, keep = NULL) {
  x <- wide_scores(d, score)
  if (!is.null(keep)) x <- x[keep, , drop = FALSE]
  do.call(rbind, lapply(colnames(x), function(k) {
    v <- x[, k]; v <- v[!is.na(v)]; n <- length(v)
    m <- mean(v); se <- sd(v) / sqrt(n); q <- qt(.975, n - 1)
    data.frame(Wave = k, n = n, Mean = m, lo = m - q * se, hi = m + q * se)
  }))
}

mean_plot <- function(tab_list, label) {
  dd <- do.call(rbind, lapply(names(tab_list), function(nm) cbind(tab_list[[nm]], Sample = nm)))
  ggplot(dd, aes(Wave, Mean, colour = Sample, group = Sample)) +
    geom_line(position = position_dodge(.3)) +
    geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(.3)) +
    labs(y = paste(label, "(mean, 95% CI)"), x = NULL, colour = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
}

# paired change: adjacent waves + first -> last
change_table <- function(d, score) {
  x <- wide_scores(d, score); k <- ncol(x)
  if (k < 2) return(NULL)
  pairs <- lapply(seq_len(k - 1), function(i) c(i, i + 1))
  if (k > 2) pairs <- c(pairs, list(c(1, k)))
  do.call(rbind, lapply(pairs, function(p) {
    a <- x[, p[1]]; b <- x[, p[2]]; ok <- !is.na(a) & !is.na(b)
    diff <- b[ok] - a[ok]; tt <- t.test(diff)
    data.frame(Comparison = paste(colnames(x)[p], collapse = " to "), n = sum(ok),
               `Mean change` = r2(mean(diff)),
               `95% CI` = paste0("[", r2(tt$conf.int[1]), ", ", r2(tt$conf.int[2]), "]"),
               d_z = r2(mean(diff) / sd(diff)),
               check.names = FALSE)
  }))
}

stability_matrix <- function(d, score) {
  m <- r2(cor(wide_scores(d, score), use = "pairwise.complete.obs"))
  m[upper.tri(m)] <- NA
  data.frame(Wave = rownames(m), m, check.names = FALSE)
}

balanced_vs_available <- function(d, score) {
  x <- wide_scores(d, score); bal <- complete.cases(x)
  data.frame(Wave = colnames(x),
             `Available n` = colSums(!is.na(x)),
             `Available mean` = r2(colMeans(x, na.rm = TRUE)),
             `Balanced n` = sum(bal),
             `Balanced mean` = r2(colMeans(x[bal, , drop = FALSE])),
             check.names = FALSE)
}

# Random-intercept model: how much of the variation is stable between persons?
# (intraclass correlation) and the average change per wave.
icc_model <- function(d, score) {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)
  dd <- d[!is.na(d[[score]]), c("exercise_id", "wave", score)]
  names(dd)[3] <- "y"
  dd$wave_c <- dd$wave - min(dd$wave)
  m <- lme4::lmer(y ~ wave_c + (1 | exercise_id), data = dd, REML = TRUE)
  vc <- as.data.frame(lme4::VarCorr(m))
  icc <- vc$vcov[1] / sum(vc$vcov)
  ci <- suppressMessages(confint(m, parm = "wave_c", method = "Wald"))
  data.frame(`Persons` = length(unique(dd$exercise_id)), Observations = nrow(dd),
             ICC = r2(icc),
             `Change per wave` = r2(lme4::fixef(m)["wave_c"], 3),
             `95% CI` = paste0("[", r2(ci[1], 3), ", ", r2(ci[2], 3), "]"),
             check.names = FALSE)
}
