# report_setup.R
# Loaded at the top of every report page.
# Loads the packages and the processed data, and defines a few small helpers
# that several reports use. Every helper returns aggregate results only.

library(ggplot2)

d    <- readRDS("../data/processed/study_e_processed.rds")
spec <- read.csv("../spec/item_spec.csv", stringsAsFactors = FALSE)

MIN_CELL <- 10                  # groups smaller than this are hidden on the public site
options(knitr.kable.NA = "")    # hidden values appear as empty cells

score_names <- c(rwa = "RWA", anomie = "Anomie", rel_deprivation = "Relative deprivation",
                 rehabilitation = "Support for rehabilitation",
                 attr_societal = "Societal attribution", attr_individual = "Individual attribution")


# Print a table with 2 decimals
show_table <- function(x, digits = 2) {
  if (is.null(x) || nrow(x) == 0) return(knitr::asis_output("*Not available in this wave.*"))
  knitr::kable(x, digits = digits, row.names = FALSE)
}

# n, mean and SD of a score per group; groups smaller than MIN_CELL are hidden
group_means <- function(data, score, group) {
  x <- data[!is.na(data[[score]]) & !is.na(data[[group]]), ]
  out <- NULL
  for (g in levels(factor(x[[group]]))) {
    v <- x[[score]][x[[group]] == g]
    small <- length(v) < MIN_CELL
    out <- rbind(out, data.frame(Group = g,
                                 n    = if (small) paste0("<", MIN_CELL) else as.character(length(v)),
                                 Mean = if (small) NA else mean(v),
                                 SD   = if (small) NA else sd(v)))
  }
  out
}

# Reliability of a set of items in one wave
reliability <- function(data, item_names) {
  x  <- data[paste0(item_names, "_r")]
  a  <- psych::alpha(x, warnings = FALSE)
  om <- tryCatch(suppressWarnings(suppressMessages(
          psych::omega(x, nfactors = 1, plot = FALSE)$omega.tot)), error = function(e) NA)
  data.frame(Items = ncol(x), alpha = a$total$raw_alpha, omega = om,
             `Mean inter-item r`  = a$total$average_r,
             `Lowest item-rest r` = min(a$item.stats$r.drop), check.names = FALSE)
}

# Exploratory factor analysis in one wave: fit indices and loadings
efa <- function(data, item_names, nfactors) {
  f <- suppressWarnings(suppressMessages(
         psych::fa(data[paste0(item_names, "_r")], nfactors = nfactors, rotate = "oblimin")))
  loadings <- round(unclass(f$loadings), 2)
  loadings[abs(loadings) < .30] <- NA            # hide small loadings
  list(fit = data.frame(Factors = nfactors, RMSR = f$rms, RMSEA = unname(f$RMSEA[1]),
                        TLI = f$TLI, `Factor r` = if (nfactors > 1) f$Phi[1, 2] else NA,
                        check.names = FALSE),
       loadings = data.frame(Item = item_names, loadings, check.names = FALSE))
}

# Pearson correlation with its 95% confidence interval
correlation <- function(x, y, name) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 20) return(NULL)
  test <- cor.test(x[ok], y[ok])
  data.frame(Variable = name, n = sum(ok), r = unname(test$estimate),
             `95% CI` = sprintf("[%.2f, %.2f]", test$conf.int[1], test$conf.int[2]),
             check.names = FALSE)
}

# Mean and 95% CI of a score per wave
wave_means <- function(data, score) {
  out <- NULL
  for (w in sort(unique(data$wave))) {
    v <- data[[score]][data$wave == w]
    v <- v[!is.na(v)]
    if (length(v) < 2) next
    ci <- t.test(v)$conf.int
    out <- rbind(out, data.frame(Wave = paste0("W", w), n = length(v),
                                 Mean = mean(v), lo = ci[1], hi = ci[2]))
  }
  out
}

# Respondents x waves table of one score (for change and stability)
score_by_wave <- function(data, score) {
  waves <- sort(unique(data$wave[!is.na(data[[score]])]))
  ids   <- sort(unique(data$exercise_id))
  out   <- data.frame(row.names = ids)
  for (w in waves) {
    dw <- data[data$wave == w, ]
    out[[paste0("W", w)]] <- dw[[score]][match(ids, dw$exercise_id)]
  }
  out
}
