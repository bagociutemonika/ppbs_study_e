# =====================================================================
# vote_functions.R
# Functions for Report 6 (vote intention as a categorical outcome).
# All outputs are aggregate; cells below MIN_CELL are suppressed.
# Labels are written with \u escapes so the file works in any encoding.
# =====================================================================

# Text labels in the extract (German) -> English labels
VOTE_LABELS <- c(
  "CDU/CSU"                              = "CDU/CSU",
  "SPD"                                  = "SPD",
  "FDP"                                  = "FDP",
  "Bündnis 90/Die Grünen"      = "Greens",
  "Die Linke"                            = "Die Linke",
  "AfD"                                  = "AfD",
  "Andere Partei"                        = "Other party",
  "Ich würde nicht wählen"     = "Would not vote",
  "Ich weiß es noch nicht"          = "Don't know yet")

vote_detail <- function(v) {
  out <- unname(VOTE_LABELS[trimws(v)])
  out[is.na(v)] <- "No answer"
  out[!is.na(v) & is.na(out)] <- "UNRECOGNISED LABEL"
  factor(out, levels = c(unname(VOTE_LABELS), "No answer", "UNRECOGNISED LABEL"))
}

# Grouped categories used for switching tables and models
VOTE_GROUPS <- c("CDU/CSU", "SPD", "Greens", "AfD", "Other parties", "Undecided / non-voter")
vote_group <- function(v) {
  det <- as.character(vote_detail(v))
  g <- det
  g[det %in% c("FDP", "Die Linke", "Other party")] <- "Other parties"
  g[det %in% c("Would not vote", "Don't know yet")] <- "Undecided / non-voter"
  g[det %in% c("No answer", "UNRECOGNISED LABEL")] <- NA
  factor(g, levels = VOTE_GROUPS)
}

fmt_n_pct <- function(n, total) {
  ifelse(n < MIN_CELL, paste0("<", MIN_CELL),
         paste0(n, " (", formatC(100 * n / total, format = "f", digits = 1), "%)"))
}

# ---- frequencies by wave -------------------------------------------------
vote_freq_table <- function(d) {
  waves <- sort(unique(d$wave))
  det <- lapply(waves, function(w) vote_detail(d$vote[d$wave == w]))
  levs <- levels(det[[1]])
  levs <- levs[levs != "UNRECOGNISED LABEL" | any(sapply(det, function(x) any(x == "UNRECOGNISED LABEL")))]
  out <- data.frame(Category = levs)
  for (i in seq_along(waves)) {
    tab <- table(det[[i]])[levs]
    col <- fmt_n_pct(as.integer(tab), length(det[[i]]))
    # categories not offered in wave 1
    if (waves[i] == 1) col[levs %in% c("Would not vote", "Don't know yet") & tab == 0] <- "not offered"
    out[[paste0("W", waves[i])]] <- col
  }
  out
}

vote_share_plot <- function(d) {
  dd <- d[!is.na(vote_group(d$vote)), ]
  dd$group <- vote_group(dd$vote)
  tab <- as.data.frame(prop.table(table(wave = dd$wave, group = dd$group), 1))
  tab$wave <- paste0("W", tab$wave)
  ggplot(tab, aes(wave, Freq, colour = group, group = group)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(x = NULL, y = "Share of respondents with a valid answer", colour = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
}

# ---- transitions ---------------------------------------------------------
vote_pair <- function(d, w1, w2) {
  a <- d[d$wave == w1, c("exercise_id", "vote")]
  b <- d[d$wave == w2, c("exercise_id", "vote")]
  m <- merge(a, b, by = "exercise_id", suffixes = c("_from", "_to"))
  data.frame(from = vote_group(m$vote_from), to = vote_group(m$vote_to))
}

transition_table <- function(d, w1, w2) {
  p <- vote_pair(d, w1, w2)
  p <- p[!is.na(p$from) & !is.na(p$to), ]
  t <- table(p$from, p$to)
  out <- as.data.frame.matrix(t)
  out[] <- lapply(out, function(x) ifelse(x < MIN_CELL, paste0("<", MIN_CELL), as.character(x)))
  out <- data.frame(From = rownames(t), out, check.names = FALSE)
  names(out)[1] <- "From (rows) / to (columns)"
  out
}

switch_type <- function(from, to) {
  und <- "Undecided / non-voter"
  ifelse(from == und & to == und, "Stayed undecided / non-voter",
  ifelse(from == und,             "Undecided / non-voter to a party",
  ifelse(to == und,               "Party to undecided / non-voter",
  ifelse(from == to,              "Same party",
                                  "Switched to another party"))))
}

switching_summary <- function(d, pairs) {
  types <- c("Same party", "Switched to another party", "Party to undecided / non-voter",
             "Undecided / non-voter to a party", "Stayed undecided / non-voter")
  out <- data.frame(Pattern = types)
  for (p in pairs) {
    x <- vote_pair(d, p[1], p[2])
    x <- x[!is.na(x$from) & !is.na(x$to), ]
    s <- table(factor(switch_type(as.character(x$from), as.character(x$to)), levels = types))
    out[[paste0("W", p[1], " to W", p[2])]] <- fmt_n_pct(as.integer(s), nrow(x))
  }
  out
}

# ---- prospective multinomial model -----------------------------------------
# Vote (grouped) at wave w_out predicted by construct scores at EARLIER wave w_in.
# Predictors are standardised (z) so effects compare a 2-SD difference.
vote_model_data <- function(d, w_in, w_out, preds) {
  x <- d[d$wave == w_in, c("exercise_id", preds)]
  for (p in preds) x[[p]] <- as.numeric(scale(x[[p]]))
  y <- d[d$wave == w_out, c("exercise_id", "vote")]
  m <- merge(x, y, by = "exercise_id")
  m$vote_g <- vote_group(m$vote)
  m <- m[complete.cases(m[c(preds, "vote_g")]), ]
  m$vote_g <- droplevels(m$vote_g)
  m
}

fit_multinom <- function(m, preds) {
  f <- reformulate(preds, response = "vote_g")
  nnet::multinom(f, data = m, trace = FALSE, maxit = 500)
}

# change in average predicted probability when one predictor moves from -1 SD to +1 SD
prob_change <- function(fit, m, preds) {
  res <- lapply(preds, function(p) {
    lo <- m; lo[[p]] <- -1
    hi <- m; hi[[p]] <-  1
    colMeans(predict(fit, hi, type = "probs")) - colMeans(predict(fit, lo, type = "probs"))
  })
  out <- do.call(rbind, res); rownames(out) <- preds
  out
}

vote_effects <- function(d, w_in, w_out, preds, labels, B = 300, seed = 2026) {
  m   <- vote_model_data(d, w_in, w_out, preds)
  fit <- fit_multinom(m, preds)
  est <- prob_change(fit, m, preds)
  set.seed(seed)
  boot <- replicate(B, {
    mb <- m[sample(nrow(m), replace = TRUE), ]
    mb$vote_g <- factor(mb$vote_g, levels = levels(m$vote_g))
    fb <- tryCatch(fit_multinom(mb, preds), error = function(e) NULL)
    if (is.null(fb)) return(est * NA)
    pc <- prob_change(fb, mb, preds)
    pc[, colnames(est), drop = FALSE]
  }, simplify = "array")
  lo <- apply(boot, c(1, 2), quantile, .025, na.rm = TRUE)
  hi <- apply(boot, c(1, 2), quantile, .975, na.rm = TRUE)
  long <- do.call(rbind, lapply(rownames(est), function(p) data.frame(
    Predictor = labels[p], Category = colnames(est),
    change = est[p, ], lo = lo[p, ], hi = hi[p, ])))
  rownames(long) <- NULL
  list(n = nrow(m), counts = table(m$vote_g), effects = long)
}

effects_table <- function(eff) {
  tab <- eff$effects
  pp <- function(x) formatC(100 * x, format = "f", digits = 1)
  data.frame(Predictor = tab$Predictor, Category = tab$Category,
             `Change (pp)` = pp(tab$change),
             `95% CI` = paste0("[", pp(tab$lo), ", ", pp(tab$hi), "]"),
             check.names = FALSE)
}

effects_plot <- function(eff) {
  tab <- eff$effects
  ggplot(tab, aes(100 * change, Predictor)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_pointrange(aes(xmin = 100 * lo, xmax = 100 * hi), size = .3) +
    facet_wrap(~ Category, nrow = 2) +
    labs(x = "Change in predicted probability, -1 SD to +1 SD (percentage points)", y = NULL) +
    theme_minimal(base_size = 10)
}
