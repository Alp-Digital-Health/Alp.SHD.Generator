# =============================================================================
# Synthetic Health Data Generator  -  PUBLIC DEMO BUILD  (for shinyapps.io)
#
# This is a DEMO-ONLY variant of the full application:
#   * Runs ONLY on built-in synthetic sample data (file upload is removed).
#   * Shows a mandatory privacy warning on load + a persistent banner.
#   * Directs users to DOWNLOAD the full app and run it on their own machine.
#   * Caps the number of synthetic records to keep the shared free tier stable.
#
# The analysis engine (generators, fidelity, metrics, evaluation, sweep) is
# IDENTICAL to the full app - only data entry and the warnings differ.
#
# Deploy:  put this single file (named app.R) in a folder, then in R:
#          rsconnect::deployApp("path/to/folder")
# =============================================================================

# =============================================================================
# >>> EDIT THIS ONE LINE: the public download link for the local version. <<<
#     Point it at your GitHub Release (where the R-Portable bundle lives).
# =============================================================================
DOWNLOAD_URL <- "https://github.com/Alp-Digital-Health/Alp.SHD.Generator/releases/latest"

# ---- Packages ---------------------------------------------------------------
# On shinyapps.io the dependencies are detected from these library() calls and
# installed automatically - do NOT install packages at runtime here.
# (readxl was only needed for file upload, which the demo removes.)
pkgs <- c("shiny", "shinyWidgets", "synthpop", "bnlearn", "rpart",
          "ggplot2", "dplyr", "tidyr", "randomForest")
invisible(lapply(pkgs, library, character.only = TRUE))

DEMO_MAX_SYNTH <- 10000  # upper cap on synthetic records for the public demo

# ---- Built-in demo data -----------------------------------------------------
set.seed(2024)
n <- 1000
age     <- pmin(pmax(round(rnorm(n, 50, 15)), 18), 90)
sex     <- factor(sample(c("F", "M"), n, replace = TRUE, prob = c(0.52, 0.48)))
smoker  <- factor(ifelse(runif(n) < plogis(-1 + 0.01 * (age - 50)), "Yes", "No"))
bmi     <- pmax(round(rnorm(n, 27 + 0.04 * (age - 50) + ifelse(sex == "M", 1, 0), 4), 1), 15)
sbp     <- round(rnorm(n, 110 + 0.5 * age + 0.8 * (bmi - 25) + ifelse(smoker == "Yes", 6, 0), 10))
chol    <- round(rnorm(n, 180 + 0.4 * age + 1.2 * (bmi - 25), 25))
glucose <- round(rnorm(n, 85 + 0.2 * age + 0.9 * (bmi - 25) + ifelse(smoker == "Yes", 4, 0), 12))
diabetes <- factor(ifelse(runif(n) < plogis(-9 + 0.04 * glucose + 0.06 * bmi + 0.02 * age),
                          "Yes", "No"))
real_demo <- data.frame(age, sex, bmi, smoker, sbp, chol, glucose, diabetes)

# ---- Model list -------------------------------------------------------------
MODEL_CHOICES <- c("Bayesian network" = "bn", "CART" = "cart", "Random forest" = "rf",
                   "Parametric" = "parametric", "Gaussian copula" = "copula")
model_label <- function(key) names(MODEL_CHOICES)[match(key, MODEL_CHOICES)]

# ---- Fidelity ---------------------------------------------------------------
fidelity_frac <- list("1 - Low" = 0.20, "2 - Moderate" = 0.50,
                      "3 - High" = 0.80, "4 - Very high" = 1.00)
NOISE_MULT <- 2.5
apply_fidelity <- function(synth, real, f, cont_vars, cat_vars) {
  out <- synth
  for (v in cont_vars) {
    s <- sd(real[[v]], na.rm = TRUE)
    out[[v]] <- synth[[v]] + (1 - f) * NOISE_MULT * s * rnorm(nrow(synth))
  }
  for (v in cat_vars) {
    flip <- runif(nrow(synth)) > f
    if (any(flip)) {
      vals <- as.character(synth[[v]])
      vals[flip] <- as.character(sample(real[[v]], sum(flip), replace = TRUE))
      out[[v]] <- factor(vals, levels = levels(factor(real[[v]])))
    }
  }
  out
}

# ---- Data preparation -------------------------------------------------------
prepare_data <- function(df) {
  df <- as.data.frame(df); msgs <- character(0)
  for (v in names(df)) {
    if (inherits(df[[v]], c("Date", "POSIXct", "POSIXt"))) df[[v]] <- as.numeric(df[[v]])
    if (is.character(df[[v]]) || is.logical(df[[v]]))       df[[v]] <- factor(df[[v]])
  }
  keep <- vapply(df, function(c) length(unique(c[!is.na(c)])) > 1, logical(1))
  if (any(!keep)) msgs <- c(msgs, paste0("Dropped constant/empty columns: ",
                                         paste(names(df)[!keep], collapse = ", ")))
  df <- df[, keep, drop = FALSE]
  high <- vapply(df, function(c) is.factor(c) && nlevels(droplevels(c)) > 30, logical(1))
  if (any(high)) msgs <- c(msgs, paste0("Dropped high-cardinality (ID/text-like) columns: ",
                                        paste(names(df)[high], collapse = ", ")))
  df <- df[, !high, drop = FALSE]
  for (v in names(df))
    if (is.numeric(df[[v]]) && length(unique(df[[v]][!is.na(df[[v]])])) <= 8)
      df[[v]] <- factor(df[[v]])
  hi_na <- vapply(df, function(c) mean(is.na(c)) > 0.5, logical(1))
  if (any(hi_na)) msgs <- c(msgs, paste0("Dropped columns >50% missing: ",
                                         paste(names(df)[hi_na], collapse = ", ")))
  df <- df[, !hi_na, drop = FALSE]
  n0 <- nrow(df); df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < n0) msgs <- c(msgs, paste0("Removed ", n0 - nrow(df), " rows with missing values."))
  if (nrow(df) > 8000) { set.seed(1); df <- df[sample(nrow(df), 8000), , drop = FALSE]
  msgs <- c(msgs, "Subsampled to 8000 rows for performance.") }
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- droplevels(df[[v]])
  cont <- names(df)[vapply(df, is.numeric, logical(1))]
  cat  <- names(df)[vapply(df, is.factor,  logical(1))]
  list(data = df[c(cont, cat)], cont_vars = cont, cat_vars = cat, msgs = msgs)
}

# ---- Train / holdout split --------------------------------------------------
make_split <- function(df, frac, strat = NULL, seed = 7) {
  set.seed(seed)
  if (!is.null(strat) && strat %in% names(df) && is.factor(df[[strat]])) {
    idx <- unlist(lapply(split(seq_len(nrow(df)), df[[strat]]), function(ii)
      if (length(ii) <= 1) ii else sample(ii, max(1, floor(length(ii) * frac)))),
      use.names = FALSE)
  } else {
    idx <- sample(seq_len(nrow(df)), max(1, floor(nrow(df) * frac)))
  }
  idx <- sort(unique(idx)); ho <- setdiff(seq_len(nrow(df)), idx)
  if (length(ho) == 0) { ho <- idx[length(idx)]; idx <- idx[-length(idx)] }
  list(train = df[idx, , drop = FALSE], holdout = df[ho, , drop = FALSE])
}

# ---- Metric helpers used by the exploratory tabs ----------------------------
make_breaks <- function(x, k = 4) {
  qs <- quantile(x, probs = seq(0, 1, length.out = k + 1), na.rm = TRUE)
  qs[1] <- -Inf; qs[length(qs)] <- Inf; unique(qs)
}
discretize_df <- function(df, cont_vars, cat_vars, breaks) {
  out <- df
  for (v in cont_vars) out[[v]] <- cut(df[[v]], breaks = breaks[[v]], include.lowest = TRUE)
  out[cont_vars] <- lapply(out[cont_vars], factor)
  out[c(cont_vars, cat_vars)]
}
tvd <- function(x_real, x_syn) {
  lv <- union(levels(factor(x_real)), levels(factor(x_syn)))
  pr <- prop.table(table(factor(x_real, levels = lv)))
  ps <- prop.table(table(factor(x_syn,  levels = lv)))
  0.5 * sum(abs(pr - ps))
}
cramerV <- function(x, y) {
  tbl <- table(x, y); chi <- suppressWarnings(chisq.test(tbl)$statistic)
  k <- min(nrow(tbl), ncol(tbl)); if (k < 2) return(0)
  as.numeric(sqrt(chi / (sum(tbl) * (k - 1))))
}
assoc_matrix <- function(df) {
  v <- names(df); m <- matrix(0, length(v), length(v), dimnames = list(v, v))
  for (i in seq_along(v)) for (j in seq_along(v)) m[i, j] <- cramerV(df[[i]], df[[j]]); m
}
distinguish <- function(real_df, syn_df) {
  real_df$.src <- 0; syn_df$.src <- 1
  dat <- rbind(real_df, syn_df); dat$.src <- factor(dat$.src, levels = c(0, 1))
  fit <- rpart(.src ~ ., data = dat, method = "class",
               control = rpart.control(cp = 0.001, minbucket = 10))
  p <- predict(fit, type = "prob")[, "1"]; cprop <- mean(dat$.src == "1")
  data.frame(pMSE = round(mean((p - cprop)^2), 5),
             Accuracy = round(mean((p > 0.5) == (dat$.src == "1")), 3))
}

# ---- Clinical evaluation metric functions -----------------------------------
ks_d <- function(a, b) {
  a <- a[is.finite(a)]; b <- b[is.finite(b)]
  if (length(a) < 2 || length(b) < 2) return(NA_real_)
  suppressWarnings(as.numeric(ks.test(a, b)$statistic))
}
jsd_cat <- function(a, b) {
  lv <- union(levels(factor(a)), levels(factor(b)))
  p  <- as.numeric(prop.table(table(factor(a, levels = lv))))
  q  <- as.numeric(prop.table(table(factor(b, levels = lv))))
  m  <- 0.5 * (p + q)
  kl <- function(x, y) { i <- x > 0; sum(x[i] * log2(x[i] / y[i])) }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}
eta_ratio <- function(num, grp) {
  ok <- is.finite(num) & !is.na(grp); num <- num[ok]; grp <- droplevels(factor(grp[ok]))
  if (length(num) < 2 || nlevels(grp) < 2) return(0)
  gm <- mean(num); ss_t <- sum((num - gm)^2); if (ss_t == 0) return(0)
  ss_b <- sum(tapply(num, grp, function(g) length(g) * (mean(g) - gm)^2))
  sqrt(min(1, ss_b / ss_t))
}
mixed_assoc <- function(df, cont, cat) {
  v <- c(cont, cat); p <- length(v)
  m <- matrix(0, p, p, dimnames = list(v, v))
  for (i in seq_len(p)) for (j in seq_len(p)) {
    if (i == j) { m[i, j] <- 1; next }
    a <- v[i]; b <- v[j]; ai <- a %in% cont; bi <- b %in% cont
    val <- if (ai && bi)        abs(suppressWarnings(cor(df[[a]], df[[b]], use = "complete.obs")))
           else if (!ai && !bi) cramerV(df[[a]], df[[b]])
           else { num <- if (ai) df[[a]] else df[[b]]; grp <- if (ai) df[[b]] else df[[a]]
                  eta_ratio(num, grp) }
    m[i, j] <- if (is.na(val)) 0 else val
  }
  m
}
auc_score <- function(score, label) {
  ok <- is.finite(score) & !is.na(label); score <- score[ok]; label <- label[ok]
  n1 <- sum(label == 1); n0 <- sum(label == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score); (sum(r[label == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
tstr_one <- function(train, holdout, syn, target, cont, cat) {
  blank <- list(metric = "n/a", tstr = NA_real_, trtr = NA_real_, retention = NA_real_)
  if (!(target %in% c(cont, cat))) return(blank)
  preds <- setdiff(c(cont, cat), target)
  if (length(preds) < 1) return(blank)
  cols <- c(preds, target); is_class <- target %in% cat
  cats <- intersect(cat, cols)
  for (v in cats) {
    lv <- Reduce(union, list(levels(factor(train[[v]])), levels(factor(holdout[[v]])),
                             levels(factor(syn[[v]]))))
    train[[v]]   <- factor(train[[v]],   levels = lv)
    holdout[[v]] <- factor(holdout[[v]], levels = lv)
    syn[[v]]     <- factor(syn[[v]],     levels = lv)
  }
  fml <- as.formula(paste0("`", target, "` ~ ."))
  trn <- train[cols]; hod <- holdout[cols]; syd <- syn[cols]
  fit_model <- function(dat) {
    dat <- dat[stats::complete.cases(dat), , drop = FALSE]
    if (nrow(dat) < 10) return(NULL)
    if (is_class) {
      dat[[target]] <- droplevels(dat[[target]])
      if (nlevels(dat[[target]]) < 2) return(NULL)
    }
    tryCatch(randomForest(fml, data = dat, ntree = 200), error = function(e) NULL)
  }
  score_model <- function(rf) {
    if (is.null(rf)) return(NA_real_)
    if (is_class) {
      lv <- levels(hod[[target]])
      if (length(lv) == 2) {
        pr <- tryCatch(predict(rf, hod, type = "prob"), error = function(e) NULL)
        if (is.null(pr)) return(NA_real_)
        pos <- lv[2]; if (!(pos %in% colnames(pr))) return(NA_real_)
        auc_score(pr[, pos], as.integer(hod[[target]] == pos))
      } else {
        pr <- tryCatch(predict(rf, hod), error = function(e) NULL)
        if (is.null(pr)) return(NA_real_)
        mean(pr == hod[[target]])
      }
    } else {
      pr <- tryCatch(predict(rf, hod), error = function(e) NULL)
      if (is.null(pr)) return(NA_real_)
      y <- hod[[target]]; denom <- sum((y - mean(y))^2)
      if (denom == 0) return(NA_real_)
      1 - sum((y - pr)^2) / denom
    }
  }
  tstr <- score_model(fit_model(syd)); trtr <- score_model(fit_model(trn))
  retention <- if (is.na(tstr) || is.na(trtr) || trtr == 0) NA_real_ else tstr / trtr
  metric <- if (is_class) { if (nlevels(droplevels(holdout[[target]])) == 2) "AUC" else "Accuracy" } else "R^2"
  list(metric = metric, tstr = tstr, trtr = trtr, retention = retention)
}
col_ranges <- function(df, cont) {
  r <- sapply(cont, function(v) { rr <- diff(range(df[[v]], na.rm = TRUE)); if (rr == 0) 1 else rr })
  names(r) <- cont; r
}
gower_dist <- function(A, B, cont, cat, rng) {
  p <- length(cont) + length(cat); if (p == 0) return(matrix(0, nrow(A), nrow(B)))
  D <- matrix(0, nrow(A), nrow(B))
  for (v in cont) D <- D + abs(outer(A[[v]], B[[v]], "-")) / rng[[v]]
  for (v in cat) {
    av <- as.character(A[[v]]); bv <- as.character(B[[v]])
    D <- D + outer(av, bv, FUN = function(x, y) as.numeric(x != y))
  }
  D / p
}
nearest_dist <- function(A, B, cont, cat, rng) {
  if (nrow(A) == 0 || nrow(B) == 0) return(numeric(0))
  apply(gower_dist(A, B, cont, cat, rng), 1, min)
}
cap_rows <- function(df, k, seed = 99) {
  if (nrow(df) <= k) return(df)
  set.seed(seed); df[sample(nrow(df), k), , drop = FALSE]
}
privacy_eval <- function(syn, train, holdout, cont, cat, cap = 1000) {
  rng <- col_ranges(train, cont)
  tr <- cap_rows(train, cap); sy <- cap_rows(syn, cap); ho <- cap_rows(holdout, cap)
  for (v in cat) {
    lv <- Reduce(union, list(levels(factor(tr[[v]])), levels(factor(sy[[v]])),
                             levels(factor(ho[[v]]))))
    tr[[v]] <- factor(tr[[v]], levels = lv); sy[[v]] <- factor(sy[[v]], levels = lv)
    ho[[v]] <- factor(ho[[v]], levels = lv)
  }
  dcr_syn <- nearest_dist(sy, tr, cont, cat, rng)
  dcr_ho  <- nearest_dist(ho, tr, cont, cat, rng)
  exact   <- if (length(dcr_syn)) mean(dcr_syn < 1e-9) else NA_real_
  mem <- nearest_dist(tr, sy, cont, cat, rng)
  non <- nearest_dist(ho, sy, cont, cat, rng)
  list(dcr_syn_p5 = if (length(dcr_syn)) quantile(dcr_syn, 0.05, names = FALSE) else NA_real_,
       dcr_ho_p5  = if (length(dcr_ho))  quantile(dcr_ho,  0.05, names = FALSE) else NA_real_,
       exact_match = exact,
       mia_auc = auc_score(-c(mem, non), c(rep(1, length(mem)), rep(0, length(non)))))
}
eval_one <- function(syn, train, holdout, cont, cat, target, cap = 1000) {
  ks   <- if (length(cont)) mean(vapply(cont, function(v) ks_d(holdout[[v]], syn[[v]]), numeric(1)), na.rm = TRUE) else NA_real_
  jsdm <- if (length(cat))  mean(vapply(cat,  function(v) jsd_cat(holdout[[v]], syn[[v]]), numeric(1)), na.rm = TRUE) else NA_real_
  Ar <- mixed_assoc(holdout, cont, cat); As <- mixed_assoc(syn, cont, cat); off <- upper.tri(Ar)
  assoc <- if (any(off)) mean(abs(As[off] - Ar[off]), na.rm = TRUE) else NA_real_
  list(ks = ks, jsd = jsdm, assoc = assoc,
       tstr = tstr_one(train, holdout, syn, target, cont, cat),
       priv = privacy_eval(syn, train, holdout, cont, cat, cap = cap))
}
flatten_eval <- function(e) c(
  KS = e$ks, JSD = e$jsd, Assoc = e$assoc,
  TSTR = e$tstr$tstr, TRTR = e$tstr$trtr, Retention = e$tstr$retention,
  MIA_AUC = e$priv$mia_auc, DCR_syn_p5 = e$priv$dcr_syn_p5,
  DCR_holdout_p5 = e$priv$dcr_ho_p5, Exact_match = e$priv$exact_match)

# ---- Generators -------------------------------------------------------------
generate_bn <- function(real, cont_vars, cat_vars, k, bins = 6, iss = 1, seed = 2024) {
  brks <- lapply(real[cont_vars], make_breaks, k = bins)
  real_lab <- lapply(cont_vars, function(v)
    as.character(cut(real[[v]], breaks = brks[[v]], include.lowest = TRUE)))
  names(real_lab) <- cont_vars
  disc <- real
  for (v in cont_vars) disc[[v]] <- cut(real[[v]], breaks = brks[[v]], include.lowest = TRUE)
  disc[cont_vars] <- lapply(disc[cont_vars], factor)
  disc <- disc[c(cont_vars, cat_vars)]
  fit <- bn.fit(hc(disc), disc, method = "bayes", iss = iss)
  set.seed(seed); syn_disc <- rbn(fit, n = k)[, names(disc), drop = FALSE]
  out <- syn_disc
  for (v in cont_vars) out[[v]] <- vapply(as.character(syn_disc[[v]]), function(lvl) {
    pool <- real[[v]][ real_lab[[v]] == lvl ]; if (length(pool) == 0) NA_real_ else sample(pool, 1)
  }, numeric(1))
  out
}
generate_copula <- function(real, cont_vars, cat_vars, k) {
  out <- data.frame(row.names = seq_len(k))
  if (length(cont_vars) == 1) {
    out[[cont_vars]] <- quantile(real[[cont_vars]], probs = runif(k), type = 8, names = FALSE)
  } else if (length(cont_vars) >= 2) {
    X <- as.matrix(real[cont_vars])
    Z <- qnorm(apply(X, 2, function(col) rank(col) / (length(col) + 1)))
    R <- cor(Z)
    Lc <- tryCatch(chol(R), error = function(e) chol(0.99 * R + 0.01 * diag(ncol(R))))
    Un <- pnorm(matrix(rnorm(k * ncol(Z)), nrow = k) %*% Lc)
    for (j in seq_along(cont_vars))
      out[[cont_vars[j]]] <- quantile(X[, j], probs = Un[, j], type = 8, names = FALSE)
  }
  for (v in cat_vars)
    out[[v]] <- factor(sample(real[[v]], k, replace = TRUE), levels = levels(factor(real[[v]])))
  out[c(cont_vars, cat_vars)]
}
generate_one <- function(key, k, real, cont_vars, cat_vars) {
  if (key == "bn")
    return(tryCatch(generate_bn(real, cont_vars, cat_vars, k = k),
                    error = function(e) generate_copula(real, cont_vars, cat_vars, k)))
  if (key == "copula") return(generate_copula(real, cont_vars, cat_vars, k))
  s <- tryCatch(syn(real, method = key, k = k, seed = 2024, print.flag = FALSE),
                error = function(e) syn(real, method = "cart", k = k, seed = 2024, print.flag = FALSE))
  s$syn
}

# ---- Reusable warning text --------------------------------------------------
demo_warning_html <- paste0(
  "<p>You are using the <b>public online demo</b> of the Synthetic Health Data Generator. ",
  "It runs on <b>built-in synthetic sample data only</b>, so you can explore how the app ",
  "looks and works.</p>",
  "<p style='color:#b91c1c; font-weight:600;'>Do not upload or enter any real, personal, or ",
  "patient data into this demo. It is hosted on a shared public server and is not a private ",
  "or secure environment for sensitive health information.</p>",
  "<p>To analyse your own data, <b>download the application and run it locally on your own ",
  "computer</b>, where your data never leaves your machine.</p>")

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(
    tags$div(
      style = "display:flex; align-items:center; gap:12px;",
      HTML(paste0(
        '<svg width="46" height="46" viewBox="0 0 46 46" xmlns="http://www.w3.org/2000/svg">',
        '<rect x="1" y="1" width="44" height="44" rx="10" fill="#1f77b4"/>',
        '<rect x="20.5" y="9" width="5" height="17" rx="1.5" fill="#ffffff"/>',
        '<rect x="14.5" y="15" width="17" height="5" rx="1.5" fill="#ffffff"/>',
        '<circle cx="13" cy="34" r="3" fill="#ff7f0e"/>',
        '<circle cx="23" cy="31" r="3" fill="#ff7f0e"/>',
        '<circle cx="33" cy="28" r="3" fill="#ff7f0e"/></svg>')),
      tags$span("Synthetic Health Data Generator - model & fidelity explorer"),
      tags$span(style = "font-size:13px; color:#fff; background:#b91c1c; padding:3px 10px;
                         border-radius:12px; margin-left:6px;", "PUBLIC DEMO")
    )
  ),

  # ===== Persistent privacy banner (always visible, every tab) =====
  div(
    style = "background:#b91c1c; color:#ffffff; padding:12px 16px; margin:0 0 14px 0;
             border-radius:6px; font-size:15px; line-height:1.45;",
    HTML(paste0(
      "<b>&#9888; This is a public demo on built-in synthetic data.</b> ",
      "Do <b>not</b> upload or enter real or patient data here &mdash; this is a shared public ",
      "server and is not private or secured. To analyse your own data safely, download and run ",
      "the app on your own machine: ")),
    tags$a(href = DOWNLOAD_URL, target = "_blank",
           style = "color:#ffffff; font-weight:bold; text-decoration:underline;",
           "Download the app \u2192")
  ),

  fluidRow(
    # ===== LEFT RAIL: Models, Generate, Display + buttons. =====
    column(
      width = 3,
      wellPanel(
        style = "position: sticky; top: 10px;",
        # Prominent download call-to-action at the very top of the controls.
        tags$a(href = DOWNLOAD_URL, target = "_blank",
               class = "btn btn-primary btn-block",
               style = "width:100%; margin-bottom:8px; font-weight:600;",
               icon("download"), " Download to use your own data"),
        tags$hr(),
        tags$h4("Models", style = "margin-top:0;"),
        selectInput("modelA", "Model A", choices = MODEL_CHOICES, selected = "bn"),
        selectInput("modelB", "Model B", choices = MODEL_CHOICES, selected = "cart"),
        radioGroupButtons("which", "Single-model tabs show",
                          choices = c("Model A", "Model B"), selected = "Model B",
                          status = "info", justified = TRUE),
        tags$hr(),
        tags$h4("Generate"),
        radioGroupButtons("fidelity", "Fidelity level", choices = names(fidelity_frac),
                          selected = "3 - High", direction = "vertical",
                          status = "primary", justified = TRUE),
        numericInput("n_new", "Number of synthetic records", value = 1500,
                     min = 2, max = DEMO_MAX_SYNTH, step = 500),
        helpText(paste0("Must be greater than the loaded data size. Demo limit: ",
                        format(DEMO_MAX_SYNTH, big.mark = ","), " records.")),
        tags$hr(),
        tags$h4("Display"),
        selectInput("xvar", "Scatter X axis", choices = names(real_demo), selected = "bmi"),
        selectInput("yvar", "Scatter Y axis", choices = names(real_demo), selected = "sbp"),
        tags$hr(),
        actionButton("show_export", "Export", icon = icon("download"), width = "100%"),
        br(), br(),
        actionButton("show_about", "About / Guide", icon = icon("circle-info"), width = "100%"),
        tags$hr(),
        tags$strong("Active settings"), verbatimTextOutput("params"),
        tags$hr(),
        tags$small(HTML(paste0(
          "Created by <b>Abraham Alvandi</b><br>Version: <b>SHD.V004.06.2026 (public demo)</b><br>",
          "<a href='https://alvandi.weebly.com/' target='_blank'>Profile</a>")))
      )
    ),

    # ===== MAIN PANEL: four purpose-based sections. =====
    column(
      width = 9,
      tabsetPanel(
        id = "main",
        tabPanel(
          "Data", value = "data", br(),
          div(style = "background:#f5f7fa; border-left:4px solid #1f77b4; padding:10px 14px; margin-bottom:14px; border-radius:4px;",
              HTML(paste0(
                "<b>What is this?</b> This app creates realistic but <b>made-up</b> patient data, ",
                "lets you compare two ways of making it, and grades how <b>realistic</b> and how ",
                "<b>private</b> the result is. <b>This online demo runs on a built-in synthetic ",
                "sample dataset only</b> (1,000 made-up records) so you can see how everything ",
                "works without providing any data of your own. New here? Click <b>About / Guide</b> ",
                "on the left for a plain-language walkthrough of every step and score."))),
          div(style = "background:#fff4f4; border:1px solid #f0c2c2; padding:10px 14px; margin-bottom:14px; border-radius:4px;",
              HTML(paste0(
                "<b style='color:#b91c1c;'>Why is there no upload here?</b> Uploading is disabled ",
                "in the public demo on purpose, so that no real or sensitive data can ever reach ",
                "this shared server. To work with your own dataset, "),
              ),
              tags$a(href = DOWNLOAD_URL, target = "_blank", style = "font-weight:bold;",
                     "download the full app"),
              HTML(" and run it on your own computer.")),
          verbatimTextOutput("data_info"),
          br(), strong("Detected variables"), tableOutput("var_table"),
          br(), strong("Preview (first rows)"), tableOutput("data_preview")
        ),
        tabPanel(
          "Explore (single model)", value = "explore", br(),
          tabsetPanel(
            tabPanel("Scatter",       plotOutput("scatter", height = "470px")),
            tabPanel("Distributions", plotOutput("density", height = "470px")),
            tabPanel("Marginal fidelity",
                     br(), p("Total Variation Distance per variable (lower = better)."),
                     plotOutput("tvd_plot", height = "300px"), tableOutput("tvd_tbl")),
            tabPanel("Relationships",
                     br(), p("Relationship preservation (selected model)."),
                     textOutput("assoc_txt"), br(), plotOutput("corr_heat", height = "400px")),
            tabPanel("Distinguishability",
                     br(), p("Classifier real-vs-synthetic. Lower pMSE / accuracy near the synthetic share = realistic."),
                     tableOutput("pmse_tbl"))
          )
        ),
        tabPanel(
          "Compare models (A vs B)", value = "compare", br(),
          tabsetPanel(
            tabPanel("Category proportions",
                     br(), p("Category proportions for Real and the two chosen models."),
                     plotOutput("cat_props", height = "520px")),
            tabPanel("Correlation structure",
                     br(), p("Real-minus-synthetic correlation; near 0 = preserved."),
                     plotOutput("corr_struct", height = "440px")),
            tabPanel("Utility summary",
                     br(), p("Side-by-side utility metrics for the two chosen models."),
                     tableOutput("summary_tbl"))
          )
        ),
        tabPanel(
          "Clinical evaluation", value = "clinical", br(),
          fluidRow(
            column(
              width = 8,
              tabsetPanel(
                tabPanel("Single level",
                         br(),
                         p("Press 'Run clinical evaluation' on the right. Fidelity and privacy ",
                           "are assessed against the held-out real sample at the currently ",
                           "selected fidelity level."),
                         tableOutput("clinical_tbl"),
                         br(), textOutput("clinical_note")),
                tabPanel("Fidelity sweep",
                         br(),
                         p("Press 'Run fidelity sweep' on the right. Both models are scored at ",
                           "all four fidelity levels so the utility-vs-privacy trade-off is ",
                           "visible as a curve."),
                         plotOutput("sweep_plot", height = "480px"),
                         br(), textOutput("sweep_note"),
                         br(), strong("Full sweep results"), tableOutput("sweep_tbl"))
              )
            ),
            column(
              width = 4,
              wellPanel(
                style = "position: sticky; top: 10px;",
                tags$h4("Run evaluation", style = "margin-top:0;"),
                sliderInput("train_frac", "Training fraction (rest = holdout)",
                            min = 0.5, max = 0.9, value = 0.7, step = 0.05),
                selectInput("tstr_target", "Prediction target (TSTR)",
                            choices = names(real_demo), selected = "diabetes"),
                actionButton("run_eval", "Run clinical evaluation",
                             class = "btn-primary", width = "100%"),
                br(), br(),
                actionButton("run_sweep", "Run fidelity sweep (all 4 levels)",
                             class = "btn-success", width = "100%"),
                helpText("Generators are fit on the training split; all scores compare to the ",
                         "holdout. The sweep is the heavy step (8 evaluations).")
              )
            )
          )
        )
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # ===== Mandatory privacy warning shown once on load =====
  showModal(modalDialog(
    title = HTML("<span style='color:#b91c1c;'>&#9888; Public demo &mdash; please read</span>"),
    easyClose = FALSE,
    footer = tagList(
      tags$a(href = DOWNLOAD_URL, target = "_blank", class = "btn btn-primary",
             icon("download"), " Download to use on your machine"),
      modalButton("Continue to the demo")
    ),
    HTML(demo_warning_html)
  ))

  # Demo always uses the built-in synthetic data (no upload).
  prepared <- reactive(prepare_data(real_demo))

  real_full   <- reactive(prepared()$data)
  cont_vars   <- reactive(prepared()$cont_vars)
  cat_vars    <- reactive(prepared()$cat_vars)
  splitd      <- reactive(make_split(real_full(), input$train_frac, input$tstr_target))
  real_train  <- reactive(splitd()$train)
  real_holdout<- reactive(splitd()$holdout)

  real      <- reactive(real_full())
  breaks_metric <- reactive(lapply(real()[cont_vars()], make_breaks, k = 4))
  real_disc <- reactive(discretize_df(real(), cont_vars(), cat_vars(), breaks_metric()))

  observeEvent(prepared(), {
    cv <- cont_vars(); vars <- names(real_full()); nr <- nrow(real_full()); ct <- cat_vars()
    if (length(vars) >= 1) {
      updateSelectInput(session, "xvar", choices = vars,
                        selected = if (length(cv) >= 1) cv[1] else vars[1])
      updateSelectInput(session, "yvar", choices = vars,
                        selected = if (length(cv) >= 2) cv[2] else
                          if (length(vars) >= 2) vars[2] else vars[1])
      updateSelectInput(session, "tstr_target", choices = vars,
                        selected = if (length(ct) >= 1) ct[length(ct)] else vars[1])
    }
    updateNumericInput(session, "n_new", min = nr + 1, max = DEMO_MAX_SYNTH,
                       value = max(nr + 1, ceiling(nr * 1.5)))
  })

  observeEvent(input$show_export, {
    showModal(modalDialog(
      title = "Export data", size = "m", easyClose = TRUE, footer = modalButton("Close"),
      p("Download the synthetic datasets generated at the current fidelity level, or the ",
        "full results of the fidelity sweep. (These are made-up records produced from the ",
        "built-in demo data.)"),
      downloadButton("dlA", "Model A - synthetic (CSV)"), br(), br(),
      downloadButton("dlB", "Model B - synthetic (CSV)"), br(), br(),
      downloadButton("dl_sweep", "Fidelity sweep results (CSV)"),
      br(), br(),
      helpText("The sweep export contains data only after you have run the fidelity sweep.")
    ))
  })

  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About this app", size = "l", easyClose = TRUE, footer = modalButton("Close"),
      HTML(paste0(
        "<p><b>Version:</b> SHD.V004.06.2026 (public demo) &middot; <b>Author:</b> Abraham Alvandi ",
        "(<a href='https://alvandi.weebly.com/' target='_blank'>profile</a>)</p>",

        "<div style='background:#fff4f4; border:1px solid #f0c2c2; padding:10px 14px; ",
        "border-radius:4px; margin-bottom:12px;'>",
        demo_warning_html,
        "<p style='margin-bottom:0;'><a href='", DOWNLOAD_URL, "' target='_blank' ",
        "style='font-weight:bold;'>Download the full application &#8594;</a></p>",
        "</div>",

        "<h4>What this app does, in plain words</h4>",
        "<p>It creates <b>made-up patient data</b> that looks and behaves like real data ",
        "but is not tied to any real person. You can compare two different ways of making it, ",
        "and the app checks two things for you: is the made-up data <b>realistic</b>, and is it ",
        "<b>safe</b> (hard to trace back to real people)?</p>",

        "<h4>How it works, step by step</h4>",
        "<ol>",
        "<li><b>Data.</b> This demo uses a built-in synthetic sample dataset. (The downloadable ",
        "version lets you load your own file.)</li>",
        "<li><b>Split.</b> The app sets aside part of the data (the <i>holdout</i>) and ",
        "never shows it to the data-making methods. This unseen part is the fair yardstick used ",
        "later for grading.</li>",
        "<li><b>Generate.</b> Two methods (A and B) learn the patterns in the training part and ",
        "produce brand-new, made-up rows.</li>",
        "<li><b>Fidelity dial.</b> This decides how closely the made-up data copies the real ",
        "patterns. High = very close (more useful, less private). Low = blurry (more private, ",
        "less useful).</li>",
        "<li><b>Evaluate.</b> The app grades the made-up data against the unseen holdout on ",
        "realism and on safety.</li>",
        "</ol>",

        "<h4>What the realism (fidelity) scores mean</h4>",
        "<ul>",
        "<li><b>KS</b> (Kolmogorov-Smirnov distance, numeric columns): are values spread the same ",
        "way as real? The largest gap between the real and synthetic spread of a numeric column. ",
        "0 = identical, 1 = completely different. <i>Lower is better.</i></li>",
        "<li><b>JSD</b> (Jensen-Shannon divergence, category columns): do the category mixes match ",
        "(e.g. share of smokers)? 0 = identical, 1 = completely different. <i>Lower is better.</i></li>",
        "<li><b>Association preservation</b>: are the relationships between variables kept (e.g. ",
        "higher BMI tends to go with higher blood pressure)? Average change in the strength of the ",
        "relationships between every pair of variables. 0 = relationships perfectly kept. ",
        "<i>Lower is better.</i></li>",
        "<li><b>TSTR utility &amp; retention</b>: train a prediction model on the made-up data, ",
        "then test it on real unseen data. <b>Retention</b> near 1.0 means the made-up data is ",
        "about as useful as real data for that prediction. <i>Higher is better.</i></li>",
        "<li><b>TRTR</b> - Train on Real, Test on Real. The baseline TSTR is compared against. ",
        "<i>Reference only.</i></li>",
        "<li><b>Distinguishability (pMSE)</b>, on the Explore tabs: can a simple model tell ",
        "made-up from real apart? If it struggles, the data is realistic.</li>",
        "</ul>",

        "<h4>What the safety (privacy) scores mean</h4>",
        "<ul>",
        "<li><b>DCR</b> (distance to closest record): how close is each made-up row to a real ",
        "training person? Too close is risky. We compare it to how close the unseen real holdout ",
        "sits to training - the made-up data should be <i>no closer than that benchmark</i>. ",
        "<i>Higher distance = safer.</i></li>",
        "<li><b>Exact-match share</b>: fraction of made-up rows that are identical copies of a ",
        "real row. Should be near 0. <i>Lower is safer.</i></li>",
        "<li><b>Membership-inference AUC</b>: could an attacker guess whether a specific person ",
        "was in the training data, just from the made-up data? 0.5 = cannot tell (good). ",
        "<i>Closer to 0.5 is safer.</i></li>",
        "</ul>",

        "<h4>The trade-off to remember</h4>",
        "<p>Turning fidelity <b>up</b> makes the data more realistic but easier to trace back to ",
        "real people. Turning it <b>down</b> protects privacy but makes the data less useful. ",
        "The <b>Fidelity sweep</b> shows this tug-of-war across all four levels so you can pick a ",
        "sensible middle point rather than trusting a single number.</p>",

        "<p><i>Important:</i> these scores are a research and quality-check aid, not a legal ",
        "guarantee that data is safe to release. Always follow your organisation's data-governance ",
        "rules for real patient data - and never put such data into this public demo.</p>",

        "<h4>License</h4>",
        "<p><b>MIT License</b> &middot; Copyright (c) 2026 Abraham Alvandi.</p>",
        "<p>Permission is hereby granted, free of charge, to any person obtaining a copy of this ",
        "software and associated documentation files (the \"Software\"), to deal in the Software ",
        "without restriction, including without limitation the rights to use, copy, modify, ",
        "merge, publish, distribute, sublicense, and/or sell copies of the Software, and to ",
        "permit persons to whom the Software is furnished to do so, subject to the following ",
        "conditions: the above copyright notice and this permission notice shall be included in ",
        "all copies or substantial portions of the Software.</p>",
        "<p>THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, ",
        "INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR ",
        "PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE ",
        "FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR ",
        "OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER ",
        "DEALINGS IN THE SOFTWARE.</p>")
      )))
  })

  output$data_info <- renderText({
    p <- prepared(); d <- p$data; sp <- splitd()
    paste0("Public demo - using built-in synthetic sample data only ",
           "(download the app to analyse your own data locally).",
           "\nRecords: ", nrow(d), "   |   Variables used: ", ncol(d),
           "   (continuous: ", length(p$cont_vars), ", categorical: ", length(p$cat_vars), ")",
           "\nTrain / holdout split: ", nrow(sp$train), " / ", nrow(sp$holdout),
           "  (stratified on '", input$tstr_target, "' when categorical)",
           if (length(p$msgs)) paste0("\n\nNotes:\n- ", paste(p$msgs, collapse = "\n- ")) else "")
  })
  output$var_table <- renderTable({
    p <- prepared(); d <- p$data
    data.frame(Variable = names(d),
               Type = ifelse(names(d) %in% p$cont_vars, "continuous", "categorical"),
               Unique_values = vapply(d, function(c) length(unique(c)), integer(1)),
               row.names = NULL)
  }, striped = TRUE, bordered = TRUE)
  output$data_preview <- renderTable(head(real_full(), 8), striped = TRUE, bordered = TRUE)

  two_models <- reactive({
    validate(need(!is.na(input$n_new) && input$n_new > nrow(real_full()),
                  paste0("Enter a number of synthetic records greater than ", nrow(real_full()), ".")),
             need(input$n_new <= DEMO_MAX_SYNTH,
                  paste0("For the public demo, keep synthetic records at or below ",
                         format(DEMO_MAX_SYNTH, big.mark = ","), ".")),
             need(input$modelA != input$modelB, "Choose two different models for A and B."))
    f <- fidelity_frac[[input$fidelity]]; k <- as.integer(input$n_new)
    rl <- real_train(); cv <- cont_vars(); ct <- cat_vars()
    withProgress(message = "Generating synthetic data (both models)...", value = 0.5, {
      a0 <- generate_one(input$modelA, k, rl, cv, ct)
      b0 <- generate_one(input$modelB, k, rl, cv, ct)
      set.seed(101); A <- apply_fidelity(a0, rl, f, cv, ct)
      set.seed(202); B <- apply_fidelity(b0, rl, f, cv, ct)
      list(A = A, B = B, labA = model_label(input$modelA), labB = model_label(input$modelB))
    })
  })
  disc_two <- reactive({
    tm <- two_models()
    list(A = discretize_df(tm$A, cont_vars(), cat_vars(), breaks_metric()),
         B = discretize_df(tm$B, cont_vars(), cat_vars(), breaks_metric()),
         labA = tm$labA, labB = tm$labB)
  })
  sel_data  <- reactive({ tm <- two_models(); if (input$which == "Model B") tm$B else tm$A })
  sel_disc  <- reactive(discretize_df(sel_data(), cont_vars(), cat_vars(), breaks_metric()))
  sel_label <- reactive({ tm <- two_models(); if (input$which == "Model B") tm$labB else tm$labA })

  output$params <- renderText({
    f <- fidelity_frac[[input$fidelity]]
    paste0("A: ", model_label(input$modelA), "  |  B: ", model_label(input$modelB),
           "\nFidelity = ", f, "  (", round(f * 100), "% model + ", round((1 - f) * 100), "% noise)",
           "\nSynthetic: ", input$n_new, "  |  Train: ", nrow(real_train()),
           "  Holdout: ", nrow(real_holdout()),
           "\nTSTR target: ", input$tstr_target)
  })

  dl_name <- function(key) paste0("synthetic_", gsub("[^A-Za-z0-9]", "_", model_label(key)),
                                  "_fid", substr(input$fidelity, 1, 1), "_n", input$n_new, ".csv")
  output$dlA <- downloadHandler(
    filename = function() dl_name(input$modelA),
    content  = function(file) write.csv(two_models()$A, file, row.names = FALSE))
  output$dlB <- downloadHandler(
    filename = function() dl_name(input$modelB),
    content  = function(file) write.csv(two_models()$B, file, row.names = FALSE))

  output$scatter <- renderPlot({
    validate(need(input$xvar %in% names(real()) && input$yvar %in% names(real()),
                  "Select X and Y variables."))
    sx <- input$xvar; sy <- input$yvar; synth <- sel_data()
    df <- rbind(data.frame(x = real()[[sx]],  y = real()[[sy]],  source = "Real"),
                data.frame(x = synth[[sx]], y = synth[[sy]], source = "Synthetic"))
    df <- df[stats::complete.cases(df), ]
    jw <- if (is.factor(df$x) || is.character(df$x)) 0.2 else 0
    jh <- if (is.factor(df$y) || is.character(df$y)) 0.2 else 0
    ggplot(df, aes(x, y, color = source)) +
      geom_jitter(alpha = 0.5, size = 2.2, width = jw, height = jh) +
      scale_color_manual(values = c(Real = "#1f77b4", Synthetic = "#ff7f0e")) +
      labs(title = paste0(sel_label(), "  |  fidelity ", input$fidelity), x = sx, y = sy, color = NULL) +
      theme_minimal(base_size = 14) + theme(legend.position = "top")
  })

  output$density <- renderPlot({
    validate(need(length(cont_vars()) >= 1, "No numeric variables to plot."))
    synth <- sel_data(); cv <- cont_vars(); lab <- paste0("Synthetic (", sel_label(), ")")
    to_long <- function(df, src) do.call(rbind, lapply(cv, function(v)
      data.frame(value = df[[v]], variable = v, source = src)))
    d <- rbind(to_long(real(), "Real"), to_long(synth, lab))
    d <- d[stats::complete.cases(d), ]; d$source <- factor(d$source, levels = c("Real", lab))
    ggplot(d, aes(value, fill = source)) +
      geom_density(alpha = 0.4) + facet_wrap(~variable, scales = "free") +
      scale_fill_manual(values = setNames(c("#1f77b4", "#ff7f0e"), c("Real", lab))) +
      labs(title = paste0("Marginal distributions  |  ", sel_label(), "  |  fidelity ", input$fidelity),
           x = NULL, y = "Density", fill = NULL) +
      theme_minimal(base_size = 14) + theme(legend.position = "top")
  })

  tvd_data <- reactive({
    sd <- sel_disc()
    data.frame(Variable = names(real_disc()),
               TVD = sapply(names(real_disc()), function(v) round(tvd(real_disc()[[v]], sd[[v]]), 3)),
               row.names = NULL)
  })
  output$tvd_tbl  <- renderTable(tvd_data(), striped = TRUE, bordered = TRUE)
  output$tvd_plot <- renderPlot({
    ggplot(tvd_data(), aes(reorder(Variable, TVD), TVD)) +
      geom_col(fill = "#ff7f0e") + coord_flip() +
      labs(x = NULL, y = "Total Variation Distance") + theme_minimal(base_size = 14)
  })

  output$assoc_txt <- renderText({
    A_r <- assoc_matrix(real_disc()); A_s <- assoc_matrix(sel_disc()); off <- upper.tri(A_r)
    paste0("Mean |delta Cramer's V| across all variable pairs (lower = better): ",
           round(mean(abs(A_s[off] - A_r[off])), 3))
  })
  output$corr_heat <- renderPlot({
    validate(need(length(cont_vars()) >= 2, "Need at least two numeric variables."))
    synth <- sel_data(); cv <- cont_vars()
    d <- cor(real()[cv]) - cor(synth[cv], use = "complete.obs")
    long <- as.data.frame(as.table(d)); names(long) <- c("V1", "V2", "Diff")
    ggplot(long, aes(V1, V2, fill = Diff)) +
      geom_tile() + geom_text(aes(label = round(Diff, 2)), size = 4) +
      scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                           midpoint = 0, limits = c(-0.4, 0.4)) +
      labs(title = "Correlation difference (real - synthetic); near 0 is best",
           x = NULL, y = NULL, fill = "delta r") + theme_minimal(base_size = 14)
  })

  output$pmse_tbl <- renderTable(distinguish(real_disc(), sel_disc()), striped = TRUE, bordered = TRUE)

  output$cat_props <- renderPlot({
    dt <- disc_two()
    prop_tab <- bind_rows(
      real_disc() %>% mutate(source = "Real"),
      dt$A %>% mutate(source = dt$labA),
      dt$B %>% mutate(source = dt$labB)
    ) %>%
      mutate(across(-source, as.character)) %>%
      pivot_longer(-source, names_to = "variable", values_to = "level") %>%
      count(source, variable, level) %>%
      group_by(source, variable) %>% mutate(prop = n / sum(n)) %>% ungroup()
    prop_tab$source <- factor(prop_tab$source, levels = c("Real", dt$labA, dt$labB))
    ggplot(prop_tab, aes(level, prop, fill = source)) +
      geom_col(position = "dodge") + facet_wrap(~variable, scales = "free_x", ncol = 4) +
      scale_fill_manual(values = setNames(c("#1f77b4", "#2ca02c", "#ff7f0e"),
                                          c("Real", dt$labA, dt$labB))) +
      labs(title = paste0("Category proportions  |  fidelity ", input$fidelity),
           x = NULL, y = "Proportion", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top")
  })

  output$corr_struct <- renderPlot({
    validate(need(length(cont_vars()) >= 2, "Need at least two numeric variables."))
    tm <- two_models(); cv <- cont_vars()
    mk <- function(synth, lab) {
      d <- cor(real()[cv]) - cor(synth[cv], use = "complete.obs")
      long <- as.data.frame(as.table(d)); names(long) <- c("V1", "V2", "Diff"); long$model <- lab; long
    }
    long <- rbind(mk(tm$A, paste0("Real - ", tm$labA)), mk(tm$B, paste0("Real - ", tm$labB)))
    ggplot(long, aes(V1, V2, fill = Diff)) +
      geom_tile() + geom_text(aes(label = round(Diff, 2)), size = 3) + facet_wrap(~model) +
      scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                           midpoint = 0, limits = c(-0.4, 0.4)) +
      labs(title = "Correlation difference (real - synthetic); near 0 is best",
           x = NULL, y = NULL, fill = "delta r") +
      theme_minimal(base_size = 13) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$summary_tbl <- renderTable({
    dt <- disc_two(); tm <- two_models(); cv <- cont_vars()
    A_real <- assoc_matrix(real_disc()); off <- upper.tri(A_real)
    mean_tvd  <- function(sd) mean(sapply(names(real_disc()), function(v) tvd(real_disc()[[v]], sd[[v]])))
    mean_cram <- function(sd) { A <- assoc_matrix(sd); mean(abs(A[off] - A_real[off])) }
    mean_corr <- function(s) {
      if (length(cv) < 2) return(NA_real_)
      cm_off <- upper.tri(cor(real()[cv]))
      mean(abs((cor(real()[cv]) - cor(s[cv], use = "complete.obs"))[cm_off]))
    }
    a_d <- distinguish(real_disc(), dt$A); b_d <- distinguish(real_disc(), dt$B)
    res <- data.frame(
      Metric = c("Mean marginal TVD (lower better)",
                 "Mean |delta Cramer's V| (lower better)",
                 "Mean |delta correlation| (lower better)",
                 "Distinguishability pMSE (lower better)",
                 "Distinguishability accuracy (lower better)"),
      A = round(c(mean_tvd(dt$A), mean_cram(dt$A), mean_corr(tm$A), a_d$pMSE, a_d$Accuracy), 3),
      B = round(c(mean_tvd(dt$B), mean_cram(dt$B), mean_corr(tm$B), b_d$pMSE, b_d$Accuracy), 3))
    names(res) <- c("Metric", tm$labA, tm$labB)
    res
  }, striped = TRUE, bordered = TRUE)

  # ---- Clinical evaluation: single level ----
  evaluation <- eventReactive(input$run_eval, {
    tm <- two_models(); tr <- real_train(); ho <- real_holdout()
    cv <- cont_vars(); ct <- cat_vars(); tgt <- input$tstr_target
    validate(need(nrow(ho) >= 5, "Holdout too small to evaluate; lower the training fraction."))
    withProgress(message = "Running clinical evaluation...", value = 0.2, {
      eA <- eval_one(tm$A, tr, ho, cv, ct, tgt); incProgress(0.4)
      eB <- eval_one(tm$B, tr, ho, cv, ct, tgt); incProgress(0.4)
      list(A = eA, B = eB, labA = tm$labA, labB = tm$labB, target = tgt)
    })
  })
  output$clinical_tbl <- renderTable({
    ev <- evaluation()
    fmt <- function(x) if (length(x) != 1 || is.na(x)) "NA" else formatC(x, digits = 3, format = "f")
    mk <- function(e) c(fmt(e$ks), fmt(e$jsd), fmt(e$assoc),
                        paste0(fmt(e$tstr$tstr), "  (", e$tstr$metric, ")"),
                        fmt(e$tstr$trtr), fmt(e$tstr$retention),
                        fmt(e$priv$dcr_syn_p5), fmt(e$priv$dcr_ho_p5),
                        fmt(e$priv$exact_match), fmt(e$priv$mia_auc))
    res <- data.frame(
      Metric = c("Distribution: mean KS, continuous (lower better)",
                 "Distribution: mean JSD, categorical (lower better)",
                 "Association preservation, mixed |delta| (lower better)",
                 "TSTR utility (higher better)",
                 "TRTR baseline (reference)",
                 "Utility retention TSTR/TRTR (-> 1 best)",
                 "Privacy: DCR 5th pct, synthetic (higher = safer)",
                 "Privacy: DCR 5th pct, holdout benchmark (reference)",
                 "Privacy: exact-match share (lower = safer)",
                 "Privacy: membership-inference AUC (-> 0.5 best)"),
      A = mk(ev$A), B = mk(ev$B), stringsAsFactors = FALSE, check.names = FALSE)
    names(res) <- c("Metric", ev$labA, ev$labB); res
  }, striped = TRUE, bordered = TRUE)
  output$clinical_note <- renderText({
    ev <- evaluation()
    paste0("Target: '", ev$target, "'. Generators fit on the training split; scores compare ",
           "synthetic to the holdout. Read fidelity (top) with privacy (bottom): higher fidelity ",
           "usually lowers DCR and raises the membership-inference AUC. DCR is interpretable ",
           "relative to the holdout benchmark - synthetic 5th-pct distance at least as large as ",
           "the holdout's means synthetic records are no closer to training data than a real ",
           "unseen sample. The membership-inference figure is a simplified distance-based attack.")
  })

  # ---- Clinical evaluation: fidelity sweep (all four levels) ----
  sweep <- eventReactive(input$run_sweep, {
    validate(need(input$modelA != input$modelB, "Choose two different models for A and B."),
             need(!is.na(input$n_new) && input$n_new > nrow(real_full()),
                  paste0("Enter a number of synthetic records greater than ", nrow(real_full()), ".")),
             need(input$n_new <= DEMO_MAX_SYNTH,
                  paste0("For the public demo, keep synthetic records at or below ",
                         format(DEMO_MAX_SYNTH, big.mark = ","), ".")),
             need(nrow(real_holdout()) >= 5, "Holdout too small; lower the training fraction."))
    tr <- real_train(); ho <- real_holdout(); cv <- cont_vars(); ct <- cat_vars()
    tgt <- input$tstr_target; k <- as.integer(input$n_new)
    labA <- model_label(input$modelA); labB <- model_label(input$modelB); levs <- names(fidelity_frac)
    withProgress(message = "Fidelity sweep (8 evaluations)...", value = 0.05, {
      a0 <- generate_one(input$modelA, k, tr, cv, ct); incProgress(0.1)
      b0 <- generate_one(input$modelB, k, tr, cv, ct); incProgress(0.1)
      rows <- list(); i <- 0; metric_type <- "n/a"
      for (L in levs) {
        f <- fidelity_frac[[L]]
        set.seed(1000 + i); A <- apply_fidelity(a0, tr, f, cv, ct)
        set.seed(2000 + i); B <- apply_fidelity(b0, tr, f, cv, ct)
        eA <- eval_one(A, tr, ho, cv, ct, tgt, cap = 600)
        eB <- eval_one(B, tr, ho, cv, ct, tgt, cap = 600)
        metric_type <- eA$tstr$metric
        rows[[length(rows) + 1]] <- data.frame(Level = L, Fidelity = f, Model = labA,
                                               t(flatten_eval(eA)), check.names = FALSE)
        rows[[length(rows) + 1]] <- data.frame(Level = L, Fidelity = f, Model = labB,
                                               t(flatten_eval(eB)), check.names = FALSE)
        i <- i + 1; incProgress(0.7 / length(levs))
      }
      df <- do.call(rbind, rows); df$Level <- factor(df$Level, levels = levs)
      list(tbl = df, levs = levs, metric_type = metric_type, labA = labA, labB = labB)
    })
  })

  output$sweep_plot <- renderPlot({
    sw <- sweep()
    long <- sw$tbl %>%
      select(Level, Model, Retention, MIA_AUC, KS, DCR_syn_p5) %>%
      pivot_longer(c(Retention, MIA_AUC, KS, DCR_syn_p5),
                   names_to = "metric", values_to = "value")
    lab_map <- c(Retention  = "Utility retention TSTR/TRTR (higher better)",
                 MIA_AUC    = "Membership-inference AUC (0.5 best)",
                 KS         = "Mean KS, continuous (lower better)",
                 DCR_syn_p5 = "DCR 5th pct, synthetic (higher safer)")
    long$metric <- factor(unname(lab_map[long$metric]), levels = unname(lab_map))
    ggplot(long, aes(Level, value, color = Model, group = Model)) +
      geom_line(linewidth = 1) + geom_point(size = 2.8) +
      facet_wrap(~metric, scales = "free_y") +
      scale_color_manual(values = setNames(c("#2ca02c", "#ff7f0e"), c(sw$labA, sw$labB))) +
      labs(title = "Fidelity sweep: utility vs privacy across the four levels",
           x = "Fidelity level", y = NULL, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", axis.text.x = element_text(angle = 20, hjust = 1))
  })
  output$sweep_note <- renderText({
    sw <- sweep()
    paste0("TSTR metric: ", sw$metric_type, ". Each generator is fit once on the training split; ",
           "the four levels re-apply the noise model to that base and score against the holdout. ",
           "Expected pattern: utility retention rises and KS falls as fidelity increases, while ",
           "privacy degrades (MIA AUC up, DCR down). Where the utility and privacy curves cross ",
           "is a natural operating point.")
  })
  output$sweep_tbl <- renderTable({
    sw <- sweep(); df <- sw$tbl
    df$Fidelity <- formatC(df$Fidelity, digits = 2, format = "f")
    num <- vapply(df, is.numeric, logical(1))
    df[num] <- lapply(df[num], function(x) formatC(x, digits = 3, format = "f"))
    df
  }, striped = TRUE, bordered = TRUE)
  output$dl_sweep <- downloadHandler(
    filename = function() paste0("fidelity_sweep_",
                                 gsub("[^A-Za-z0-9]", "_", input$tstr_target), ".csv"),
    content  = function(file) write.csv(sweep()$tbl, file, row.names = FALSE))
}

shinyApp(ui, server)
