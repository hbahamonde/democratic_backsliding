cat("\014")
rm(list=ls())


## ---- loadings
setwd("/Users/hectorbahamonde/research/democratic_backsliding/2025/")

## Packages
if (!require("pacman")) install.packages("pacman"); library(pacman) 
pacman::p_load(dplyr, forcats, ggplot2, patchwork)

## Data
load("/Users/hectorbahamonde/research/democratic_backsliding/2025/dat_t.RData")

## Normalize literal "<NA>" strings to proper NA for all character cols
dat.t <- dat.t %>% mutate(across(where(is.character), ~ na_if(., "<NA>")))

## Core covariates used in models
dat.t$Q8_1 <- suppressWarnings(as.numeric(dat.t$Q8_1))  # democratic satisfaction

## Q9 trust items: coerce to numeric 1..11, map DK (12) & junk to NA
q9_cols <- paste0("Q9_", 1:7)
dat.t[q9_cols] <- lapply(dat.t[q9_cols], function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("I don't know", "I dont know", "", "NA", "<NA>")] <- NA
  v <- suppressWarnings(as.numeric(x_chr))
  v[v == 12 | v < 0 | v > 10] <- NA_real_
  v
})

## Government distance building

## Map Q27_* to named party columns (0..10; DK=12 -> NA)
q27_old <- paste0("Q27_", 1:9)
q27_new <- c("q27_sdp","q27_ps","q27_kok","q27_kesk","q27_vihr","q27_vas","q27_rkp","q27_kd","q27_liike")

## Ensure numeric & rename
dat.t[q27_old] <- lapply(dat.t[q27_old], function(x) suppressWarnings(as.numeric(x)))
names(dat.t)[match(q27_old, names(dat.t))] <- q27_new

## Clamp to 0..10; DK (12) -> NA
dat.t <- dat.t %>%
  mutate(across(all_of(q27_new), ~ ifelse(. == 12 | . < 0 | . > 10, NA_real_, .)))

## Governing parties (Orpo cabinet) & seat weights
gov_cols <- c("q27_kok","q27_ps","q27_rkp","q27_kd")
w_named  <- c(kok = 48, ps = 46, rkp = 9, kd = 5)

## Row-wise weighted mean with renormalization over non-missing items
renorm_wmean <- function(x, w) {
  ok <- !is.na(x)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

## Seat-weighted (renormalized) closeness & distance (0..10)
dat.t <- dat.t %>%
  rowwise() %>%
  mutate(
    gov_closeness_w = {
      x <- c_across(all_of(gov_cols))
      w <- w_named[sub("^q27_", "", gov_cols)]
      renorm_wmean(x, w)
    },
    gov_distance_w = 10 - gov_closeness_w
  ) %>%
  ungroup()

## Unweighted mean closeness & distance (0..10)
Xg        <- as.matrix(dat.t[, gov_cols, drop = FALSE])
answered  <- rowSums(!is.na(Xg))
closeness_u <- rowSums(Xg, na.rm = TRUE) / pmax(answered, 1L)
closeness_u[answered == 0] <- NA_real_
dat.t$gov_closeness_u <- closeness_u
dat.t$gov_distance_u  <- 10 - dat.t$gov_closeness_u

## Closest-party distance (0..10)
max_close <- do.call(pmax, c(dplyr::select(dat.t, all_of(gov_cols)), list(na.rm = TRUE)))
all_missing <- rowSums(!is.na(dplyr::select(dat.t, all_of(gov_cols)))) == 0
dat.t$gov_distance_min <- ifelse(all_missing, NA_real_, 10 - max_close)

## 0–1 scaled versions used in plots & models
dat.t <- dat.t %>%
  mutate(
    gov_distance_w_01   = gov_distance_w   / 10,
    gov_distance_u_01   = gov_distance_u   / 10,
    gov_distance_min_01 = gov_distance_min / 10
  )

## Outcomes: techno5 and business5

## techno5 (Q13_6)
dat.t <- dat.t %>%
  mutate(
    q13_6_chr  = na_if(as.character(Q13_6), "I don't know"),
    techno_ord = factor(
      q13_6_chr,
      levels = c("Totally disagree","Somewhat disagree",
                 "Neither agree nor disagree","Somewhat agree","Totally agree"),
      ordered = TRUE
    ),
    techno5 = as.numeric(techno_ord)  # 1..5
  )

## business5 (Q13_7)
dat.t <- dat.t %>%
  mutate(
    q13_7_chr   = na_if(as.character(Q13_7), "I don't know"),
    business_ord = factor(
      q13_7_chr,
      levels = c("Totally disagree","Somewhat disagree",
                 "Neither agree nor disagree","Somewhat agree","Totally agree"),
      ordered = TRUE
    ),
    business5 = as.numeric(business_ord)  # 1..5
  )

## Controls used in the models

dat.t <- dat.t %>%
  mutate(
    ## Education (ordered; drop "Other/Don't know")
    educ_chr = na_if(as.character(Q6), "Other/Don't know"),
    educ_ord = factor(
      educ_chr,
      levels = c("Less than primary school (grades 1-9)",
                 "Primary school (grades 1-9)",
                 "Professional qualification",
                 "Matriculation",
                 "Master's degree or polytechnic degree",
                 "University degree"),
      ordered = TRUE
    ),
    ## Age (ordered)
    age_ord = factor(as.character(Q3),
                     levels = c("18-24","25-34","35-44","45-54","55+"),
                     ordered = TRUE),
    ## Gender (drop other/prefer-not)
    gender = fct_drop(fct_recode(factor(as.character(Q4)),
                                 Female = "Female",
                                 Male   = "Male",
                                 NULL   = "Other/do not want to say")),
    ## Region FE
    region = factor(as.character(Q5))
  )

## Six OLS models

m_w   <- lm(techno5   ~ gov_distance_w_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t)
m_u   <- lm(techno5   ~ gov_distance_u_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t)
m_min <- lm(techno5   ~ gov_distance_min_01 + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t)

b_w   <- lm(business5 ~ gov_distance_w_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t)
b_u   <- lm(business5 ~ gov_distance_u_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t)
b_min <- lm(business5 ~ gov_distance_min_01 + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t)

## Prediction helper & plots

## Average predicted value curve with design-matrix SEs (for lm)
avg_pred_curve <- function(model, var, grid = seq(0, 1, length.out = 101)) {
  stopifnot(inherits(model, "lm"))
  mf  <- model.frame(model)
  TT  <- terms(model)
  Vb  <- vcov(model)
  bet <- coef(model)
  
  do.call(rbind, lapply(grid, function(g) {
    nd <- mf; nd[[var]] <- g
    Xg <- model.matrix(TT, data = nd)
    a  <- colMeans(Xg)
    est <- sum(a * bet)
    se  <- sqrt(as.numeric(t(a) %*% Vb %*% a))
    data.frame(x = g,
               estimate = est,
               conf.low = est - 1.96 * se,
               conf.high = est + 1.96 * se)
  }))
}

## Technocracy panels
ap_w   <- avg_pred_curve(m_w,   "gov_distance_w_01")   %>% mutate(spec = "Seat-weighted")
ap_u   <- avg_pred_curve(m_u,   "gov_distance_u_01")   %>% mutate(spec = "Unweighted")
ap_min <- avg_pred_curve(m_min, "gov_distance_min_01") %>% mutate(spec = "Closest-party")

ap_all <- bind_rows(ap_w, ap_u, ap_min) %>%
  mutate(spec = factor(spec, levels = c("Seat-weighted","Unweighted","Closest-party")))

pred_plot_3panel <- ggplot(ap_all, aes(x, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.20) +
  geom_line(linewidth = 1) +
  facet_grid(. ~ spec) +
  labs(
    x = "Government distance (0 = very close, 1 = far)",
    y = "Predicted technocracy (1-5)",
    title = "Average predicted Technocracy vs. Government distance",
    subtitle = "Seat-weighted, unweighted, and closest-party; averaged over the empirical distribution of controls"
  ) +
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal(base_size = 12) +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.text = element_text(face = "bold"))

## Business panels (hide x-axis title on first two; show on last only)
ap_b_w   <- avg_pred_curve(b_w,   "gov_distance_w_01")
ap_b_u   <- avg_pred_curve(b_u,   "gov_distance_u_01")
ap_b_min <- avg_pred_curve(b_min, "gov_distance_min_01")

make_panel <- function(df, title_sub, show_x = FALSE) {
  ggplot(df, aes(x, estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.20) +
    geom_line(linewidth = 1) +
    labs(
      x = if (show_x) "Government distance (0 = very close, 1 = far)" else NULL,
      y = "Predicted business (1-5)",
      title = title_sub
    ) +
    coord_cartesian(ylim = c(1, 5)) +
    theme_minimal(base_size = 12)
}

p_b_w   <- make_panel(ap_b_w,   "Seat-weighted", show_x = FALSE)
p_b_u   <- make_panel(ap_b_u,   "Unweighted",    show_x = TRUE)   # x-axis label here
p_b_min <- make_panel(ap_b_min, "Closest-party", show_x = FALSE)

pred_plot_business_3panel <-
  (p_b_w | p_b_u | p_b_min) +
  plot_annotation(
    title = "Average predicted Business vs. Government distance",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )

#  technocracy error bars by party (Q15), dropping "Other"
p_load(dplyr, forcats, ggplot2)

parties <- c(
  "Centre Party of Finland (KESK)",
  "Christian Democrats of Finland (KD)",
  "Green Alliance (VIHR)",
  "Left Alliance (VAS)",
  "Movement Now",
  "National Coalition Party (NCP)",
  "Social Democratic Party of Finland (SDP)",
  "Swedish People's Party of Finland (RKP)",
  "True Finns (PS)"
)

dat_party <- dat.t %>%
  mutate(
    party = case_when(
      as.character(Q15) %in% parties ~ as.character(Q15),
      Q15 == "I did not vote"       ~ "Did not vote",
      Q15 == "I don't know"         ~ "Don't know",
      # "Other party or grouping" is dropped by sending to NA
      TRUE                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(techno5), !is.na(party)) %>%
  group_by(party) %>%
  summarise(
    n    = dplyr::n(),
    mean = mean(techno5),
    sd   = sd(techno5),
    se   = sd / sqrt(n),
    ci   = qt(0.975, df = pmax(n - 1, 1)) * se,
    .groups = "drop"
  ) %>%
  mutate(party = fct_reorder(party, mean))  # order by mean support

tech_by_party_plot <- ggplot(dat_party, aes(x = party, y = mean)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.15) +
  geom_point(size = 2) +
  coord_flip() +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    x = NULL,
    y = "Support for technocracy",
    title = "Technocracy by Party Choice"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_blank(),
    aspect.ratio = 1      # <- makes the plotting panel square
  )

# Convert POSIXct to Date
dates_fieldwork <- as.Date(dat.t$EndDate)

# Extract ordered unique months and years
fieldwork_months <- unique(format(sort(dates_fieldwork), "%B"))
fieldwork_years  <- unique(format(dates_fieldwork, "%Y"))

# Collapse to single strings
fieldwork_months_txt <- paste(fieldwork_months, collapse = " and ")
fieldwork_year_txt   <- paste(fieldwork_years, collapse = " and ")
## ----


################
#### reg_table
################

## ---- reg_table
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(stargazer, sandwich, lmtest, modelsummary, data.table, tibble)

# helper to replace Unicode dashes/minus with LaTeX-safe ASCII
sanitize_tex <- function(x) {
  x <- gsub("\u2013|\u2014", "--", x)  # en/em dash -> --
  x <- gsub("\u2212", "-", x)          # Unicode minus -> hyphen
  x
}

mods <- list(
  `Tech: Seat-wt` = m_w,
  `Tech: Unwt`    = m_u,
  `Tech: Closest` = m_min
)

coef_map <- c(
  "gov_distance_w_01"   = "Government distance (seat-weighted)",
  "gov_distance_u_01"   = "Government distance (unweighted)",
  "gov_distance_min_01" = "Government distance (closest-party)",
  "Q8_1"                = "Democratic satisfaction",
  "Q9_4"                = "Trust in politicians",
  "(Intercept)"         = "Constant"
)

fe_rows <- tibble::tribble(
  ~term,          ~`Tech: Seat-wt`, ~`Tech: Unwt`, ~`Tech: Closest`,
  "Education FE", "Yes",           "Yes",         "Yes",
  "Age FE",       "Yes",           "Yes",         "Yes",
  "Gender",       "Yes",           "Yes",         "Yes",
  "Region FE",    "Yes",           "Yes",         "Yes"
)

vc <- lapply(mods, function(m) sandwich::vcovHC(m, type = "HC1"))
dir.create("build", showWarnings = FALSE, recursive = TRUE)

tab_title <- sanitize_tex("OLS Estimates: Technocracy vs. Government Distance")
tab_notes <- sanitize_tex("HC1 robust standard errors in parentheses. Models include education, age, gender, and region fixed effects (omitted for brevity).")

# 1) full table (with \begin{table} ... \end{table})
modelsummary::msummary(
  models      = mods,
  output      = "build/table_models_full.tex",
  title       = tab_title,
  stars       = TRUE,
  estimate    = "{estimate}{stars}",
  statistic   = "({std.error})",
  vcov        = vc,
  coef_rename = coef_map,
  coef_omit   = "(^educ_ord)|(^age_ord)|(^gender)|(^region)",
  gof_omit    = "IC|Log.Lik|F$",
  add_rows    = fe_rows,
  notes       = tab_notes,
  escape      = TRUE
)

# 2) tabular-only (so you can scale it in LaTeX)
tex_tab <- modelsummary::msummary(
  models      = mods,
  output      = "latex_tabular",
  title       = NULL,
  stars       = TRUE,
  estimate    = "{estimate}{stars}",
  statistic   = "({std.error})",
  vcov        = vc,
  coef_rename = coef_map,
  coef_omit   = "(^educ_ord)|(^age_ord)|(^gender)|(^region)",
  gof_omit    = "IC|Log.Lik|F$",
  add_rows    = fe_rows,
  notes       = NULL,
  escape      = TRUE
)

# robust write: ensure UTF-8 and write bytes
dir.create("build", showWarnings = FALSE, recursive = TRUE)
writeLines(enc2utf8(as.character(tex_tab)),
           con = "build/table_models_tabular.tex",
           useBytes = TRUE)
## ----


################
#### reg table ologit
################

## ---- reg_table_ologit
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(MASS, modelsummary, broom, tibble)

# helper to replace Unicode dashes/minus with LaTeX-safe ASCII
sanitize_tex <- function(x) {
  x <- gsub("\u2013|\u2014", "--", x)  # en/em dash -> --
  x <- gsub("\u2212", "-", x)          # Unicode minus -> hyphen
  x
}

# six ordered logit models (techno & business as ordered factors) ---
t_w   <- MASS::polr(techno_ord   ~ gov_distance_w_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
                    data = dat.t, Hess = TRUE, method = "logistic")
t_u   <- MASS::polr(techno_ord   ~ gov_distance_u_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
                    data = dat.t, Hess = TRUE, method = "logistic")
t_min <- MASS::polr(techno_ord   ~ gov_distance_min_01 + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
                    data = dat.t, Hess = TRUE, method = "logistic")

#b_w_o   <- MASS::polr(business_ord ~ gov_distance_w_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t, Hess = TRUE, method = "logistic")

# b_u_o   <- MASS::polr(business_ord ~ gov_distance_u_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t, Hess = TRUE, method = "logistic")

# b_min_o <- MASS::polr(business_ord ~ gov_distance_min_01 + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region, data = dat.t, Hess = TRUE, method = "logistic")

mods_ologit <- list(
  `Tech (OL): Seat-wt` = t_w,
  `Tech (OL): Unwt`    = t_u,
  `Tech (OL): Closest` = t_min#,
  #`Biz (OL): Seat-wt`  = b_w_o,
  #`Biz (OL): Unwt`     = b_u_o,
  #`Biz (OL): Closest`  = b_min_o
)

coef_map_ologit <- c(
  "gov_distance_w_01"   = "Government distance (seat-weighted)",
  "gov_distance_u_01"   = "Government distance (unweighted)",
  "gov_distance_min_01" = "Government distance (closest-party)",
  "Q8_1"                = "Democratic satisfaction",
  "Q9_4"                = "Trust in politicians"
  # cutpoints / thresholds will be omitted via coef_omit
)

# rows indicating fixed effects presence
fe_rows_ologit <- tibble::tribble(
  ~term,          ~`Tech (OL): Seat-wt`, ~`Tech (OL): Unwt`, ~`Tech (OL): Closest`,
  "Education FE", "Yes",                "Yes",              "Yes",
  "Age FE",       "Yes",                "Yes",              "Yes",
  "Gender",       "Yes",                "Yes",              "Yes",
  "Region FE",    "Yes",                "Yes",              "Yes"
)

# covariances (model-based SEs for polr)
vc_ologit <- lapply(mods_ologit, vcov)

dir.create("build", showWarnings = FALSE, recursive = TRUE)

tab_title_ologit <- sanitize_tex("Ordered logit (proportional odds) estimates: Government Distance")
tab_notes_ologit <- sanitize_tex("Model-based standard errors in parentheses. Cutpoints omitted. Models include education, age, gender, and region fixed effects (omitted for brevity).")

# omit FE and the cutpoints (e.g., '1|2', '2|3', ...)
omit_pat <- "(^educ_ord)|(^age_ord)|(^gender)|(^region)|\\|"

# 1) full table environment
modelsummary::msummary(
  models      = mods_ologit,
  output      = "build/table_models_ologit_full.tex",
  title       = tab_title_ologit,
  stars       = TRUE,
  estimate    = "{estimate}{stars}",
  statistic   = "({std.error})",
  vcov        = vc_ologit,
  coef_rename = coef_map_ologit,
  coef_omit   = omit_pat,
  gof_omit    = "IC|Log.Lik|F$",
  add_rows    = fe_rows_ologit,
  notes       = tab_notes_ologit,
  escape      = TRUE
)

# 2) tabular-only (good for \resizebox or \scalebox)
tex_tab_ologit <- modelsummary::msummary(
  models      = mods_ologit,
  output      = "latex_tabular",
  title       = NULL,
  stars       = TRUE,
  estimate    = "{estimate}{stars}",
  statistic   = "({std.error})",
  vcov        = vc_ologit,
  coef_rename = coef_map_ologit,
  coef_omit   = omit_pat,
  gof_omit    = "IC|Log.Lik|F$",
  add_rows    = fe_rows_ologit,
  notes       = NULL,
  escape      = TRUE
)

# write tabular safely
writeLines(enc2utf8(as.character(tex_tab_ologit)),
           con = "build/table_models_ologit_tabular.tex",
           useBytes = TRUE)
## ----

################
#### Align var
################

## ---- align

# libs
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, scales, ggeffects,
  modelsummary, kableExtra, sandwich, lmtest, rlang
)

# Force classic LaTeX tabular output (no tabularray/tinytable)
options(modelsummary_factory_latex = "kableExtra")

# 0) Column names
v_trust_gov    <- "Q9_3"                 # trust in government (0–10)
v_trust_bof    <- "Q9_7"                 # trust in Bank of Finland (0–10)
v_govdist_w    <- "gov_distance_w_01"    # 0–1 seat-weighted distance
v_govdist_u    <- "gov_distance_u_01"    # 0–1 unweighted distance
v_govdist_min  <- "gov_distance_min_01"  # 0–1 closest-party distance
v_outcome_tech <- "techno5"              # 1–5 technocracy item

# defensive: ensure factors are factors (used in models)
dat.t <- dat.t %>%
  mutate(
    region = factor(region),
    gender = factor(gender)
  )

# 1) Build Align (Government minus BoF) and controls
dat.t <- dat.t %>%
  mutate(
    Align_raw = .data[[v_trust_gov]] - .data[[v_trust_bof]],  # Gov − Experts; >0 means gov trusted more
    Trust_sum = .data[[v_trust_bof]] + .data[[v_trust_gov]]
  ) %>%
  mutate(
    Align_01 = pmin(pmax(0.5 + Align_raw / 20, 0), 1)
  )

mu_align <- mean(dat.t$Align_01, na.rm = TRUE)
dat.t <- dat.t %>%
  mutate(
    Align_01_m05 = pmin(pmax(Align_01 - (mu_align - 0.5), 0), 1),
    Align_01_c   = Align_01_m05 - 0.5,                         # centered at 0
    Trust_sum_c  = Trust_sum - mean(Trust_sum, na.rm = TRUE)
  )

# 2) Main interaction model (used for the plot)
t_w_i <- lm(
  reformulate(
    c(paste0(v_govdist_w, "*Align_01_c"),
      "Trust_sum_c", "Q8_1", "Q9_4", "educ_ord", "age_ord", "gender", "region"),
    response = v_outcome_tech
  ),
  data = dat.t, na.action = na.exclude
)

# 3) Interaction plot (from the model above)
pred_dat <- ggeffects::ggpredict(
  t_w_i,
  terms = c(sprintf("%s [0:1 by=0.01]", v_govdist_w),  # x-axis: 0..1
            "Align_01_c [-0.25,0,0.25]")              # three moderator lines
)

# Build a standard ggplot object named `pint`
pint <- ggplot(pred_dat, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1) +
  labs(x = "Government distance (0 = close, 1 = far)",
       y = "Technocracy") +
  scale_color_manual(
    name   = "Gov-Expert trust gap",
    breaks = c("-0.25","0","0.25"),
    labels = c("Experts trusted", "Indifferent", "Government trusted"),
    values = scales::hue_pal()(3)
  ) +
  theme_light() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# 4) Additional OLS interaction specs (tech only; w / u / closest)
t_u_i <- lm(
  reformulate(
    c(paste0(v_govdist_u, "*Align_01_c"),
      "Trust_sum_c", "Q8_1", "Q9_4", "educ_ord", "age_ord", "gender", "region"),
    response = v_outcome_tech
  ),
  data = dat.t, na.action = na.exclude
)

t_min_i <- lm(
  reformulate(
    c(paste0(v_govdist_min, "*Align_01_c"),
      "Trust_sum_c", "Q8_1", "Q9_4", "educ_ord", "age_ord", "gender", "region"),
    response = v_outcome_tech
  ),
  data = dat.t, na.action = na.exclude
)

# 5) Table: tech-only OLS models with interaction (TABULAR-ONLY, classic LaTeX)
models_interact <- list(
  `Tech (OLS): Seat-wt` = t_w_i,   # plot uses this one
  `Tech (OLS): Unwt`    = t_u_i,
  `Tech (OLS): Closest` = t_min_i
)

# Robust SEs (HC1)
vcovs_hc1 <- lapply(models_interact, \(m) sandwich::vcovHC(m, type = "HC1"))

# ASCII-only labels (avoid Unicode inside LaTeX)
var_keys <- c(
  "Align_01_c",
  paste0(v_govdist_w,   ":Align_01_c"),
  paste0(v_govdist_u,   ":Align_01_c"),
  paste0(v_govdist_min, ":Align_01_c"),
  v_govdist_w,
  v_govdist_u,
  v_govdist_min,
  "Trust_sum_c",
  "Q8_1",
  "Q9_4"
)

var_vals <- c(
  "Gov--Expert trust gap (centered)",
  "Distance $\\times$ Gap (seat--wt)",
  "Distance $\\times$ Gap (unwt)",
  "Distance $\\times$ Gap (closest)",
  "Gov distance (seat--wt)",
  "Gov distance (unwt)",
  "Gov distance (closest)",
  "Overall trustfulness (centered)",
  "Democratic satisfaction",
  "Trust in politicians"
)

var_labels <- setNames(var_vals, var_keys)

dir.create("build", showWarnings = FALSE, recursive = TRUE)

tab_tex <- modelsummary::msummary(
  models      = models_interact,
  vcov        = vcovs_hc1,
  coef_rename = var_labels,
  coef_omit   = "(^educ_ord)|(^region)|(^age_ord)",
  gof_omit    = "IC|Log|AIC|BIC|F|Adj|Within|Pseudo|R2 Within|R2 Between|Std\\.Errors|Std\\. Errors",
  estimate    = "{estimate}{stars}",
  statistic   = "({std.error})",
  stars       = c('*'=.10, '**'=.05, '***'=.01),
  output      = "latex_tabular",
  escape      = FALSE
)

# Write a pure \begin{tabular}...\end{tabular} file (no tblr)
writeLines(enc2utf8(as.character(tab_tex)),
           con = "build/table_models_interact_tabular.tex",
           useBytes = TRUE)
## ----



################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
abstract.c = as.character(c("Research on partisan tolerance of democratic transgressions suggests that voters often trade democratic procedures for partisan ends, weakening electoral checks on incumbents. Much less is known about how citizens evaluate technocratic delegation, that is, shifting authority from elected politicians to unelected experts. We address this gap with a simple spatial voting model in which support for technocratic delegation depends on relative policy proximity and on whether expert institutions are perceived as aligned with the governing coalition. The model yields two implications: support for delegation declines with ideological distance from government when expert institutions are perceived as government-aligned, and this distance penalty attenuates when experts are perceived as more neutral. Empirically, we analyze a novel 2025 survey of Finnish adults, a hard case for partisan resistance to expertise. We proxy perceived government-expert alignment with a relative trust differential between the Cabinet and the Bank of Finland and interact it with an individual, seat-weighted distance-to-government measure based on party-closeness evaluations. Voters farther from the right-leaning Finnish cabinet are less supportive of technocratic delegation, especially when they trust the Cabinet more than the Bank of Finland. Results replicate across alternative distance measures."))
writeLines(abstract.c, fileConn)
close(fileConn)
## ----




## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----



