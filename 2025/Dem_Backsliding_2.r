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
  v[v == 12 | v < 1 | v > 11] <- NA_real_
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

## ----


################
#### reg_table
################

## ---- reg_table
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(stargazer, sandwich, lmtest,modelsummary,data.table)

# helper to replace Unicode dashes/minus with LaTeX-safe ASCII
sanitize_tex <- function(x) {
  x <- gsub("\u2013|\u2014", "--", x)  # en/em dash -> --
  x <- gsub("\u2212", "-", x)          # Unicode minus -> hyphen
  x
}

mods <- list(
  `Tech: Seat-wt` = m_w,
  `Tech: Unwt`    = m_u,
  `Tech: Closest` = m_min,
  `Biz: Seat-wt`  = b_w,
  `Biz: Unwt`     = b_u,
  `Biz: Closest`  = b_min
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
  ~term,            ~`Tech: Seat-wt`, ~`Tech: Unwt`, ~`Tech: Closest`, ~`Biz: Seat-wt`, ~`Biz: Unwt`, ~`Biz: Closest`,
  "Education FE",   "Yes",            "Yes",         "Yes",            "Yes",           "Yes",        "Yes",
  "Age FE",         "Yes",            "Yes",         "Yes",            "Yes",           "Yes",        "Yes",
  "Gender",         "Yes",            "Yes",         "Yes",            "Yes",           "Yes",        "Yes",
  "Region FE",      "Yes",            "Yes",         "Yes",            "Yes",           "Yes",        "Yes"
)

vc <- lapply(mods, function(m) sandwich::vcovHC(m, type = "HC1"))
dir.create("build", showWarnings = FALSE, recursive = TRUE)

tab_title <- sanitize_tex("OLS Estimates: Technocracy and Business vs. Government Distance")
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
# tabular-only for \resizebox or \scalebox
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
#### reg_table_ologit
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

b_w_o   <- MASS::polr(business_ord ~ gov_distance_w_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
                      data = dat.t, Hess = TRUE, method = "logistic")
b_u_o   <- MASS::polr(business_ord ~ gov_distance_u_01   + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
                      data = dat.t, Hess = TRUE, method = "logistic")
b_min_o <- MASS::polr(business_ord ~ gov_distance_min_01 + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
                      data = dat.t, Hess = TRUE, method = "logistic")

mods_ologit <- list(
  `Tech (OL): Seat-wt` = t_w,
  `Tech (OL): Unwt`    = t_u,
  `Tech (OL): Closest` = t_min,
  `Biz (OL): Seat-wt`  = b_w_o,
  `Biz (OL): Unwt`     = b_u_o,
  `Biz (OL): Closest`  = b_min_o
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
  ~term,            ~`Tech (OL): Seat-wt`, ~`Tech (OL): Unwt`, ~`Tech (OL): Closest`,
  ~`Biz (OL): Seat-wt`, ~`Biz (OL): Unwt`,  ~`Biz (OL): Closest`,
  "Education FE",   "Yes",                 "Yes",              "Yes",
  "Yes",                 "Yes",              "Yes",
  "Age FE",         "Yes",                 "Yes",              "Yes",
  "Yes",                 "Yes",              "Yes",
  "Gender",         "Yes",                 "Yes",              "Yes",
  "Yes",                 "Yes",              "Yes",
  "Region FE",      "Yes",                 "Yes",              "Yes",
  "Yes",                 "Yes",              "Yes"
)

# covariances (model-based SEs for polr)
vc_ologit <- lapply(mods_ologit, vcov)

dir.create("build", showWarnings = FALSE, recursive = TRUE)

tab_title_ologit <- sanitize_tex("Ordered logit (proportional odds) estimates: Technocracy and Business vs. Government Distance")
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
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
abstract.c = as.character(c("Polarization research shows that voters often trade democratic procedures for partisan ends, weakening electoral checks on incumbents. Much less is known about how citizens evaluate delegation-shifting authority from elected politicians to unelected experts. We address this gap with a simple formal model and new evidence from Finland. In the model, voters weigh a perceived performance gain from technocratic rule against ideological distance from government and the loss of electoral accountability. The model yields three implications: (i) support for delegation declines monotonically with distance from the governing coalition when expert institutions are perceived as government-aligned; (ii) this negative relationship attenuates when experts are viewed as neutral (or closer to the voter than the government) and steepens when alignment is high; and (iii) an irreversibility cost from ceding electoral control uniformly depresses support. Empirically, we analyze a 2025 survey of Finnish adults-a high-capacity, multiparty parliamentary democracy and thus a hard case where expert governance might otherwise be broadly acceptable. We develop an individual-level, seat-weighted measure of distance to the governing coalition based on party-closeness evaluations (renormalized for item nonresponse) and study its association with support for delegating power to experts. Linear and ordered-logit models indicate that voters farther from the right-leaning cabinet are less supportive of technocratic delegation. Substantively, this identifies when losers' consent does-and does not-extend to technocracy: even in a high-trust context, delegation is acceptable primarily when perceived performance gains outweigh ideological and accountability costs, and when expert bodies are not seen as aligned with the incumbents."))
writeLines(abstract.c, fileConn)
close(fileConn)
## ----




## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----



