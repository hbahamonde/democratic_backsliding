##########################################################################################
# Data Cleaning
##########################################################################################

## ---- loadings:d
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/2025/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# loading df
load(file="/Users/hectorbahamonde/research/democratic_backsliding/2025/dat_t.RData") 

# setting "<NA>" to NA
p_load(dplyr)
dat.t <- dat.t %>% mutate(across(where(is.character), ~ ifelse(. == "<NA>", NA, .)))

# cleaning
dat.t$Q8_1 = as.numeric(dat.t$Q8_1) # dem satisfaction
dat.t$Q10_1 = as.factor(dat.t$Q10_1) # dem might have problems, but...
dat.t$Q10_2 = as.factor(dat.t$Q10_2) # dem is not effective, better a strong leader
dat.t$Q10_3 = as.factor(dat.t$Q10_3) # importance of media
dat.t$Q10_4 = as.factor(dat.t$Q10_4) # pol participation 
dat.t$Q10_5 = as.factor(dat.t$Q10_5) # no pressure on the judiciary
#dat.t$Q11 = as.factor(dat.t$Q11) # Interest in politics
dat.t$Q15 = as.factor(dat.t$Q15)
#dat.t$Q3 = as.factor(dat.t$Q3) # age
#dat.t$Q4 = as.factor(dat.t$Q4) # gender
#dat.t$Q5 = as.factor(dat.t$Q5) # region
#dat.t$Q6 = as.factor(dat.t$Q6) # educ

# --- Q9 trust items: recode to numeric 1..11; 12 ("I don't know") -> NA ---
q9_cols <- paste0("Q9_", 1:7)

# warn if any are missing
missing <- setdiff(q9_cols, names(dat.t))
if (length(missing)) warning("Missing Q9 columns: ", paste(missing, collapse = ", "))

# recode
dat.t[q9_cols] <- lapply(dat.t[q9_cols], function(x) {
  x_chr <- as.character(x)
  # drop textual DKs if present
  x_chr[x_chr %in% c("I don't know", "I dont know", "<NA>", "", "NA")] <- NA
  v <- suppressWarnings(as.numeric(x_chr))
  # keep only 1..11; set 12 (DK) and any out-of-range to NA
  v[ v == 12 | v < 1 | v > 11 ] <- NA_real_
  v
})

# Optional: also provide 0–10 and 0–1 scaled versions
dat.t[paste0(q9_cols, "_010")] <- lapply(dat.t[q9_cols], function(v) ifelse(is.na(v), NA, v - 1))
dat.t[paste0(q9_cols, "_01")]  <- lapply(dat.t[q9_cols], function(v) ifelse(is.na(v), NA, (v - 1) / 10))

# Quick sanity checks
# sapply(dat.t[q9_cols], function(v) range(v, na.rm = TRUE))
# sapply(dat.t[q9_cols], function(v) table(v, useNA="ifany"))


# prepares q27 (government distance question)

p_load(dplyr)

# mapping
q27_old <- paste0("Q27_", 1:9)
q27_new <- c("q27_sdp","q27_ps","q27_kok","q27_kesk","q27_vihr","q27_vas","q27_rkp","q27_kd","q27_liike")

# 1) warn if any expected columns are missing
missing <- setdiff(q27_old, names(dat.t))
if (length(missing)) warning("Missing Q27 columns: ", paste(missing, collapse = ", "))

# 2) coerce to numeric
dat.t[q27_old] <- lapply(dat.t[q27_old], function(x) suppressWarnings(as.numeric(x)))

# 3) rename to party names
names(dat.t)[match(q27_old, names(dat.t))] <- q27_new

# 4) DK (=12) → NA; guard range 0–10 just in case
dat.t <- dat.t %>%
  mutate(across(all_of(q27_new),
                ~ ifelse(. == 12 | . < 0 | . > 10, NA_real_, .)))



# Generate Weighted Distance Variable
p_load(dplyr)

# Seat shares of 2023 Orpo govt (edit if your wave differs)
gov_cols <- c("q27_kok","q27_ps","q27_rkp","q27_kd")

# Seat counts for the Orpo cabinet (Dec 2024–Jan 2025)
w_named <- c(kok = 48, ps = 46, rkp = 9, kd = 5)

# Helper: weighted mean with rowwise renormalization over non-missing items
renorm_wmean <- function(x, w) {
  ok <- !is.na(x)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# 1) Clean values (character -> numeric; DK=12 or out-of-range -> NA)
dat.t <- dat.t %>%
  mutate(across(all_of(gov_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(across(all_of(gov_cols), ~ ifelse(. == 12 | . < 0 | . > 10, NA_real_, .)))

# 2) Seat-weighted (renormalized) coalition closeness & distance
dat.t <- dat.t %>%
  rowwise() %>%
  mutate(
    gov_closeness_w = {
      x <- c_across(all_of(gov_cols))
      # align weights to column order
      w <- w_named[sub("^q27_", "", gov_cols)]
      renorm_wmean(x, w)
    },
    gov_distance_w = 10 - gov_closeness_w
  ) %>%
  ungroup()

# 3) Unweighted versions + diagnostics (robust base-R) ---

# sanity: check the columns exist and are numeric
stopifnot(all(gov_cols %in% names(dat.t)))
stopifnot(all(sapply(dat.t[gov_cols], is.numeric)))

# make a plain matrix of the gov-party ratings
Xg <- as.matrix(dat.t[, gov_cols, drop = FALSE])

# count answered gov-party items per row
answered <- rowSums(!is.na(Xg))

# unweighted closeness: mean over answered items
closeness_u <- rowSums(Xg, na.rm = TRUE) / pmax(answered, 1L)  # avoid /0
closeness_u[answered == 0] <- NA_real_                          # all-missing -> NA

# attach to dat.t
dat.t$gov_closeness_u <- closeness_u
dat.t$gov_distance_u  <- 10 - dat.t$gov_closeness_u
dat.t$gov_known_share <- answered / ncol(Xg)                     # 0..1 share answered

# 4) “Closest-government-party” distance (safe when all-missing)
max_close <- do.call(
  pmax,
  c(dplyr::select(dat.t, tidyselect::all_of(gov_cols)), list(na.rm = TRUE))
  )
all_missing <- rowSums(!is.na(dplyr::select(dat.t, tidyselect::all_of(gov_cols)))) == 0
dat.t$gov_distance_min <- ifelse(all_missing, NA_real_, 10 - max_close)


# gov_closeness_w — Seat-weighted coalition closeness: respondent’s average closeness to the governing parties, weighting each party by its seat share
# gov_distance_w — Seat-weighted coalition distance. A flipped version so that larger = farther from government.
# gov_closeness_u — Unweighted coalition closeness. Simple (unweighted) mean of the respondent’s closeness to the gov parties.
# gov_distance_u — Unweighted coalition distance. The flipped version of the unweighted closeness.
# gov_distance_min — “Closest-party” distance. Distance to the single government party the respondent likes the most.

# order Q13_1
dat.t$Q13_1 <- factor(
  dat.t$Q13_1,
  levels = c("I don't know",
    "Totally disagree",
    "Somewhat disagree",
    "Neither agree nor disagree",
    "Somewhat agree",
    "Totally agree"
  ),
  ordered = TRUE)

# Q13_1.r
dat.t$Q13_1.r <- ifelse(dat.t$Q13_1 == "Totally disagree", 1, 0)

#
p_load(dplyr, forcats, sandwich, lmtest)

## 1) Recode technocracy item (Q13_6): drop "I don't know" and make 1..5 ---
dat.t <- dat.t %>%
  mutate(
    q13_6_chr = as.character(Q13_6),
    q13_6_chr = na_if(q13_6_chr, "I don't know"),
    techno_ord = factor(
      q13_6_chr,
      levels = c("Totally disagree","Somewhat disagree",
                 "Neither agree nor disagree","Somewhat agree","Totally agree"),
      ordered = TRUE
    ),
    techno5 = as.numeric(techno_ord)   # 1..5 (higher = more technocratic)
  )
# quick check
# table(dat.t$techno5, useNA="ifany")



## 2) Controls (light-touch recodes; drop DK/Other where needed) ---
dat.t <- dat.t %>%
  mutate(
    # Education (ordered; drop "Other/Don't know")
    educ_chr = as.character(Q6),
    educ_chr = na_if(educ_chr, "Other/Don't know"),
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
    
    # Age (ordered categories from the survey)
    age_ord = factor(
      as.character(Q3),
      levels = c("18-24","25-34","35-44","45-54","55+"),
      ordered = TRUE
    ),
    
    # Gender (drop Other / prefer not to say)
    gender = fct_drop(fct_recode(factor(as.character(Q4)),
                                 Female = "Female",
                                 Male    = "Male",
                                 NULL    = "Other/do not want to say")),
    
    # Political interest (optional but useful; drop DK)
    polint_chr = na_if(as.character(Q11), "I don't know"),
    polint_ord = factor(
      polint_chr,
      levels = c("Not interested at all","Only slightly interested",
                 "Somewhat interested","Very interested"),
      ordered = TRUE
    ),
    
    # Region (as factor)
    region = factor(as.character(Q5))
  )

## 3) Main predictor: scale gov_distance_w to 0–1 for interpretability ---
dat.t <- dat.t %>%
  mutate(
    gov_closeness_w_01 = gov_closeness_w / 10,   # 0..1 (higher = closer to govt)
    gov_distance_w_01  = 1 - gov_closeness_w_01  # exact complement of closeness
  )


## 4) OLS: technocracy (1..5) on distance + controls ---
#model_dat <- dat.t %>%
#  filter(!is.na(techno5), !is.na(gov_distance_w_01))



# ols
m_ols <- lm(
  techno5 ~ gov_distance_w_01 + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
  ,
  data = dat.t
)
summary(m_ols)


# Packages
p_load(ggplot2)
       

mf <- model.frame(m_ols)           # data actually used in the fit (complete cases)
V  <- vcov(m_ols)
xseq <- seq(0, 1, by = 0.01)

ap <- lapply(xseq, function(x) {
  nd <- mf
  nd$gov_distance_w_01 <- x
  X  <- model.matrix(formula(m_ols), data = nd)
  xbar <- colMeans(X)                               # average design vector
  fit  <- drop(xbar %*% coef(m_ols))                # average prediction
  se   <- sqrt(drop(t(xbar) %*% V %*% xbar))        # delta-method SE
  data.frame(
    gov_distance_w_01 = x,
    estimate = fit,
    conf.low = fit - 1.96 * se,
    conf.high = fit + 1.96 * se
  )
})
ap <- do.call(rbind, ap)

## ----

plot.p = ggplot(ap, aes(gov_distance_w_01, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.20) +
  geom_line(linewidth = 1) +
  labs(
    x = "Government distance (0 = very close, 1 = far)",
    y = "Predicted technocracy (1–5)",
    title = "Average predicted\nTechnocracy vs. Government distance",
    subtitle = "Averaged over the empirical distribution of controls"
  ) +
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal(base_size = 12)




# o logit
p_load(dplyr, MASS, sandwich, lmtest)
m_ologit <- MASS::polr(
  techno_ord ~ gov_distance_w_01 + Q8_1 + Q9_4 + educ_ord + age_ord + gender + region,
  data = dat.t, method = "logistic", Hess = TRUE
)

summary(m_ologit)

# Predicted probabilities for each level of techno_ord
# across gov_distance_w_01 in [0,1], averaged over the sample
# (no marginaleffects; no predict.polr)

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(ggplot2); library(dplyr); library(tidyr)

# 1) Extract pieces from the fitted model
mf         <- model.frame(m_ologit)                      # data used in the fit (levels/contrasts correct)
trmX       <- delete.response(terms(m_ologit))           # RHS terms
beta       <- coef(m_ologit)                             # slope coefficients
beta_names <- names(beta)
zeta       <- m_ologit$zeta                              # cutpoints
K          <- length(zeta) + 1                           # number of outcome categories
lev_y      <- levels(mf$techno_ord)

# 2) Helper: average predicted probs at a given x
avg_probs_at_x <- function(x){
  nd <- mf
  nd$gov_distance_w_01 <- x
  
  # Build X with the *same* terms and contrasts; align columns to beta names
  X <- model.matrix(trmX, data = nd)
  # keep exactly the columns used in the model, in the right order
  X <- X[, beta_names, drop = FALSE]
  
  eta <- as.numeric(X %*% beta)  # linear predictor for every row
  
  # Cumulative probabilities for categories 1..K-1: P(Y<=k) = logit^{-1}(zeta_k - eta)
  cum <- plogis(matrix(zeta, nrow = length(eta), ncol = length(zeta), byrow = TRUE) - eta)
  
  # Convert to category probabilities 1..K
  probs <- matrix(NA_real_, nrow = length(eta), ncol = K)
  probs[, 1] <- cum[, 1]
  if (K > 2) for (k in 2:(K - 1)) probs[, k] <- cum[, k] - cum[, k - 1]
  probs[, K] <- 1 - cum[, K - 1]
  
  pm <- colMeans(probs)  # average over the sample
  tibble(gov_distance_w_01 = x,
         outcome = factor(lev_y, levels = lev_y, ordered = TRUE),
         prob = as.numeric(pm))
}

# 3) Evaluate on a grid and bind
xseq <- seq(0, 1, by = 0.01)
pp   <- bind_rows(lapply(xseq, avg_probs_at_x))

# 4) Plot
ggplot(pp, aes(x = gov_distance_w_01, y = prob, color = outcome)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Distance from government (0–1)",
    y = "Predicted probability",
    color = "Technocracy level",
    title = "Predicted probabilities vs. distance from government (ordered logit)",
    subtitle = "Averaged over the empirical distribution of controls"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 12)






## ---- abstract ----
fileConn <- file ("abstract.txt")
abstract.c = as.character(c("Polarization research shows that voters often trade democratic procedures for partisan ends, weakening electoral checks on incumbents. Yet we know far less about how citizens evaluate \emph{delegation}—shifting authority from elected politicians to unelected experts. This paper bridges that gap by theorizing that delegation is not ideologically neutral: its appeal depends on the perceived ideological valence of “expert” institutions. We argue that electoral losers will oppose delegation when experts are seen as aligned with the incumbent coalition, and may favor it when experts are perceived as neutral or aligned with the opposition. This reframes canonical trade-offs as a choice between delegation and electoral accountability, and refines scope conditions in the polarization literature. Empirically, we leverage a novel, original survey dataset from Finland---an expert-trusting, high-capacity democracy---constitutes a hard case that might make technocratic delegation broadly acceptable; finding the opposite pattern here sharpens our scope conditions. We develop an individual-level, seat-weighted measure of distance to the governing coalition based on respondents’ party closeness evaluations, renormalized for item nonresponse, and examine its association with support for delegating power to experts. Contrary to “losers favor constraints” expectations, greater distance from the right-leaning cabinet is associated with \emph{lower} support for technocratic delegation. We interpret this as evidence that losers prefer electoral accountability when expertise is perceived as ideologically right-coded. Substantively, the findings identify when losers’ consent does---and does not---extend to technocracy; methodologically, we introduce a transparent distance-to-government metric that might travel to other coalition systems."))
writeLines(abstract.c, fileConn)
close(fileConn)
## ----


## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----





### Democracy Support and Priming Variable

# Question that shows the priming condition is "dat.t$DisplayOrder"

#table(dat.t$DisplayOrder)
# 25: loser
# 17: winner
# NA: control

# 
dat.t$Priming <- factor(ifelse(is.na(dat.t$DisplayOrder), "Control", dat.t$DisplayOrder))
p_load("tidyverse")
dat.t$Priming <- recode_factor(dat.t$DisplayOrder, 
                               `Q25` = "Loser", 
                               `Q17` = "Winner",
                               #.default = "Control",
                               .missing = "Control")
####################
# Polarization
####################

# Vector of variables corresponding to Q27_1 to Q27_9
q27_vars <- paste0("Q27_", 1:9)

# Define mapping from character to numeric
closeness_map <- function(x) {
  x <- as.character(x)
  x[x == "Not at all close to 0"] <- "0"
  x[x == "Very close to 10"] <- "10"
  x[x == "I don't know"] <- NA
  as.numeric(x)
}

# Apply mapping to convert all Q27_* variables to numeric
for (v in q27_vars) {
  dat.t[[v]] <- closeness_map(dat.t[[v]])
}

# Define coalition party positions (based on the variable ordering)
# Assuming:
# Q27_1 = SDP
# Q27_2 = PS (True Finns)
# Q27_3 = KOK (National Coalition)
# Q27_4 = KESK
# Q27_5 = VIHR
# Q27_6 = VAS
# Q27_7 = RKP
# Q27_8 = KD
# Q27_9 = Movement Now

# coalition_parties as positions (KOK = Q27_3, PS = Q27_2, RKP = Q27_7, KD = Q27_8)
coalition_indices <- c(2, 3, 7, 8)
coalition_vars <- q27_vars[coalition_indices]

# Compute average closeness to coalition parties
dat.t$avg_closeness_coalition <- rowMeans(dat.t[, coalition_vars], na.rm = TRUE)

# Compute ideological distance from government
dat.t$govt_distance <- 10 - dat.t$avg_closeness_coalition

# Optional: label NA if all coalition ratings were NA
dat.t$govt_distance[is.nan(dat.t$govt_distance)] <- NA


####################
# Dependent Variable
####################

# Recode Q13_6 to numeric Likert-style scale
dat.t$Q13_6_num <- recode(dat.t$Q13_6,
                          "Totally disagree" = 1,
                          "Somewhat disagree" = 2,
                          "Neither agree nor disagree" = 3,
                          "Somewhat agree" = 4,
                          "Totally agree" = 5,
                          "I don't know" = NA_real_)

####################
# Models
####################

p_load(jtools)
# dem satisfaction
fit = lm(Q13_6_num ~ govt_distance*Q8_1 + Q3 + Q4 + Q5 + Q6, data = dat.t)
summary(fit)
effect_plot(fit,
            pred = "govt_distance",
            modx = "Q8_1",
            data = dat.t,
            interval = TRUE)

dat.t$Q8_1 = as.numeric(dat.t$Q8_1) # dem satisfaction
dat.t$Q10_1 = as.factor(dat.t$Q10_1) # dem might have problems, but...
dat.t$Q10_2 = as.factor(dat.t$Q10_2) # dem is not effective, better a strong leader
dat.t$Q10_3 = as.factor(dat.t$Q10_3) # importance of media
dat.t$Q10_4 = as.factor(dat.t$Q10_4) # pol participation 
dat.t$Q10_5 = as.factor(dat.t$Q10_5) # no pressure on the judiciary
dat.t$Q11 = as.factor(dat.t$Q11) # Interest in politics
dat.t$Q15 = as.factor(dat.t$Q15)
dat.t$Q3 = as.factor(dat.t$Q3) # age
dat.t$Q4 = as.factor(dat.t$Q4) # gender
dat.t$Q5 = as.factor(dat.t$Q5) # region
dat.t$Q6 = as.factor(dat.t$Q6) # educ




####################
# Coalition Govt Var
####################

# Define coalition parties based on the current Finnish government
coalition_parties <- c("National Coalition Party (NCP)", 
                       "True Finns (PS)", 
                       "Swedish People's Party of Finland (RKP)", 
                       "Christian Democrats of Finland (KD)")

# Create the new variable
dat.t$CoalitionPartyVote <- ifelse(dat.t$Q15 %in% coalition_parties, "Coalition",
                                   ifelse(dat.t$Q15 %in% c("I did not vote", "I don't know"), "Other", "Non-Coalition"))

# Convert to factor for ordered categories
dat.t$CoalitionPartyVote <- factor(dat.t$CoalitionPartyVote, levels = c("Coalition", "Non-Coalition", "Other"))

##




####################
# Talking Points (Inga)
####################
table(dat.t$CoalitionPartyVote, dat.t$Q15)
table(dat.t$Priming, dat.t$CoalitionPartyVote)


as.data.frame(dat.t %>%
                dplyr::arrange(desc(Q15)) %>%
                dplyr::filter(Priming == "Control" & CoalitionPartyVote == "Coalition") %>% 
                dplyr::select(Q15, Priming, CoalitionPartyVote, DisplayOrder))

as.data.frame(dat.t %>%
                dplyr::arrange(desc(Q15)) %>%
                dplyr::filter(Priming == "Control" & CoalitionPartyVote == "Non-Coalition") %>% 
                dplyr::select(Q15, Priming, CoalitionPartyVote, DisplayOrder))

# concern: self-selection. the effects can be because of their party pref. // loser/winning condition, and 
# not the priming.
####################

# plots
p_load(tidyr, ggplot2)

ggplot(dat.t, aes(x = Q13_1, fill = Q15)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Party",
       x = "The Gov't must be able to censor media that are unduly critical of the Gov't",
       y = "Count",
       fill = "Party Choice") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dat.t, aes(x = Q13_1, fill = Priming)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Framing Condition",
       x = "The Gov't must be able to censor media that are unduly critical of the Gov't",
       y = "Count",
       fill = "Framing Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dat.t, aes(x = Q13_1.r, fill = Priming)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Framing Condition",
       x = "The Gov't must be able to censor media that are unduly critical of the Gov't",
       y = "Count",
       fill = "Framing Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dat.t, aes(x = Q13_3, fill = Priming)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Framing Condition",
       x = "The government should be able to ignore court decisions that it considers politically biased",
       y = "Count",
       fill = "Framing Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dat.t, aes(x = Q13_5, fill = Priming)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Framing Condition",
       x = "Government should be able to stretch the limits of legislation to solve pressing social problems",
       y = "Count",
       fill = "Framing Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dat.t, aes(x = Q13_6, fill = Priming)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Framing Condition",
       x = "Our country would work better if political decisions were left to independent, non-elected experts instead of politicians",
       y = "Count",
       fill = "Framing Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dat.t, aes(x = Q13_6, fill = Priming)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Framing Condition",
       x = "Our country would work better if political decisions were left to independent, non-elected experts instead of politicians",
       y = "Count",
       fill = "Framing Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##



##############################
# Analyses
##############################
p_load(jtools)
# dem satisfaction
fit = lm(Q8_1 ~ Priming, data = dat.t)
summary(fit)
effect_plot(fit, pred = Priming, interval = TRUE)



##############################
# Data / quota checks
##############################

# Don't knows ISSUE
dont_know_variations <- c("Other/Don't know", "I don't know")

# Count the occurrences of "don't know" responses per respondent
p_load("tidyverse")
dont_know_count <- dat.t %>%
  dplyr::select(-ResponseId) %>%  # Exclude the ResponseId column
  dplyr::mutate_all(~ . %in% dont_know_variations) %>%  # Check for "don't know" variations
  rowSums()  # Count "don't know" responses per respondent

# Combine with ResponseId for reference
respondent_dont_know <- data.frame(
  ResponseId = dat.t$ResponseId,
  DontKnowCount = dont_know_count
)

# Identify the respondents with the most "don't know" responses
most_dont_know <- respondent_dont_know %>%
  dplyr::arrange(desc(DontKnowCount)) %>%
  dplyr::filter(DontKnowCount > 0)  # Optional: Exclude respondents with no "don't know" answers


head(most_dont_know)

### Map of respondents

dat.t <- dat.t %>%
  mutate(
    LocationLatitude = as.numeric(LocationLatitude),
    LocationLongitude = as.numeric(LocationLongitude)
  )


p_load(ggplot2, sf, rnaturalearth, rnaturalearthdata)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define Finland's bounding box
finland_xlim <- c(19, 32)  # Approximate longitude range of Finland
finland_ylim <- c(59.5, 70)  # Approximate latitude range of Finland

# Create the map
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray50") +
  geom_point(data = dat.t, aes(x = LocationLongitude, y = LocationLatitude),
             color = "red", alpha = 0.6, size = 2) +
  coord_sf(xlim = finland_xlim, ylim = finland_ylim) +
  labs(title = "Survey Respondent Locations",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Quota plots

## REGION
fi.eurostat.region <- data.frame(
  Region = c(
    "Helsinki and Uusimaa",
    "Northern and Eastern Finland",
    "Southern Finland", 
    "Western Finland"),
  Population_Percentage = c(29.9,24.0,21.0,25.1)
)

# Survey data
fi.survey.data.region <- dat.t %>%
  count(Q5) %>%
  mutate(Survey_Percentage = (n / sum(n)) * 100)

# Combine the data
fi.combined.data.region <- fi.eurostat.region %>%
  left_join(fi.survey.data.region, by = c("Region" = "Q5")) %>%
  select(Region, Population_Percentage, Survey_Percentage)

# Correct pivot_longer with updated column names
fi.plot.data.region <- fi.combined.data.region %>%
  pivot_longer(cols = c("Population_Percentage", "Survey_Percentage"),
               names_to = "Source",
               values_to = "Percentage") %>%
  mutate(Source = recode(Source, 
                         "Population_Percentage" = "Statistics Finland", 
                         "Survey_Percentage" = "Survey"))

# Plot the data
ggplot(fi.plot.data.region, aes(x = reorder(Region, -Percentage), y = Percentage, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Survey Quotas with Statistics Finland Data",
       x = "Region",
       y = "Percentage",
       fill = "Data Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Gender

p_load(dplyr, ggplot2)

# 
fi.survey.gender <- dat.t %>%
  group_by(Q4) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# 
fi.eurostat.gender <- data.frame(
  Gender = c("Male", "Female"),
  Percentage = c(50.0, 49.4),
  Source = "Statistics Finland"
)


# 
fi.survey.gender <- fi.survey.gender %>%
  rename(Gender = Q4) %>%
  mutate(Source = "Survey") %>%
  select(Gender, Percentage, Source)

combined_gender_data.fi <- bind_rows(fi.eurostat.gender, fi.survey.gender)


# Create comparison plot
ggplot(combined_gender_data.fi, aes(x = Gender, y = Percentage, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Survey Gender with Statistics Finland Data",
       x = "Gender",
       y = "Percentage",
       fill = "Data Source") +
  theme_minimal() 

# Age

## Age
p_load(dplyr, ggplot2, tidyr)

# Statistics Finland age distribution
fi.eurostat.age <- data.frame(
  Age_Group = c("18-29 years", "30-39 years", "40-49 years", 
                "50-59 years", "60-69 years", "65-99 years"),
  Population_Percentage = c(9.00, 15.00, 15.00, 17.00, 18.00, 26.00)
)

# Group survey responses to match Statistics Finland's age categories
fi.survey.data.age <- dat.t %>%
  mutate(Age_Group = case_when(
    Q3 %in% c("18-24", "25-34") ~ "18-29 years",  # Merge 18-24 and 25-34 into 18-29
    Q3 == "35-44" ~ "30-39 years",  # Adjusting based on available categories
    Q3 == "45-54" ~ "40-49 years",
    Q3 == "55+" ~ "50-59 years",  # Assuming 55+ mostly falls into 50-59
    TRUE ~ NA_character_  # Catch-all for unexpected values
  )) %>%
  filter(!is.na(Age_Group)) %>%
  count(Age_Group) %>%
  mutate(Survey_Percentage = (n / sum(n)) * 100)

# Merge survey and Statistics Finland data
fi.combined.data.age <- fi.eurostat.age %>%
  left_join(fi.survey.data.age, by = "Age_Group") %>%
  select(Age_Group, Population_Percentage, Survey_Percentage)

# Convert to long format for plotting
fi.plot.data.age <- fi.combined.data.age %>%
  pivot_longer(cols = c("Population_Percentage", "Survey_Percentage"),
               names_to = "Source",
               values_to = "Percentage") %>%
  mutate(Source = recode(Source, 
                         "Population_Percentage" = "Statistics Finland", 
                         "Survey_Percentage" = "Survey"))

# Reorder Age_Group for proper plotting order
fi.plot.data.age <- fi.plot.data.age %>%
  mutate(Age_Group = factor(Age_Group, 
                            levels = c("18-29 years", 
                                       "30-39 years", 
                                       "40-49 years", 
                                       "50-59 years",
                                       "60-69 years",
                                       "65-99 years")))

# Generate comparison plot
ggplot(fi.plot.data.age, aes(x = Age_Group, y = Percentage, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Survey Age Distribution with Statistics Finland Data",
       x = "Age Group",
       y = "Percentage",
       fill = "Data Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
