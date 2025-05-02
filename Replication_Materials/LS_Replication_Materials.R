cat("\014")
rm(list=ls())

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# load data
load("dat_estonia.RData")
load("dat_chile.RData")

#####################################################
# Figure 1
#####################################################

p_load(ggplot2)

mm_Winner_Loser_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Protest + attr.Gender + attr.Age + attr.Pensions, 
                                             id = ~ respondent, 
                                             estimate = "mm", 
                                             by = ~winners.losers))

mm_Winner_Loser_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Protest + attr.Gender + attr.Age + attr.Pensions, 
                                               id = ~ respondent, 
                                               estimate = "mm", 
                                               by = ~winners.losers))


mm_Winner_Loser_Chile$Country <- "Chile"
mm_Winner_Loser_Estonia$Country <- "Estonia"

mm_Winner_Loser.d = rbind(mm_Winner_Loser_Chile, mm_Winner_Loser_Estonia)

# desired_order
desired_order <- c(
  "Younger than 35 years old",
  "Between 35-50 years old",
  "Over 50 years old",
  "The candidate OPPOSES increases in pensions for the elderly",
  "The candidate SUPPORTS increases in pensions for the elderly",
  "Man",
  "Woman",
  "The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government",
  "The candidate SUPPORTS anti-government protest\nthat will seek to de-destabilize the current government"
)

mm_Winner_Loser.d$level <- factor(mm_Winner_Loser.d$level, levels = desired_order)


p_load(ggplot2)
ggplot(mm_Winner_Loser.d,
                           aes(factor(level),
                               y=estimate,
                               ymin=lower,
                               ymax=upper,
                               color=factor(winners.losers))) + 
  geom_hline(yintercept = 0.5, colour = "black", lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size=0.25)+
  facet_wrap(~Country) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    axis.text.y = element_text(size=14), 
    axis.text.x = element_text(size=14), 
    axis.title.y = element_text(size=14), 
    axis.title.x = element_text(size=14), 
    legend.text=element_text(size=14), 
    legend.title=element_text(size=14),
    plot.title = element_text(size=14),
    strip.text.x = element_text(size = 14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "", y = "")

#####################################################
# Figure 2
#####################################################

# Load required packages
p_load(ggplot2, dplyr, ggpubr)

mm_Vote_Choice_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~ respondent, 
                                              estimate = "mm", 
                                              by = ~Vote.Choice))

mm_Vote_Choice_Estonia$Vote.Choice <- recode_factor(mm_Vote_Choice_Estonia$Vote.Choice,
                                                  `Estonian Centre Party` = "Centre Party",
                                                  `Estonian Conservative People\'s Party` = "Conservative People\'s Party",
                                                  `Estonian Reform Party` = "Reform Party")

mm_Vote_Choice_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Vote.Choice))



# Chile plot
mm_Vote_Choice_Chile = mm_Vote_Choice_Chile %>% filter(feature == "attr.Protest")

mm_Vote_Choice_Chile.p <- ggplot(mm_Vote_Choice_Chile,
                                 aes(x = level, y = estimate, ymin = lower, ymax = upper, color = factor(Vote.Choice))) +
  geom_hline(yintercept = 0.5, colour = "black", linetype = 2) +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.4) +
  coord_flip() +
  labs(x = "", y = "Support for Candidate", title = "Chile") +
  theme_bw(base_size = 14) +
  theme(
    aspect.ratio = 0.5,
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(title = "", nrow = 5, byrow = TRUE))


# Estonia plot
mm_Vote_Choice_Estonia = mm_Vote_Choice_Estonia %>% filter(feature == "attr.Protest")



mm_Vote_Choice_Estonia.p <- ggplot(mm_Vote_Choice_Estonia,
                                   aes(x = estimate, y = level, xmin = lower, xmax = upper, color = factor(Vote.Choice))) +
  geom_vline(xintercept = 0.5, colour = "black", linetype = 2) +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.25) +
  labs(x = "Support for Candidate", y = "", title = "Estonia") +
  theme_bw(base_size = 14) +
  theme(
    aspect.ratio = 0.4,
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 12)
  ) +
  coord_cartesian(xlim = c(0.38, 0.62)) +  # Makes x-axis tighter
  guides(color = guide_legend(title = "", nrow = 6, byrow = TRUE))



combined_plot <- ggarrange(
  mm_Vote_Choice_Chile.p,
  mm_Vote_Choice_Estonia.p,
  ncol = 1, nrow = 2, align = "v"
)

ggexport(combined_plot,
         filename = "mm_Vote_Choice_Combined_FIXED.pdf",
         width = 10, height = 9, res = 300
)




#####################################################
# Figure 2 Supplemental Materials
#####################################################

# changes ref category
conjoint.d.chile$attr.Protest <- relevel(conjoint.d.chile$attr.Protest, ref = "The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government")
conjoint.d.estonia$attr.Protest <- relevel(conjoint.d.estonia$attr.Protest, ref = "The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government")

amce_Winner_Loser_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Protest + attr.Gender + attr.Age + attr.Pensions, 
                                               id = ~ respondent, 
                                               estimate = "amce", 
                                               by = ~winners.losers))

amce_Winner_Loser_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Protest + attr.Gender + attr.Age + attr.Pensions, 
                                                 id = ~ respondent, 
                                                 estimate = "amce", 
                                                 by = ~winners.losers))


amce_Winner_Loser_Chile$Country <- "Chile"
amce_Winner_Loser_Estonia$Country <- "Estonia"

amce_Winner_Loser.d = rbind(amce_Winner_Loser_Chile, amce_Winner_Loser_Estonia)
# amce_Winner_Loser.p <- plot(amce_Winner_Loser.d, group = "winners.losers", vline = 0.5)
# amce_Winner_Loser.p %+% facet_wrap(~Country)

# desired_order
desired_order <- c(
  "Younger than 35 years old",
  "Between 35-50 years old",
  "Over 50 years old",
  "The candidate OPPOSES increases in pensions for the elderly",
  "The candidate SUPPORTS increases in pensions for the elderly",
  "Man",
  "Woman",
  "The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government",
  "The candidate SUPPORTS anti-government protest\nthat will seek to de-destabilize the current government"
)

amce_Winner_Loser.d$level <- factor(amce_Winner_Loser.d$level, levels = desired_order)


p_load(ggplot2)
ggplot(amce_Winner_Loser.d,
                             aes(factor(level),
                                 y=estimate,
                                 ymin=lower,
                                 ymax=upper,
                                 color=factor(winners.losers))) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size=0.25)+
  facet_wrap(~Country) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    axis.text.y = element_text(size=14), 
    axis.text.x = element_text(size=14), 
    axis.title.y = element_text(size=14), 
    axis.title.x = element_text(size=14), 
    legend.text=element_text(size=14), 
    legend.title=element_text(size=14),
    plot.title = element_text(size=14),
    strip.text.x = element_text(size = 14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "", y = "")


#####################################################
## Robustness Checks
# No carryover assumption
#####################################################

p_load(CRTConjoint)
form = formula("chosen ~ attr.Protest + attr.Gender + attr.Age + attr.Pensions")
J = 8 # Each respondent evaluated 8 tasks

# Chile
carryover_df.chile = conjoint.d.chile

# A
p_load(tidyverse)
carryover_df.chile.left <- carryover_df.chile %>%
  dplyr::filter(profile == "A") %>%
  dplyr::select(
    respondent,
    chosen,
    task,
    profile,
    attr.Protest,
    attr.Gender,
    attr.Age,
    attr.Pensions
  ) %>%
  rename_with(.cols = starts_with("attr."), 
              .fn = ~ paste0(., ".A"))

# B
p_load(tidyverse)
carryover_df.chile.right <- carryover_df.chile %>%
  dplyr::filter(profile == "B") %>%
  dplyr::select(
    respondent,
    chosen,
    task,
    profile,
    attr.Protest,
    attr.Gender,
    attr.Age,
    attr.Pensions
  ) %>%
  rename_with(.cols = starts_with("attr."), 
              .fn = ~ paste0(., ".B"))

carryover_df.chile.left <- carryover_df.chile.left %>% select(-profile)
carryover_df.chile.right <- carryover_df.chile.right %>% select(-profile)
carryover_df.chile = merge(carryover_df.chile.left,carryover_df.chile.right, by = c("respondent", "task"))

left = colnames(carryover_df.chile)[4:7]
right = colnames(carryover_df.chile)[9:12]
carryover_df.chile$chosen <- ifelse(carryover_df.chile$chosen.x == 1, 1, 0)
carryover_df.chile <- carryover_df.chile %>% select(-chosen.x, chosen.y)

set.seed(1234)
CRT_carryovereffect.chile = CRT_carryovereffect(formula = form, data = carryover_df.chile, task = "task", left = left, right = right, seed = 1234)
CRT_carryovereffect.chile$p_val # p > 0.05 = No significant carryover effect â†’ Assumption holds

# estonia

carryover_df.estonia = conjoint.d.estonia

# A
p_load(tidyverse)
carryover_df.estonia.left <- carryover_df.estonia %>%
  dplyr::filter(profile == "A") %>%
  dplyr::select(
    respondent,
    chosen,
    task,
    profile,
    attr.Protest,
    attr.Gender,
    attr.Age,
    attr.Pensions
  ) %>%
  rename_with(.cols = starts_with("attr."), 
              .fn = ~ paste0(., ".A"))

# B
p_load(tidyverse)
carryover_df.estonia.right <- carryover_df.estonia %>%
  dplyr::filter(profile == "B") %>%
  dplyr::select(
    respondent,
    chosen,
    task,
    profile,
    attr.Protest,
    attr.Gender,
    attr.Age,
    attr.Pensions
  ) %>%
  rename_with(.cols = starts_with("attr."), 
              .fn = ~ paste0(., ".B"))

carryover_df.estonia.left <- carryover_df.estonia.left %>% select(-profile)
carryover_df.estonia.right <- carryover_df.estonia.right %>% select(-profile)
carryover_df.estonia = merge(carryover_df.estonia.left,carryover_df.estonia.right, by = c("respondent", "task"))

left = colnames(carryover_df.estonia)[4:7]
right = colnames(carryover_df.estonia)[9:12]
carryover_df.estonia$chosen <- ifelse(carryover_df.estonia$chosen.x == 1, 1, 0)
carryover_df.estonia <- carryover_df.estonia %>% select(-chosen.x, chosen.y)

set.seed(1234)
CRT_carryovereffect.estonia = CRT_carryovereffect(formula = form, data = carryover_df.estonia, task = "task", left = left, right = right, seed = 1234)
CRT_carryovereffect.estonia$p_val # p > 0.05 = No significant carryover effect â†’ Assumption holds


#####################################################
## Robustness Checks
# Profile Order Effects (Left vs. Right Bias)
#####################################################


# Estonia
set.seed(1234)
CRT_profileordereffect.estonia = CRT_profileordereffect(formula = form, data = carryover_df.estonia, left = left, right = right, seed = 1234)
CRT_profileordereffect.estonia$p_val

# Chile
set.seed(1234)
CRT_profileordereffect.chile = CRT_profileordereffect(formula = form, data = carryover_df.chile, left = left, right = right, seed = 1234)
CRT_profileordereffect.chile$p_val

#####################################################
## Robustness Checks
# fatigue effects
#####################################################


# Estonia
set.seed(1234)
CRT_fatigueeffect.estonia = CRT_fatigueeffect(formula = form, data = carryover_df.estonia, respondent = "respondent", task = "task", left = left, right = right, seed = 1234)
CRT_fatigueeffect.estonia$p_val

# Chile
set.seed(1234)
CRT_fatigueeffect.chile = CRT_fatigueeffect(formula = form, data = carryover_df.chile, respondent = "respondent", task = "task", left = left, right = right, seed = 1234)
CRT_fatigueeffect.chile$p_val


