cat("\014")
rm(list=ls())

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# load data
load("dat_estonia.RData")
load("dat_chile.RData")

# load("/Users/hectorbahamonde/research/democratic_backsliding/Replication_Materials/dat_chile.RData")
# load("/Users/hectorbahamonde/research/democratic_backsliding/Replication_Materials/dat_estonia.RData")

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
