##########################################################################################
# Data Cleaning
##########################################################################################

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
dat.t$Q11 = as.factor(dat.t$Q11) # Interest in politics
dat.t$Q15 = as.factor(dat.t$Q15)
dat.t$Q3 = as.factor(dat.t$Q3) # age
dat.t$Q4 = as.factor(dat.t$Q4) # gender
dat.t$Q5 = as.factor(dat.t$Q5) # region
dat.t$Q6 = as.factor(dat.t$Q6) # educ


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
dat.t$Q13_6 <- factor(
  dat.t$Q13_6,
  levels = c(
    "I don't know",
    "Totally disagree",
    "Somewhat disagree",
    "Neither agree nor disagree",
    "Somewhat agree",
    "Totally agree"
  ),
  ordered = TRUE
)

#
dat.t$Q13_6.r <- ifelse(dat.t$Q13_6 == "Totally disagree", 1, 0)

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
