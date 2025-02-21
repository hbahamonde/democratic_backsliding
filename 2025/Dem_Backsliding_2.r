## DO NOT RUN
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/2025/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# p_load(qualtRics)

# this should be run just ONCE per machine
# qualtrics_api_credentials(api_key = "UPU8fMH4I7Yr5Bz23zuZSPj7RUxRdbEPmB3ubbNo", 
#                          base_url = "fra1.qualtrics.com",
#                          install = TRUE,
#                          overwrite=TRUE)

# surveys <- all_surveys() # to find out each survey id.

# download data
options(scipen=999999)
# download data
# dat <- fetch_survey(surveyID = surveys$id[surveys$name=='Winners and Losers_survey'], verbose = FALSE)

# Translating Data

# install.packages("devtools")
# devtools::install_github("zumbov2/deeplr")
# library(deeplr)

# Load required packages
p_load(deeplr,progress)

# Function to translate text with progress bar
translate_text <- function(text, pb) {
  if (!is.na(text) && is.character(text) && nzchar(text)) {  # Ensure it is a character
    tryCatch({
      result <- deeplr::translate(text = text,
                                  source_lang = "FI",
                                  target_lang = "EN",
                                  auth_key = "048783fe-31b8-4d6c-bfbc-0a5fd51f9c46")
      if (!pb$finished) pb$tick()  # Only tick if progress is not finished
      return(result)
    }, error = function(e) {
      warning(paste("Translation error:", e$message))
      if (!pb$finished) pb$tick()  # Prevent over-ticking
      return(text) # Return original text if translation fails
    })
  } else {
    if (!pb$finished) pb$tick()  # Update progress bar for NA or non-character values
    return(text)
  }
}

# dat.fi = dat

# Ensure all columns are character type before translation
# dat.fi <- dat.fi %>%
#  mutate(across(where(is.factor), as.character)) %>%  # Convert factors to characters
#  mutate(across(where(is.numeric), as.character))    # Convert numbers to characters

# Correctly count the number of translatable elements (only character and non-NA values)
# num_elements <- sum(sapply(dat.fi, function(col) sum(!is.na(col) & is.character(col))))

# Initialize progress bar
# pb <- progress_bar$new(
#  format = "  Translating [:bar] :percent ETA: :eta",
#  total = num_elements,
#  clear = FALSE,
#  width = 60
#)

# TRANSLATING CODE
# Apply translation function with progress bar (only on character columns)
# dat.fi <- dat.fi %>%  mutate(across(where(is.character), ~ sapply(., function(x) translate_text(x, pb))))

# Ensure progress bar is marked as finished
# if (!pb$finished) pb$terminate()

# dat.t = dat.fi # duplicating
# dat.t <- as.data.frame(dat.fi) # formatting
# dat.t = dat.t[-c(1:12), ] # excluding pre-test

# deletes row names
# rownames(dat.t) <- NULL


# save
# save(dat.t, file="/Users/hectorbahamonde/research/democratic_backsliding/2025/dat_t.RData")


##########################################################################################
# Data Analyses
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
dat.t$Q5 = as.factor(dat.t$Q5) # region
dat.t$Q8_1 = as.numeric(dat.t$Q8_1)
dat.t$Q10_1 = as.factor(dat.t$Q10_1)
dat.t$Q10_2 = as.factor(dat.t$Q10_2)
dat.t$Q10_3 = as.factor(dat.t$Q10_3)
dat.t$Q10_4 = as.factor(dat.t$Q10_4)
dat.t$Q10_5 = as.factor(dat.t$Q10_5)

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
                               .default = "Control",
                               .missing = "Control")  



# plots
p_load(tidyr, ggplot2)

ggplot(dat.t, aes(x = Q13_1, fill = Priming)) +
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
