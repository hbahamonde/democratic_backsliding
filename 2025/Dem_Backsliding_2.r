## ---- loadings ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/2025/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

p_load(qualtRics)

# this should be run just ONCE per machine
# qualtrics_api_credentials(api_key = "UPU8fMH4I7Yr5Bz23zuZSPj7RUxRdbEPmB3ubbNo", 
#                          base_url = "fra1.qualtrics.com",
#                          install = TRUE,
#                          overwrite=TRUE)

surveys <- all_surveys() # to find out each survey id.

# download data
options(scipen=999999)
# download data
dat <- fetch_survey(surveyID = surveys$id[surveys$name=='Winners and Losers_survey'], verbose = FALSE)

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

dat.fi = dat

# Ensure all columns are character type before translation
dat.fi <- dat.fi %>%
  mutate(across(where(is.factor), as.character)) %>%  # Convert factors to characters
  mutate(across(where(is.numeric), as.character))    # Convert numbers to characters

# Correctly count the number of translatable elements (only character and non-NA values)
num_elements <- sum(sapply(dat.fi, function(col) sum(!is.na(col) & is.character(col))))

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Translating [:bar] :percent ETA: :eta",
  total = num_elements,
  clear = FALSE,
  width = 60
)

# Apply translation function with progress bar (only on character columns)
dat.fi <- dat.fi %>%
  mutate(across(where(is.character), ~ sapply(., function(x) translate_text(x, pb))))

# Ensure progress bar is marked as finished
if (!pb$finished) pb$terminate()

dat.t = dat.fi # duplicating
dat.t <- as.data.frame(dat.fi) # formatting
dat.t = dat.t[-c(1:12), ] # excluding pre-test

# deletes row names
rownames(dat.t) <- NULL


# save
save(dat.t, file="/Users/hectorbahamonde/research/democratic_backsliding/2025/dat_t.RData")


# HERE



# codebook
codebook <- c(dat.t[1,])



# cleaning
dat.t$Q5 = as.factor(dat.t$Q5)
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

# Question that shows the priming condition is "dat.t$DisplayOrder"

dat.t$DisplayOrder = as.factor(dat.t$DisplayOrder)
table(dat.t$DisplayOrder)

p_load(tidyr, ggplot2)

ggplot(dat.t, aes(x = Q13_1, fill = DisplayOrder)) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribution of Responses by Framing Condition",
       x = "Response",
       y = "Count",
       fill = "Framing Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


