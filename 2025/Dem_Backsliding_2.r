## ---- loadings ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/2025/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# Import Data
dat <- read.csv("/Users/hectorbahamonde/research/democratic_backsliding/2025/soft_launch/dat.csv")

#
install.packages("devtools")
devtools::install_github("zumbov2/deeplr")

# FI    Finnish
# EN    English

# Create an empty list to store translated columns
translated_columns <- list()

# Loop through each column of the dataset
for (col in names(dat)) {
  # Translate the entire column
  translated_column <- sapply(dat[[col]], function(text) {
    if (!is.na(text)) {
      # Translate only non-NA values
      deeplr::translate(text = text,
                        source_lang = "FI",
                        target_lang = "EN",
                        auth_key = "048783fe-31b8-4d6c-bfbc-0a5fd51f9c46")
    } else {
      # Return NA for missing values
      NA
    }
  })
  
  # Add the translated column to the list
  translated_columns[[col]] <- translated_column
}

# Combine the translated columns into a new data frame
dat.t <- as.data.frame(translated_columns)

# deletes row names
rownames(dat.t) <- NULL

# codebook
codebook <- c(dat.t[1,])

# deletes second row
dat.t <- dat.t[-c(1,2), ]

# introduces NA when cell is empty
p_load("tidyverse")
dat.t <- dat.t %>% mutate_all(na_if,"")

# cleaning
dat.t$Q5 = as.factor(dat.t$Q5)
plot(dat.t$Q5)
dat.t$Q8_1 = as.numeric(dat.t$Q8_1)

dat.t$Q10_1 = as.factor(dat.t$Q10_1)
dat.t$Q10_2 = as.factor(dat.t$Q10_2)
dat.t$Q10_3 = as.factor(dat.t$Q10_3)
dat.t$Q10_4 = as.factor(dat.t$Q10_4)
dat.t$Q10_5 = as.factor(dat.t$Q10_5)

# exclude survey preview
p_load("tidyverse")
dat.t = dat.t %>% filter(Status != "Survey preview")

# saves dat.t
save(dat.t, file="dat_t.RData")
write.csv(dat.t, "dat_t.csv")

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





