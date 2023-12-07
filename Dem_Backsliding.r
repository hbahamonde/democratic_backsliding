## ---- loadings ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# Import Data
# dat <- read.csv("/Users/hectorbahamonde/research/democratic_backsliding/data/Qualtrics/Chile_Soft_Launch.csv")

# 2
# dat <- read.csv("/Users/hectorbahamonde/research/democratic_backsliding/data/Qualtrics/Chile_Soft_Launch2.csv")

# 3
dat <- read.csv("/Users/hectorbahamonde/research/democratic_backsliding/data/Qualtrics/chile_data.csv")


# delete first two/three rows
# dat = dat[-c(1, 2, 3), ]  # 1
dat = dat[-c(1, 2), ]  # 2 and 3

# Chile data
dat.chile = dat
chile.sample.size = as.numeric(nrow(dat.chile))

# convert all character columns to factor
dat[sapply(dat, is.character)] <- lapply(dat[sapply(dat, is.character)], 
                                         as.factor)

########################################################
# Re-coding // Descriptive
########################################################

p_load("dplyr")

##
dat$Q4  <- recode_factor(as.factor(dat$Q4), `Hombre` = "Man", `Mujer` = "Woman") # gender
#lattice::histogram(dat$Q4, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

##
dat$Q10_1  <- recode_factor(as.factor(dat$Q10_1),  # Democracy might have problems but it's better...
                            "Completamente de acuerdo" = "Agree completely", 
                            "Un poco de acuerdo" = "Agree to some extent",
                            "Un poco en desacuerdo" = "Somewhat disagree",
                            "Completamente en desacuerdo" = "Completely disagree"
                            )
# lattice::histogram(dat$Q10_1, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

##
dat$Q10_2  <- recode_factor(as.factor(dat$Q10_2),  # Democracy is not an effective form of government...better a strong leader
                            "Completamente de acuerdo" = "Agree completely", 
                            "Un poco de acuerdo" = "Agree to some extent",
                            "Un poco en desacuerdo" = "Somewhat disagree",
                            "Completamente en desacuerdo" = "Completely disagree"
                            )
# lattice::histogram(dat$Q10_2, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

##
dat$Q10_3  <- recode_factor(as.factor(dat$Q10_3),  # right to protest
                            "Completamente de acuerdo" = "Agree completely", 
                            "Un poco de acuerdo" = "Agree to some extent",
                            "Un poco en desacuerdo" = "Somewhat disagree",
                            "Completamente en desacuerdo" = "Completely disagree"
                            )
# lattice::histogram(dat$Q10_3, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

##
dat$Q10_4  <- recode_factor(as.factor(dat$Q10_4),  # free press
                            "Completamente de acuerdo" = "Agree completely", 
                            "Un poco de acuerdo" = "Agree to some extent",
                            "Un poco en desacuerdo" = "Somewhat disagree",
                            "Completamente en desacuerdo" = "Completely disagree"
                            )
# lattice::histogram(dat$Q10_4, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

#
dat$Q12_1 = as.numeric(dat$Q12_1) # Governments tax the rich and subsidize the poor
# lattice::histogram(dat$Q12_1, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 
dat$Q12_1_highlow = ifelse(dat$Q12_1 >= median(dat$Q12_1), 1, 0)
dat$Q12_1_highlow = as.factor(dat$Q12_1_highlow)

dat$Q12_1_highlow = recode_factor(as.factor(dat$Q12_1_highlow),  # Governments tax the rich and subsidize the poor
              "0" = "Not an essential characteristic", 
              "1" = "An essential characteristic"
              )


#
dat$Q8_1 = as.numeric(dat$Q8_1) # satisfied w dem
# lattice::histogram(dat$Q8_1, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

# satisfied w dem high/low
dat$Q8_1_highlow = ifelse(dat$Q8_1 >= median(dat$Q8_1), 1, 0)
dat$Q8_1_highlow = as.factor(dat$Q8_1_highlow)
dat$Q8_1_highlow = recode_factor(dat$Q8_1_highlow, "0"="Low Satisfaction", "1" = "High Satisfaction")

#
dat$Q12_2 = as.numeric(dat$Q12_2) # Religious authorities ultimately interpret the laws
# lattice::histogram(dat$Q12_2, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

#
dat$Q12_3 = as.numeric(dat$Q12_3) # People choose their leaders in free elections.
# lattice::histogram(dat$Q12_3, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

#
dat$Q12_5 = as.numeric(dat$Q12_5) # The army takes over when government is incompetent
# lattice::histogram(dat$Q12_5, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

# The army takes over when government is incompetent / (high/low)
dat$Q12_5_highlow = ifelse(dat$Q12_5 >= median(dat$Q12_5), 1, 0)
dat$Q12_5_highlow = as.factor(dat$Q12_5_highlow)

dat$Q12_5_highlow = recode_factor(as.factor(dat$Q12_5_highlow),  # the army takes over when government is incompetent
                                  "0" = "Not an essential characteristic", 
                                  "1" = "An essential characteristic"
                                  )

#
dat$Q12_7 = as.numeric(dat$Q12_7) # Civil rights protect people from state oppression.
# lattice::histogram(dat$Q12_7, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

#
dat$Q12_8 = as.numeric(dat$Q12_8) # People obey their rulers.
# lattice::histogram(dat$Q12_8, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 

#
dat$Q12_9 = as.numeric(dat$Q12_9) # Women have the same rights as men.
# lattice::histogram(dat$Q12_9, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 


# Recode Original Dataset
dat$Boric.Kast = as.factor(dat$Q13)
dat$Education = as.factor(dat$Q5)
dat$Gender = as.factor(dat$Q4)
dat$Income = as.factor(dat$Q6)

# Income Low/Mid/High
dat$IncomeLowMidHigh <- recode_factor(dat$Income, 
                                      `Menos de $35.000 mensuales liquidos` = "Low", 
                                      `De $35.001 a $75.000 mensuales liquidos` = "Low",
                                      `De $75.001 a $110.000 mensuales liquidos` = "Low",
                                      `De $110.001 a $150.000 mensuales liquidos ` = "Low",
                                      `De $150.001 a $225.000 mensuales liquidos` = "Low",
                                      `De $225.001 a $350.000 mensuales liquidos` = "Low",
                                      `De $350.001 a $450.000 mensuales liquidos ` = "Mid", 
                                      `De $450.001 a $550.000 mensuales liquidos` = "Mid",
                                      `De $550.001 a $700.000 mensuales liquidos` = "Mid",
                                      `De $700.001 a $1.000.000 mensuales liquidos` = "Mid",
                                      `De $1000.001 a $2.000.000 mensuales liquidos`   = "Mid",
                                      `De $2.000.001 a $3.000.000 mensuales liquidos` = "High",
                                      `De $3.000.001 a $4.500.000 mensuales liquidos` = "High",
                                      `Más de $4.500.000 mensuales liquidos` = "High",
                                      `No sabe / No contesta` = "Don't know")


# Age young/old
dat$Q3_young_old <- recode_factor(dat$Q3, 
                                      `18-24` = "Young", 
                                      `25-34` = "Old",
                                      `35-44` = "Old",
                                      `45-54` = "Old",
                                      `Más de 55` = "Old"
                                  )


# Boric.Kast
dat$Boric.Kast <- recode_factor(dat$Boric.Kast, 
                                `Blanco/Nulo.` = "Other", # "Other", "Null"
                                `GABRIEL BORIC FONT` = "Boric",
                                `JOSÉ ANTONIO KAST RIST` = "Kast",
                                `No voté.` = "Other", # "Other", "Didn't vote"
                                `Prefiero no decir.` = "Other" # "Other", "Don't want to say"
                                )

# dat <- dat[ which(dat$Boric.Kast=="Boric" | dat$Boric.Kast == "Kast"), ]
# dat$Boric.Kast <- droplevels(dat$Boric.Kast)

# Education High/Low
dat$Educ.HighLow <- recode_factor(dat$Education, 
                                  `Menos que educación básica (menos que octavo básico).` = "Low", 
                                  `Educación básica completa (hasta octavo básico).` = "Low",
                                  `Educación media completa.` = "Low",
                                  `Educación técnico-profesional completa.` = "Mid.",
                                  `Educación universitaria completa.` = "High", 
                                  `Magister o Doctorado completo.` = "High", 
                                  `Otro/Prefiero no decir` = "Other"
                                  )


# generate id variable
dat$respondent = 1:nrow(dat)
dat <- dat %>% select(respondent, everything()) # reorder

# summary stats demographics
p_load(vtable,kableExtra)

vars = c('Q3', # Age
         'Q4', # Gender
         'Q5', # Educ
         'Q6' # Income
)

labs <- c('Age',
          'Gender',
          'Education',
          'Income'
)
## ----


## ---- sum:table ----
sumtable <- sumtable(dat,labels=labs, vars = vars,out='latex')
## ---- 

########################################
# Tax Experiment
########################################

# Recode Treatment
dat$block  <- recode_factor(as.factor(dat$block), 
                            `1` = "Control", 
                            `2` = "T1: Infra. (Flat Tax).",
                            `3` = "T2: Infra. (Prog. Tax).",
                            `4` = "T3: Pensions (Flat Tax).",
                            `5` = "T4: Pensions (Prog. Tax)."
                            ) 

lattice::histogram(dat$block, type = "percent", scales=list(y=list(rot=45), x=list(rot=45))) 



# Recode Answer
dat$Q39_6 = as.numeric(dat$Q39_6)

p_load(ggplot2)
ggplot(dat, aes(block, Q39_6)) + 
  geom_boxplot(aes(fill=factor(block))) + 
  labs(title="Box plot", 
       subtitle="",
       caption="",
       x="Experimental Condition",
       y="Scale")



########################################
# Conjoint Data Prep
########################################

## ---- conjoint:prep ----
# name structure is = [4 features][h tasks][2 candidates]

# rename
p_load("dplyr")
dat <- dat %>% 
  rename(
    # features
    "feature1a1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.1.1_CBCONJOINT" ,
    "feature2a1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.1.1_CBCONJOINT" ,
    "feature3a1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.1.1_CBCONJOINT" ,
    "feature4a1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.1.1_CBCONJOINT" ,
    "feature1a2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.1.2_CBCONJOINT" ,
    "feature2a2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.1.2_CBCONJOINT" ,
    "feature3a2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.1.2_CBCONJOINT" ,
    "feature4a2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.1.2_CBCONJOINT" ,
    "feature1b1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.2.1_CBCONJOINT" ,
    "feature2b1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.2.1_CBCONJOINT" ,
    "feature3b1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.2.1_CBCONJOINT" ,
    "feature4b1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.2.1_CBCONJOINT" ,
    "feature1b2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.2.2_CBCONJOINT" ,
    "feature2b2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.2.2_CBCONJOINT" ,
    "feature3b2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.2.2_CBCONJOINT" ,
    "feature4b2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.2.2_CBCONJOINT" ,
    "feature1c1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.3.1_CBCONJOINT" ,
    "feature2c1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.3.1_CBCONJOINT" ,
    "feature3c1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.3.1_CBCONJOINT" ,
    "feature4c1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.3.1_CBCONJOINT" ,
    "feature1c2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.3.2_CBCONJOINT" ,
    "feature2c2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.3.2_CBCONJOINT" ,
    "feature3c2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.3.2_CBCONJOINT" ,
    "feature4c2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.3.2_CBCONJOINT" ,
    "feature1d1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.4.1_CBCONJOINT" ,
    "feature2d1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.4.1_CBCONJOINT" ,
    "feature3d1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.4.1_CBCONJOINT" ,
    "feature4d1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.4.1_CBCONJOINT" ,
    "feature1d2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.4.2_CBCONJOINT" ,
    "feature2d2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.4.2_CBCONJOINT" ,
    "feature3d2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.4.2_CBCONJOINT" ,
    "feature4d2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.4.2_CBCONJOINT" ,
    "feature1e1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.5.1_CBCONJOINT" ,
    "feature2e1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.5.1_CBCONJOINT" ,
    "feature3e1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.5.1_CBCONJOINT" ,
    "feature4e1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.5.1_CBCONJOINT" ,
    "feature1e2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.5.2_CBCONJOINT" ,
    "feature2e2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.5.2_CBCONJOINT" ,
    "feature3e2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.5.2_CBCONJOINT" ,
    "feature4e2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.5.2_CBCONJOINT" ,
    "feature1f1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.6.1_CBCONJOINT" ,
    "feature2f1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.6.1_CBCONJOINT" ,
    "feature3f1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.6.1_CBCONJOINT" ,
    "feature4f1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.6.1_CBCONJOINT" ,
    "feature1f2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.6.2_CBCONJOINT" ,
    "feature2f2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.6.2_CBCONJOINT" ,
    "feature3f2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.6.2_CBCONJOINT" ,
    "feature4f2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.6.2_CBCONJOINT" ,
    "feature1g1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.7.1_CBCONJOINT" ,
    "feature2g1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.7.1_CBCONJOINT" ,
    "feature3g1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.7.1_CBCONJOINT" ,
    "feature4g1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.7.1_CBCONJOINT" ,
    "feature1g2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.7.2_CBCONJOINT" ,
    "feature2g2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.7.2_CBCONJOINT" ,
    "feature3g2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.7.2_CBCONJOINT" ,
    "feature4g2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.7.2_CBCONJOINT" ,
    "feature1h1" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.8.1_CBCONJOINT" ,
    "feature2h1" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.8.1_CBCONJOINT" ,
    "feature3h1" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.8.1_CBCONJOINT" ,
    "feature4h1" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.8.1_CBCONJOINT" ,
    "feature1h2" = "X13773a6d.7567.418e.9a54.5ed4a9e1be74.8.2_CBCONJOINT" ,
    "feature2h2" = "X5e2374ad.f827.494e.ae28.c6a2430508ba.8.2_CBCONJOINT" ,
    "feature3h2" = "X39381113.09d9.4f5e.a4d9.5ce7e8a27c88.8.2_CBCONJOINT" ,
    "feature4h2" = "b1429583.39b1.42fc.8ee2.2559edc4ca94.8.2_CBCONJOINT",
    # choice
    "choice_a" = "C1" ,
    "choice_b" = "C2" ,
    "choice_c" = "C3" ,
    "choice_d" = "C4" ,
    "choice_e" = "C5" ,
    "choice_f" = "C6" ,
    "choice_g" = "C7" ,
    "choice_h" = "C8"
  )


# keep conjoint columns
conjoint.d <- dat %>% dplyr:: select(grep("feature", names(dat)), 
                                     grep("respondent", names(dat)),
                                     grep("choice", names(dat))
                                     )

# CREGGG Approach
p_load(cregg,dplyr)
# https://thomasleeper.com/cregg/
# https://thomasleeper.com/cregg/reference/cj_tidy.html#examples
# "If a variable in the original format records which of the two profiles was chosen (e.g., “left” and “right”), it should go in task_variables"


## profile_variables
list1 <- list(
  feature1 = list( # feature 1
    names(conjoint.d)[grep("^feature1.{1}1", names(conjoint.d))],
    names(conjoint.d)[grep("^feature1.{1}2", names(conjoint.d))]
  ),
  feature2 = list(# feature 2
    names(conjoint.d)[grep("^feature2.{1}1", names(conjoint.d))],
    names(conjoint.d)[grep("^feature2.{1}2", names(conjoint.d))]
  ),
  feature3 = list(# feature 3
    names(conjoint.d)[grep("^feature3.{1}1", names(conjoint.d))],
    names(conjoint.d)[grep("^feature3.{1}2", names(conjoint.d))]
  ),
  feature4 = list(# feature 4
    names(conjoint.d)[grep("^feature4.{1}1", names(conjoint.d))],
    names(conjoint.d)[grep("^feature4.{1}2", names(conjoint.d))]
  )
)

# task variables 
list2 <- list(choice = paste0("choice_", letters[1:8]))

# perform reshape
conjoint.d <- cj_tidy(conjoint.d, 
                      profile_variables = list1,
                      task_variables = list2,
                      id = ~ respondent)

# checking (if nothing happens, it's true)
# stopifnot(nrow(conjoint.d) == nrow(dat)*8*2) # 8 tasks and 2 candidates
  
# recode outcome so it is coded sensibly
conjoint.d$chosen <- ifelse((conjoint.d$profile == "A" & conjoint.d$choice == 1) |
                        (conjoint.d$profile == "B" & conjoint.d$choice == 2), 1, 0)

# rename features
# p_load("dplyr")
conjoint.d <- conjoint.d %>% 
  rename("attr.Gender" = "feature1", "attr.Age" = "feature2","attr.Protest" = "feature3","attr.Pensions" = "feature4")

# features to factor
conjoint.d$attr.Gender = as.factor(conjoint.d$attr.Gender)
conjoint.d$attr.Age = as.factor(conjoint.d$attr.Age)
conjoint.d$attr.Protest = as.factor(conjoint.d$attr.Protest)
conjoint.d$attr.Pensions = as.factor(conjoint.d$attr.Pensions)

# Translate // Recode

## Gender
conjoint.d$attr.Gender <- recode_factor(conjoint.d$attr.Gender, `Mujer` = "Woman", `Hombre` = "Man")

## Age
conjoint.d$attr.Age <- recode_factor(conjoint.d$attr.Age, 
                                     `Entre 35 y 50 años` = "Between 35-50 years old", 
                                     `Menos de 35 años` = "Younger than 35 years old",
                                     `Sobre 50 años` = "Over 50 years old"
                                     )
## Protest
conjoint.d$attr.Protest <- recode_factor(
  conjoint.d$attr.Protest, 
  `El candidato APOYA protestas que busquen desestabilizar el actual gobierno.` = 
    "The candidate SUPPORTS anti-government protest that will seek to de-destabilize the current government", 
  `El candidato SE OPONE a protestas que busquen desestabilizar el actual gobierno.` = 
    "The candidate OPPOSES anti-government protest that will seek to de-destabilize the current government"
  )

## Pensions
conjoint.d$attr.Pensions <- recode_factor(
  conjoint.d$attr.Pensions, 
  `El candidato APOYA un aumento en las pensiones para la tercera edad.` = 
    "The candidate SUPPORTS increases in pensions for the elderly", 
  `El candidato SE OPONE a un aumento en las pensiones para la tercera edad.` = 
    "The candidate OPPOSES increases in pensions for the elderly"
)

# use for analysis
# cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, id = ~ respondent)


# descriptive plotting
# plot(mm(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, id = ~ respondent), vline = 0.5)


##############################
# MERGING WITH LARGER DATASET
##############################

# subset vars from the big dataset to be merged to the conjoint dataset

dat.subset = dat %>% dplyr::select(respondent, Boric.Kast, Education, Educ.HighLow, Gender, Income, IncomeLowMidHigh, Q8_1_highlow, Q12_5_highlow, Q3, Q3_young_old, Q4 , Q10_1 , Q10_2 , Q10_3 , Q10_4 , Q12_1, Q12_1_highlow , Q8_1 , Q12_2 , Q12_3 , Q12_5 , Q12_7 , Q12_8 , Q12_9)

# Merge
conjoint.d = merge(dat.subset, conjoint.d, by.x = "respondent")
## ----



##############################
# CONOINT Data Analyses
##############################


# conjoint.d$BoricKast_Age <- interaction(conjoint.d$Boric.Kast, conjoint.d$Q3_young_old, sep = "_")
# plot(cj(conjoint.d, chosen ~ BoricKast_Age + attr.Gender + attr.Age + attr.Protest + attr.Pensions,id = ~respondent, estimate = "mm", h0 = 0.5))



## ---- conjoint:data:analyses ----

# Marginal Means // Subgroup Analyses: Boric and Kast
mm_BoricKast <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
            id = ~ respondent, 
            estimate = "mm", 
            by = ~Boric.Kast))

BoricKast.p = plot(mm_BoricKast, group = "Boric.Kast", vline = 0.5)



# Marginal Means // Subgroup Analyses: High/Low Satisfaction with Democracy
mm_DemSatis <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                   id = ~respondent, 
                   estimate = "mm", 
                   by = ~Q8_1_highlow))

# DemSatis.p = plot(mm_DemSatis, group = "Q8_1_highlow", vline = 0.5)


# Marginal Means // Subgroup Analyses: Army should take over
mm_ArmyTakesOver <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                  id = ~respondent, 
                  estimate = "mm", 
                  by = ~Q12_5_highlow))

# ArmyTakesOver.p = plot(mm_ArmyTakesOver, group = "Q12_5_highlow", vline = 0.5)


# Marginal Means // Subgroup Analyses: Democracy is not an effective form of government...better a strong leader
mm_StrongLeader <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                       id = ~respondent, 
                       estimate = "mm", 
                       by = ~Q10_2))

# StrongLeader.p = plot(mm_StrongLeader, group = "Q10_2", vline = 0.5)


# Marginal Means // Subgroup Analyses: Democracy might have problems but it's better...
mm_DemIsBetter <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                      id = ~respondent, 
                      estimate = "mm", 
                      by = ~Q10_1))

# DemIsBetter.p = plot(mm_DemIsBetter, group = "Q10_1", vline = 0.5)


# Marginal Means // Subgroup Analyses: right to protest
mm_RightToProtest <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                     id = ~respondent, 
                     estimate = "mm", 
                     by = ~Q10_3))

# RightToProtest.p = plot(mm_RightToProtest, group = "Q10_3", vline = 0.5)

# Marginal Means // Subgroup Analyses: education
mm_Educ <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                        id = ~respondent, 
                        estimate = "mm", 
                        by = ~Education))

# Educ.p = plot(mm_Educ, group = "Education", vline = 0.5)

# Marginal Means // Subgroup Analyses: education High/Low
mm_EducHighLow <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
              id = ~respondent, 
              estimate = "mm", 
              by = ~Educ.HighLow))

# EducHighLow.p = plot(mm_EducHighLow, group = "Educ.HighLow", vline = 0.5)

# Marginal Means // Subgroup Analyses: income Low/Mid/High
mm_IncomeHighLow <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                     id = ~respondent, 
                     estimate = "mm", 
                     by = ~IncomeLowMidHigh))

# IncomeHighLow.p = plot(mm_IncomeHighLow, group = "IncomeLowMidHigh", vline = 0.5)

# Marginal Means // Subgroup Analyses: gov't should tax the rich/poor essential for dem
mm_TaxRichHighLow <- suppressWarnings(cj(conjoint.d, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                       id = ~respondent, 
                       estimate = "mm", 
                       by = ~Q12_1_highlow))

# TaxRichHighLow.p = plot(mm_TaxRichHighLow, group = "Q12_1_highlow", vline = 0.5)
## ----
               






################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
writeLines(paste("Citizen's support for democracy is central for democratic stability, yet recent research has begun to question the depth of this commitment in both new and established democracies. Contrary to most research that concentrates on potential breaches of democratic values by the 'winners,' we turn our attention to the 'losers.' In particular, we seek to understand if individuals who sided with the losing candidate are more open to supporting anti-systemic actions against the government, and whether their stance is influenced by their country's regime type. To do this, we carried out a novel survey experiment in two new democracies, Estonia and Chile (n=", chile.sample.size, ")", ", probing into the willingness of these 'losers' to tolerate transgressions against democratic principles", sep=""), fileConn)
close(fileConn)
## ----





               

##########
# VDEM
##########


# import data
vdem.d <- readRDS("/Users/hectorbahamonde/research/democratic_backsliding/data/vdem.rds")

# Keep Chile and Estonia
vdem.d <- vdem.d[which(vdem.d$country_name=='Chile'  | vdem.d$country_name=='Estonia'),]

# free up memory
gc()

# keep variables
p_load(dplyr)

vdem.d = vdem.d %>% dplyr::select(
  country_name,
  year, 
  v2elfrfair, # Election free and fair
  v2elaccept, # Election losers accept results
  v2psbars,# Barriers to parties
  v2psparban, # Party ban
  # v2psbantar, # Party ban target
  v2exrescon, # Executive respects constitution
  # v2regsupgroups, # Regime support groups
  v2regimpgroup, # Regime most important support group
  v2regsupgroupssize, # Regime support groups size
  v2regsuploc, # Regime support location,
  v2regproreg, # Strongest pro-regime preferences
  v2regantireg, # Strongest anti-regime preferences
  v2regpower, # Most powerful group in affecting regime duration and change
  v2dlcommon, # Common good
  v2dlcountr, # Respect counterarguments
  v2dlengage, # Engaged society
  v2dlencmps, # Particularistic or public goods
  v2jupoatck, # Government attacks on judiciary
  v2clrspct, # Rigorous and impartial public administration
  v2clacjust, # Social class equality in respect for civil liberty
  v2clacfree, # Freedom of academic and cultural expression
  v2clstown, # State ownership of economy
  v2clprptym, # Property rights for men
  v2clprptyw, # Property rights for women
  v2stcritapparm, # Criteria for appointment decisions in the armed forces
  v2strenarm, # Remuneration in the Armed Forces
  # v2csanmvch, # Civil Society CSO anti-system movement character
  v2mecenefm, # Government censorship effort — Media
  v2mecenefi, # Internet censorship effort
  v2mecrit, # Print/broadcast media critical
  v2merange, # Print/broadcast media perspectives
  v2meharjrn, # Harassment of journalists
  v2meslfcen, # Media self-censorship
  v2mebias, # Media bias
  v2pepwrses, # Power distributed by socioeconomic position
  v2pepwrsoc, # Power distributed by social group
  v2peapsecon, # Access to public services distributed by socio-economic position
  v2peasjsoecon, # Access to state jobs by socio-economic position
  v2clpolcl, # Political group equality in respect for civil liberties
  v2cacamps, # Political polarization
  v2caviol, # Political violence
  v2caassemb, # Freedom of peaceful assembly
  # v2casoe, # State of emergency
  v2cagenmob, # Mass mobilization
  v2caconmob, # Mass mobilization concentration
  v2cademmob, # Mobilization for democracy
  v2caautmob, # Mobilization for autocracy
  v2catrauni, # Engagement in independent trade unions
  v2capolit, # Engagement in independent political associations
  v2canonpol, # Engagement in independent non-political associations
  v2cauni, # Existence of universities
  v2caprotac, # Constitutional protection for academic freedom
  v2cafres, # Freedom to research and teach
  v2cainsaut, # Institutional autonomy
  v2cacritic, # Academics as critics
  v3partyid, # Party identification
  v3lgbudglo, # Lower chamber budget
  v2xnp_client, # Clientelism Index
  v2x_rule, # Rule of law index
  v2smgovdom, # Government dissemination of false information domestic
  v2smgovab, # Government dissemination of false information abroad
  v2smgovsmmon, # Government social media monitoring
  v2smgovsmcenprc, # Government social media censorship in practice
  v2smregcap, # Government capacity to regulate online content
  # v2smhargr, # Online harassment groups
  v2smarrest, # Arrests for political content
  v2smpolsoc, # Polarization of society
  v2smpolhate, # Political parties hate speech
  e_polcomp, # Political competition
  e_ti_cpi # Corruption perception index
  #everything()
)


p_load(ggplot2)

p1 = ggplot(vdem.d, aes(year, v2elfrfair, col=country_name)) + geom_smooth() + theme_light() + labs(y="Election free and fair") + theme(legend.position = "none", aspect.ratio=1)
p2 = ggplot(vdem.d, aes(year, v2elaccept, col=country_name)) + geom_smooth() + theme_light() + labs(y="Election losers accept results") + theme(legend.position = "none", aspect.ratio=1)
p3 = ggplot(vdem.d, aes(year, v2psbars, col=country_name)) + geom_smooth() + theme_light() + labs(y="Barriers to parties") + theme(legend.position = "none", aspect.ratio=1)
p4 = ggplot(vdem.d, aes(year, v2psparban, col=country_name)) + geom_smooth() + theme_light() + labs(y="Party ban") + theme(legend.position = "none", aspect.ratio=1)
p5 = ggplot(vdem.d, aes(year, v2exrescon, col=country_name)) + geom_smooth() + theme_light() + labs(y="Executive respects constitution") + theme(legend.position = "none", aspect.ratio=1)
p6 = ggplot(vdem.d, aes(year, v2regimpgroup, col=country_name)) + geom_smooth() + theme_light() + labs(y="Regime most important support group") + theme(legend.position = "none", aspect.ratio=1)
p7 = ggplot(vdem.d, aes(year, v2regsupgroupssize, col=country_name)) + geom_smooth() + theme_light() + labs(y="Regime support groups size") + theme(legend.position = "none", aspect.ratio=1)
p8 = ggplot(vdem.d, aes(year, v2regsuploc, col=country_name)) + geom_smooth() + theme_light() + labs(y="Regime support location") + theme(legend.position = "none", aspect.ratio=1)
p9 = ggplot(vdem.d, aes(year, v2regproreg, col=country_name)) + geom_smooth() + theme_light() + labs(y="Strongest pro-regime preferences") + theme(legend.position = "none", aspect.ratio=1)
p10 = ggplot(vdem.d, aes(year, v2regantireg, col=country_name)) + geom_smooth() + theme_light() + labs(y="Strongest anti-regime preferences") + theme(legend.position = "none", aspect.ratio=1)
p11 = ggplot(vdem.d, aes(year, v2regpower, col=country_name)) + geom_smooth() + theme_light() + labs(y="Most powerful group in affecting regime duration and change") + theme(legend.position = "none", aspect.ratio=1)
p12 = ggplot(vdem.d, aes(year, v2dlcommon, col=country_name)) + geom_smooth() + theme_light() + labs(y="Common good") + theme(legend.position = "none", aspect.ratio=1)
p13 = ggplot(vdem.d, aes(year, v2dlcountr, col=country_name)) + geom_smooth() + theme_light() + labs(y="Respect counterarguments") + theme(legend.position = "none", aspect.ratio=1)
p14 = ggplot(vdem.d, aes(year, v2dlengage, col=country_name)) + geom_smooth() + theme_light() + labs(y="Engaged society") + theme(legend.position = "none", aspect.ratio=1)
p15 = ggplot(vdem.d, aes(year, v2dlencmps, col=country_name)) + geom_smooth() + theme_light() + labs(y="Particularistic or public goods") + theme(legend.position = "none", aspect.ratio=1)
p16 = ggplot(vdem.d, aes(year, v2jupoatck, col=country_name)) + geom_smooth() + theme_light() + labs(y="Government attacks on judiciary") + theme(legend.position = "bottom", aspect.ratio=1)
#
p17 = ggplot(vdem.d, aes(year, v2clrspct, col=country_name)) + geom_smooth() + theme_light() + labs(y="Rigorous and impartial public administration") + theme(legend.position = "none", aspect.ratio=1)
p18 = ggplot(vdem.d, aes(year, v2clacjust, col=country_name)) + geom_smooth() + theme_light() + labs(y="Social class equality in respect for civil liberty") + theme(legend.position = "none", aspect.ratio=1)
p19 = ggplot(vdem.d, aes(year, v2clacfree, col=country_name)) + geom_smooth() + theme_light() + labs(y="Freedom of academic and cultural expression") + theme(legend.position = "none", aspect.ratio=1)
p20 = ggplot(vdem.d, aes(year, v2clstown, col=country_name)) + geom_smooth() + theme_light() + labs(y="State ownership of economy") + theme(legend.position = "none", aspect.ratio=1)
p21 = ggplot(vdem.d, aes(year, v2clprptym, col=country_name)) + geom_smooth() + theme_light() + labs(y="Property rights for men") + theme(legend.position = "none", aspect.ratio=1)
p22 = ggplot(vdem.d, aes(year, v2clprptyw, col=country_name)) + geom_smooth() + theme_light() + labs(y="Property rights for women") + theme(legend.position = "none", aspect.ratio=1)
p23 = ggplot(vdem.d, aes(year, v2stcritapparm, col=country_name)) + geom_smooth() + theme_light() + labs(y="Criteria for appointment decisions in the armed forces") + theme(legend.position = "none", aspect.ratio=1)
p24 = ggplot(vdem.d, aes(year, v2strenarm, col=country_name)) + geom_smooth() + theme_light() + labs(y="Remuneration in the Armed Forces") + theme(legend.position = "none", aspect.ratio=1)
p25 = ggplot(vdem.d, aes(year, v2mecenefm, col=country_name)) + geom_smooth() + theme_light() + labs(y="Government censorship effort — Media") + theme(legend.position = "none", aspect.ratio=1)
p26 = ggplot(vdem.d, aes(year, v2mecenefi, col=country_name)) + geom_smooth() + theme_light() + labs(y="Internet censorship effort")  + theme(legend.position = "none", aspect.ratio=1)
p27 = ggplot(vdem.d, aes(year, v2mecrit, col=country_name)) + geom_smooth() + theme_light() + labs(y="Print/broadcast media critical")  + theme(legend.position = "none", aspect.ratio=1)
p28 = ggplot(vdem.d, aes(year, v2merange, col=country_name)) + geom_smooth() + theme_light() + labs(y="Print/broadcast media perspectives") + theme(legend.position = "none", aspect.ratio=1)
p29 = ggplot(vdem.d, aes(year, v2meharjrn, col=country_name)) + geom_smooth() + theme_light() + labs(y="Harassment of journalists") + theme(legend.position = "none", aspect.ratio=1)
p30 = ggplot(vdem.d, aes(year, v2meslfcen, col=country_name)) + geom_smooth() + theme_light() + labs(y="Media self-censorship") + theme(legend.position = "none", aspect.ratio=1)
p31 = ggplot(vdem.d, aes(year, v2mebias, col=country_name)) + geom_smooth() + theme_light() + labs(y="Media bias") + theme(legend.position = "none", aspect.ratio=1)
p32 = ggplot(vdem.d, aes(year, v2pepwrses, col=country_name)) + geom_smooth() + theme_light() + labs(y="Power distributed by socioeconomic position") + theme(legend.position = "bottom", aspect.ratio=1)
#
p33 = ggplot(vdem.d, aes(year, v2pepwrsoc, col=country_name)) + geom_smooth() + theme_light() + labs(y="Power distributed by social group") + theme(legend.position = "none", aspect.ratio=1)
p34 = ggplot(vdem.d, aes(year, v2peapsecon, col=country_name)) + geom_smooth() + theme_light() + labs(y="Access to public services distributed by socio-economic position") + theme(legend.position = "none", aspect.ratio=1)
p35 = ggplot(vdem.d, aes(year, v2peasjsoecon, col=country_name)) + geom_smooth() + theme_light() + labs(y="Access to state jobs by socio-economic position") + theme(legend.position = "none", aspect.ratio=1)
p36 = ggplot(vdem.d, aes(year, v2clpolcl, col=country_name)) + geom_smooth() + theme_light() + labs(y="Political group equality in respect for civil liberties") + theme(legend.position = "none", aspect.ratio=1)
p37 = ggplot(vdem.d, aes(year, v2cacamps, col=country_name)) + geom_smooth() + theme_light() + labs(y="Political polarization") + theme(legend.position = "none", aspect.ratio=1)
p38 = ggplot(vdem.d, aes(year, v2caviol, col=country_name)) + geom_smooth() + theme_light() + labs(y="Political violence") + theme(legend.position = "none", aspect.ratio=1)
p39 = ggplot(vdem.d, aes(year, v2caassemb, col=country_name)) + geom_smooth() + theme_light() + labs(y="Freedom of peaceful assembly") + theme(legend.position = "none", aspect.ratio=1)
p40 = ggplot(vdem.d, aes(year, v2cagenmob, col=country_name)) + geom_smooth() + theme_light() + labs(y="Mass mobilization") + theme(legend.position = "none", aspect.ratio=1)
p41 = ggplot(vdem.d, aes(year, v2caconmob, col=country_name)) + geom_smooth() + theme_light() + labs(y="Mass mobilization concentration") + theme(legend.position = "none", aspect.ratio=1)
p42 = ggplot(vdem.d, aes(year, v2cademmob, col=country_name)) + geom_smooth() + theme_light() + labs(y="Mobilization for democracy") + theme(legend.position = "none", aspect.ratio=1)
p43 = ggplot(vdem.d, aes(year, v2caautmob, col=country_name)) + geom_smooth() + theme_light() + labs(y="Mobilization for autocracy") + theme(legend.position = "none", aspect.ratio=1)
p44 = ggplot(vdem.d, aes(year, v2catrauni, col=country_name)) + geom_smooth() + theme_light() + labs(y="Engagement in independent trade unions") + theme(legend.position = "none", aspect.ratio=1)
p45 = ggplot(vdem.d, aes(year, v2capolit, col=country_name)) + geom_smooth() + theme_light() + labs(y="Engagement in independent political associations") + theme(legend.position = "none", aspect.ratio=1)
p46 = ggplot(vdem.d, aes(year, v2canonpol, col=country_name)) + geom_smooth() + theme_light() + labs(y="Engagement in independent non-political associations") + theme(legend.position = "none", aspect.ratio=1)
p47 = ggplot(vdem.d, aes(year, v2cauni, col=country_name)) + geom_smooth() + theme_light() + labs(y="Existence of universities") + theme(legend.position = "none", aspect.ratio=1)
p48 = ggplot(vdem.d, aes(year, v2caprotac, col=country_name)) + geom_smooth() + theme_light() + labs(y="Constitutional protection for academic freedom") + theme(legend.position = "bottom", aspect.ratio=1)
#
p49 = ggplot(vdem.d, aes(year, v2cafres, col=country_name)) + geom_smooth() + theme_light() + labs(y="Freedom to research and teach") + theme(legend.position = "none", aspect.ratio=1)
p50 = ggplot(vdem.d, aes(year, v2cainsaut, col=country_name)) + geom_smooth() + theme_light() + labs(y="Institutional autonomy") + theme(legend.position = "none", aspect.ratio=1)
p51 = ggplot(vdem.d, aes(year, v2cacritic, col=country_name)) + geom_smooth() + theme_light() + labs(y="Academics as critics") + theme(legend.position = "none", aspect.ratio=1)
p52 = ggplot(vdem.d, aes(year, v3partyid, col=country_name)) + geom_smooth() + theme_light() + labs(y="Party identification") + theme(legend.position = "none", aspect.ratio=1)
p53 = ggplot(vdem.d, aes(year, v3lgbudglo, col=country_name)) + geom_smooth() + theme_light() + labs(y="Lower chamber budget") + theme(legend.position = "none", aspect.ratio=1)
p54 = ggplot(vdem.d, aes(year, v2xnp_client, col=country_name)) + geom_smooth() + theme_light() + labs(y="Clientelism Index") + theme(legend.position = "none", aspect.ratio=1)
p55 = ggplot(vdem.d, aes(year, v2x_rule, col=country_name)) + geom_smooth() + theme_light() + labs(y="Rule of law index") + theme(legend.position = "none", aspect.ratio=1)
p56 = ggplot(vdem.d, aes(year, v2smgovdom, col=country_name)) + geom_smooth() + theme_light() + labs(y="Government dissemination of false information domestic") + theme(legend.position = "none", aspect.ratio=1)
p57 = ggplot(vdem.d, aes(year, v2smgovab, col=country_name)) + geom_smooth() + theme_light() + labs(y="Government dissemination of false information abroad") + theme(legend.position = "none", aspect.ratio=1)
p58 = ggplot(vdem.d, aes(year, v2smgovsmmon, col=country_name)) + geom_smooth() + theme_light() + labs(y="Government social media monitoring") + theme(legend.position = "none", aspect.ratio=1)
p59 = ggplot(vdem.d, aes(year, v2smgovsmcenprc, col=country_name)) + geom_smooth() + theme_light() + labs(y="Government social media censorship in practice") + theme(legend.position = "none", aspect.ratio=1)
p60 = ggplot(vdem.d, aes(year, v2smregcap, col=country_name)) + geom_smooth() + theme_light() + labs(y="Government capacity to regulate online content") + theme(legend.position = "none", aspect.ratio=1)
p61 = ggplot(vdem.d, aes(year, v2smarrest, col=country_name)) + geom_smooth() + theme_light() + labs(y="Arrests for political content") + theme(legend.position = "none", aspect.ratio=1)
p62 = ggplot(vdem.d, aes(year, v2smpolsoc, col=country_name)) + geom_smooth() + theme_light() + labs(y="Polarization of society") + theme(legend.position = "none", aspect.ratio=1)
p63 = ggplot(vdem.d, aes(year, v2smpolhate, col=country_name)) + geom_smooth() + theme_light() + labs(y="Political parties hate speech") + theme(legend.position = "none", aspect.ratio=1)
p64 = ggplot(vdem.d, aes(year, e_polcomp, col=country_name)) + geom_smooth() + theme_light() + labs(y="Political competition") + theme(legend.position = "none", aspect.ratio=1)
p65 = ggplot(vdem.d, aes(year, e_ti_cpi, col=country_name)) + geom_smooth() + theme_light() + labs(y="Corruption perception index") + theme(legend.position = "bottom", aspect.ratio=1)

plot.1 = cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16, align = "hv",axis = "b", ncol = 4) ; ggsave("1.pdf", plot = plot.1, width = 1400,height = 1400,units = c("px"),dpi = 80)
plot.2 = cowplot::plot_grid(p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, align = "hv",axis = "b", ncol = 4) ; ggsave("2.pdf", plot = plot.2, width = 1400,height = 1400,units = c("px"),dpi = 80)
plot.3 = cowplot::plot_grid(p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, align = "hv",axis = "b", ncol = 4) ; ggsave("3.pdf", plot = plot.3, width = 1400,height = 1400,units = c("px"),dpi = 80)
plot.4 = cowplot::plot_grid(p49, p50, p51, p52, p53, p55, p56, p57, p58, p59, p60, p61, p62, p63, p64, p65, align = "hv",axis = "b", ncol = 4) ; ggsave("4.pdf", plot = plot.4, width = 1400,height = 1400,units = c("px"),dpi = 80)



##########
# WVS (From "Under the veil of democracy" paper)
##########

# cat("\014")
rm(list=ls())
# setwd("/Users/hectorbahamonde/research/democratic_backsliding/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import data
load("/Users/hectorbahamonde/research/democratic_backsliding/data/wvs/WVS_TimeSeries_4_0.rdata")
wvs.d <- data1
rm(data1)

# age: X003
# birth year: X002
# year of survey = S020


# delete labels 
p_load(labelled)
wvs.d = remove_val_labels(wvs.d) # was giving me "heaven" crappy problems. 

# 	Variable	Title	WVS7	WVS6	WVS5	WVS4	WVS3	WVS2	WVS1
# 	E235	Importance of democracy	Q250	V140	V162				


# Keep Chile and Estonia
wvs.d <- wvs.d[which(wvs.d$COW_ALPHA=='CHL'  | wvs.d$COW_ALPHA=='EST'),]

# Transform country var to factor
wvs.d$COW_ALPHA = as.factor(wvs.d$COW_ALPHA)
wvs.d$S020 = as.factor(wvs.d$S020)


# Dropping missing
wvs.d$E235 = ifelse(wvs.d$E235<0,NA,wvs.d$E235) # had -4 and other NA cases.
wvs.d$E224 = ifelse(wvs.d$E224<0,NA,wvs.d$E224) # had -4 and other NA cases.

# Pop-Eleches and Tucker use a continuous measure of number of years the individual has lived under the communist regime
wvs.d$years.lived.in.dictatorship = ifelse(
  wvs.d$COW_ALPHA=='CHL', wvs.d$X002-1989, ifelse(wvs.d$COW_ALPHA=='EST',wvs.d$X002-1991,NA)
)

## Notes: Chile did have dictatorships even before Pinochet (Ibanez del Campo, etc.). Thus, older folks did NOT strictly get socialized during proper democracy. 
## They did get socialized before a *major* dictatorship with a repressive apparatus and a clear political ideology. Prior dictators were more like "caudillos" WITHOUT a clear ideological agenda (in general).

# free up memory
gc()

# re-order dataset
wvs.d <- wvs.d %>% 
  dplyr::select(c("S020", "COW_ALPHA", "X002", "X003", "years.lived.in.dictatorship", "E235"), everything()
                )

# Not so many obs of folks "socialized in democracy"
table(wvs.d[wvs.d$COW_ALPHA=='CHL',]$soc.in.dem)
table(wvs.d[wvs.d$COW_ALPHA=='EST',]$soc.in.dem)

# Mins and Max years of birth
max(wvs.d[wvs.d$COW_ALPHA=="CHL",]$X002)
min(wvs.d[wvs.d$COW_ALPHA=="CHL",]$X002)

max(wvs.d[wvs.d$COW_ALPHA=="EST",]$X002)
min(wvs.d[wvs.d$COW_ALPHA=="EST",]$X002)

# plot
p_load(ggplot2)

## Importance of democracy
ggplot(wvs.d, aes(x=years.lived.in.dictatorship, y=E235, colour=COW_ALPHA)) + 
  geom_smooth() +
  labs(y = "Importance of democracy", x = "years lived in dictatorship (Year of Birth - Year of Democratization)\nIf you're born before democratization (old), negative values. Positive values otherwise (young)") + 
  theme(axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        axis.title.x = element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        plot.title = element_text(size=12),
        strip.text.x = element_text(size = 12))

# Democracy: Governments tax the rich and subsidize the poor (E224)
ggplot(wvs.d, aes(x=years.lived.in.dictatorship, y=E224, colour=COW_ALPHA)) + 
  geom_smooth() +
  labs(y = "Governments tax the rich and subsidize the poor", x = "years lived in dictatorship (Year of Birth - Year of Democratization)\nIf you're born before democratization (old), negative values. Positive values otherwise (young)") + 
  theme(axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        axis.title.x = element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        plot.title = element_text(size=12),
        strip.text.x = element_text(size = 12))
# Comments: This is interesting. First, both trends are similar. Second, in BOTH countries, older cohorts are slightly more supportive of
## the welfare state. Third, in BOTH countries, younger cohorts are slightly less supportive of the welfare state. 
## This to me means: we find evidence in favor of age effects: older folks like welfare presumably because they are on pensions. 
## We DO NOT have enough data to say anything about age effects (not so many younger folks included in the survey).


# basic model
options(scipen=999)
dem.model = lm(E235 ~ X003 + years.lived.in.dictatorship + S020 + COW_ALPHA-1, data=wvs.d) # Importance of democracy
welfare.model = lm(E224 ~ X003 + years.lived.in.dictatorship + S020 + COW_ALPHA-1, data=wvs.d) # Governments tax the rich and subsidize the poor

# plots
p_load(effects,ggpubr)

p1 = plot(effect("years.lived.in.dictatorship", dem.model), as.table=T, ylab="Importance of democracy", xlab="years.lived.in.dictatorship")
p2 = plot(effect("X003", dem.model), as.table=T, ylab="Importance of democracy", xlab="age")
ggarrange(p1,p2, ncol = 2, nrow = 1)

p3 = plot(effect("years.lived.in.dictatorship", welfare.model), as.table=T, ylab="Governments tax the rich and subsidize the poor", xlab="years.lived.in.dictatorship")
p4 = plot(effect("X003", welfare.model), as.table=T, ylab="Governments tax the rich and subsidize the poor", xlab="age")
ggarrange(p3,p4, ncol = 2, nrow = 1)


# Split Data Models
p_load(dyplr)
wvs.d.chile = wvs.d %>% dplyr::filter(COW_ALPHA == 'CHL')
wvs.d.estonia = wvs.d %>% dplyr::filter(COW_ALPHA == 'EST')


options(scipen=999)
dem.model.chile = lm(E235 ~ X003 + years.lived.in.dictatorship + S020-1, data=wvs.d.chile) # Importance of democracy
dem.model.estonia = lm(E235 ~ X003 + years.lived.in.dictatorship, data=wvs.d.estonia) # Importance of democracy NO FE ONLY TWO YEARS


welfare.model.chile = lm(E224 ~ X003 + years.lived.in.dictatorship + S020-1, data=wvs.d.chile) # Governments tax the rich and subsidize the poor
welfare.model.estonia = lm(E224 ~ X003 + years.lived.in.dictatorship, data=wvs.d.estonia) # Governments tax the rich and subsidize the poor

# plots
p_load(effects,ggpubr)

p5.chile.1 = plot(effect("years.lived.in.dictatorship", dem.model.chile), as.table=T, ylab="Importance of democracy", xlab="years.lived.in.dictatorship", main = "Chile")
p5.chile.2 = plot(effect("X003", dem.model.chile), as.table=T, ylab="Importance of democracy", xlab="Age", main = "Chile")
p6.estonia.1 = plot(effect("years.lived.in.dictatorship", dem.model.estonia), as.table=T, ylab="Importance of democracy", xlab="years.lived.in.dictatorship", main = "Estonia")
p6.estonia.2 = plot(effect("X003", dem.model.estonia), as.table=T, ylab="Importance of democracy", xlab="Age", main = "Estonia")
ggarrange(p5.chile.1,p5.chile.2,p6.estonia.1,p6.estonia.2, ncol = 2, nrow = 2)


p7.chile.1 = plot(effect("years.lived.in.dictatorship", welfare.model.chile), as.table=T, ylab="Governments tax the rich\nand subsidize the poor", xlab="years.lived.in.dictatorship", main = "Chile")
p7.chile.2 = plot(effect("X003", welfare.model.chile), as.table=T, ylab="Governments tax the rich\nand subsidize the poor", xlab="Age", main = "Chile")
p8.estonia.1 = plot(effect("years.lived.in.dictatorship", welfare.model.estonia), as.table=T, ylab="Governments tax the rich\nand subsidize the poor", xlab="years.lived.in.dictatorship", main = "Estonia")
p8.estonia.2 = plot(effect("X003", welfare.model.estonia), as.table=T, ylab="Governments tax the rich\nand subsidize the poor", xlab="Age", main = "Estonia")
ggarrange(p7.chile.1,p7.chile.2,p8.estonia.1,p8.estonia.2, ncol = 2, nrow = 2)
# This last one is interesting: opposite effects in both countries. In Chile older COHORTS and AGES support welfare more. In Estonia, the opposite happens.
# However, cohort and age effects are correlated in both cases: we CANNOT know if older folks in Chile want more welfare because they
# do not have one or because they were socialized during the years welfare was more present during their formative years.


