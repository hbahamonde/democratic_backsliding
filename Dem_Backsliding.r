## ---- loadings ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# Import Data
dat.chile <- read.csv("/Users/hectorbahamonde/research/democratic_backsliding/data/Qualtrics/chile_data.csv")
dat.estonia <- read.csv("/Users/hectorbahamonde/research/democratic_backsliding/data/Qualtrics/estonia_data.csv")


# var names
chile.d.var.names = data.frame(
  variable.number = c(colnames(dat.chile)),
  variable.name = c(dat.chile[1,])
  )
estonia.d.var.names = data.frame(
  variable.number = c(colnames(dat.estonia)),
  variable.name = c(dat.estonia[1,])
)

# delete first two/three rows
dat.chile = dat.chile[-c(1, 2), ]
dat.estonia = dat.estonia[-c(1, 2), ]

# sample size data
chile.sample.size = as.numeric(nrow(dat.chile))
estonia.sample.size = as.numeric(nrow(dat.estonia))

# insert country name
dat.chile$Country <- "Chile"
dat.estonia$Country <- "Estonia"

# proxy on external validity: if respondents would have voted for such candidate.
## chile
would.vote.chile.mean = mean(c(
  round((table(dat.chile$DC1)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC96)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC55)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC97)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC46)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC98)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC113)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC114)[[2]]*100)/chile.sample.size,0)
))

would.vote.chile.min = min(c(
  round((table(dat.chile$DC1)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC96)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC55)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC97)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC46)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC98)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC113)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC114)[[2]]*100)/chile.sample.size,0)
))

would.vote.chile.max = max(c(
  round((table(dat.chile$DC1)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC96)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC55)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC97)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC46)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC98)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC113)[[2]]*100)/chile.sample.size,0), 
  round((table(dat.chile$DC114)[[2]]*100)/chile.sample.size,0)
))

## estonia
would.vote.estonia.mean = mean(c(
  round((table(dat.estonia$DC1)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC96)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC55)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC193)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC194)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC195)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC196)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC197)[[2]]*100)/estonia.sample.size,0)
))

would.vote.estonia.min = min(c(
  round((table(dat.estonia$DC1)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC96)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC55)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC193)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC194)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC195)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC196)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC197)[[2]]*100)/estonia.sample.size,0)
))

would.vote.estonia.max = max(c(
  round((table(dat.estonia$DC1)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC96)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC55)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC193)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC194)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC195)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC196)[[2]]*100)/estonia.sample.size,0), 
  round((table(dat.estonia$DC197)[[2]]*100)/estonia.sample.size,0)
))


# convert all character columns to factor
#### THIS BELOW WAS CONVERTING TIME IN A WEIRD WAY
# dat.chile[sapply(dat.chile, is.character)] <- lapply(dat.chile[sapply(dat.chile, is.character)], as.factor)

########################################################
# Re-coding // Descriptive [Chile and Estonia]
########################################################

# Recode Original Dataset
p_load("dplyr")


# dat.chile$Gender = as.factor(dat.chile$Q4)


# Boric.Kast
dat.chile$winners.losers <- recode_factor(dat.chile$Q13,
                                          `GABRIEL BORIC FONT` = "Winner", #"Boric",
                                          `JOSÉ ANTONIO KAST RIST` = "Loser", # "Kast",
                                          `Blanco/Nulo.` = "Other", # "Other", "Null"
                                          `No voté.` = "Other", # "Other", "Didn't vote"
                                          `Prefiero no decir.` = "Other", # "Other", "Don't want to say"
                                          .ordered = TRUE)

# dat.chile <- dat.chile[ which(dat.chile$Boric.Kast=="Boric" | dat.chile$Boric.Kast == "Kast"), ]
# dat.chile$Boric.Kast <- droplevels(dat.chile$Boric.Kast)

# Losers / Winners (Q13)
dat.estonia$winners.losers <- recode_factor(dat.estonia$Q13,
                                            # winners
                                            `Eesti Reformierakond` = "Winner", #  113  
                                            `Eesti 200` = "Winner", # 55
                                            `Sotsiaaldemokraatlik Erakond` = "Winner", # 95
                                            # losers
                                            `Eesti Keskerakond` = "Loser", # 53
                                            `Eesti Konservatiivne Rahvaerakond` = "Loser", # 89
                                            `Isamaa Erakond` = "Loser", # 41
                                            `Eestimaa Ühendatud Vasakpartei` = "Loser", 
                                            `Erakond Eestimaa Rohelised` = "Loser", 
                                            `Erakond Parempoolsed` = "Loser", 
                                            `Muu` = "Loser", 
                                            # other
                                            `Ma ei käinud valimas` = "Other", # "Other",
                                            `Ma ei taha öelda` = "Other", # "Other",
                                            .ordered = TRUE)

## From Mart (2024)
# Winners (currently in governing coalition):  
# 1. Eesti Reformierakond. 
# 4. Eesti 200, 
# 5. Sotsiaaldemokraatlik Erakond. 
#
# Losers (in opposition):  
# 2. Eesti Keskerakond 
# 3. Eesti Konservatiivne Rahvaerakond 
# 6. Isamaa Erakond

# Losers / Winners (Q13)
dat.estonia$Vote.Choice <- recode_factor(dat.estonia$Q13,
                                         `Sotsiaaldemokraatlik Erakond` = "Social Democratic Party",
                                         `Eesti 200` = "Estonia 200",
                                         `Eesti Keskerakond` = "Estonian Centre Party",
                                         `Eesti Konservatiivne Rahvaerakond` = "Estonian Conservative People\'s Party",
                                         `Eesti Reformierakond` = "Estonian Reform Party",
                                         `Eestimaa Ühendatud Vasakpartei` = "Other", #"United Left Party of Estonia",
                                         `Erakond Eestimaa Rohelised` = "Other", #"Estonian Green Party",
                                         `Erakond Parempoolsed` = "Other", #"Party of Right-Wingers",
                                         `Isamaa Erakond` = "Pro Patria Party",
                                         `Ma ei käinud valimas` = "I did not vote",
                                         `Ma ei taha öelda` = "Other",
                                         `Muu` = "Other",
                                         .ordered = TRUE)

# Add a new factor variable for political position (left, center, right)
#dat.estonia$Vote.Choice <- recode_factor(dat.estonia$Vote.Choice,
#                                                `Social Democratic Party` = "Left",
#                                                `Estonia 200` = "Center",
#                                                `Estonian Centre Party` = "Center",
#                                                `Estonian Conservative People\'s Party` = "Right",
#                                                `Estonian Reform Party` = "Right",
#                                                `Pro Patria Party` = "Right",
#                                                `Other` = "Other",
#                                                `I did not vote` = "Did not vote",
#                                                .ordered = TRUE)


# Language (Q7) // Only for Estonia data
dat.estonia$Language <- recode_factor(dat.estonia$Q7,
                                      `Eesti` = "Estonian", #  561  
                                      `Mõni teine keel` = "Other", # 8
                                      #`Ukraina` = "Ukrainian", # 2
                                      `Ukraina` = "Other", # 2
                                      `Vene` = "Russian", # 68
                                      .ordered = TRUE)


# Vote.Choice (Q32) # Chile
## Coding https://www.cambridge.org/core/product/identifier/S104909652300104X/type/journal_article
dat.chile$Vote.Choice <- recode_factor(dat.chile$Q32,
                                       `PARTIDO DE LA GENTE (LISTA A)`= "Partido de la Gente", #"Right",
                                       `TODO POR CHILE (LISTA B) (Partido por la Democracia (PPD), Democracia Cristiana, y Partido Radical)` = "PPD-DC-PR", #"Center",
                                       `PARTIDO REPUBLICANO DE CHILE (LISTA C)` = "Republicano", #"Far-Right",
                                       `UNIDAD PARA CHILE (LISTA D) (Partido Socialista, Partido Comunista, Partido Liberal, Convergencia Social, Revolución Democrática, Comunes, Acción Humanista y FRSV)` = "Unidad Para Chile",# "Left",
                                       `CHILE SEGURO (LISTA E) (UDI, Renovación Nacional (RN) y Evópoli)` = "UDI-RN-Evopoli", #"Right",
                                       `Votó en blanco` = "I did not vote",
                                       `Votó nulo` = "I did not vote",
                                       `No sabe / No contesta` = "Other",
                                       .ordered = TRUE)

# gender
dat.chile$Q4  <- recode_factor(as.factor(dat.chile$Q4), `Hombre` = "Man", `Mujer` = "Woman", "Otro/Prefiero no decir"= "Other") # gender
dat.estonia$Q4  <- recode_factor(as.factor(dat.estonia$Q4), `Mees` = "Man", `Naine` = "Woman", `Ei tea` = "Do not know", `Muu` = "Other" ) # gender
Q4.chile <- dat.chile %>% select(Q4, Country)
Q4.estonia <- dat.estonia %>% select(Q4, Country)
Q4.d <- rbind(Q4.chile, Q4.estonia)

# lattice::histogram(~ Q4.d$Q4 | Q4.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "Gender") 


# Democratic satisfaction Q9_1
## "¿Cuán satisfecho está usted con el funcionamiento de la democracia en Chile?"
## 1-4 "Dissatisfied" 5-6 "Intermediate" 7-10 "Satisfied"
dat.chile$Q9_1  <- recode_factor(as.factor(dat.chile$Q9_1),
                                 "1" = "Dissatisfied", 
                                 "2" = "Dissatisfied", 
                                 "3" = "Dissatisfied", 
                                 "4" = "Dissatisfied", 
                                 "5" = "Intermediate", 
                                 "6" = "Intermediate", 
                                 "7" = "Satisfied", 
                                 "8" = "Satisfied", 
                                 "9" = "Satisfied", 
                                 "10" = "Satisfied",
                                 .ordered = TRUE)

## "Kuivõrd rahul olete te demokraatia toimimisega Eestis?"
dat.estonia$Q9_1  <- recode_factor(as.factor(dat.estonia$Q9_1),  
                                   "1" = "Dissatisfied", 
                                   "2" = "Dissatisfied", 
                                   "3" = "Dissatisfied", 
                                   "4" = "Dissatisfied", 
                                   "5" = "Intermediate", 
                                   "6" = "Intermediate", 
                                   "7" = "Satisfied", 
                                   "8" = "Satisfied", 
                                   "9" = "Satisfied", 
                                   "10" = "Satisfied",
                                   .ordered = TRUE)

# Democracy might have problems but it's better...
dat.chile$Q10_1  <- recode_factor(as.factor(dat.chile$Q10_1),  
                                  "Completamente de acuerdo" = "Agree completely", 
                                  "Un poco de acuerdo" = "Agree to some extent",
                                  "Un poco en desacuerdo" = "Somewhat disagree",
                                  "Completamente en desacuerdo" = "Completely disagree",
                                  .ordered = TRUE)

dat.estonia$Q10_1  <- recode_factor(as.factor(dat.estonia$Q10_1),  
                                    "Täiesti nõus" = "Agree completely",
                                    "Nõus" = "Agree to some extent",
                                    "Ei ole nõus" = "Somewhat disagree", 
                                    "Üldse ei ole nõus" = "Completely disagree",
                                    .ordered = TRUE)

Q10_1.chile <- dat.chile %>% select(Q10_1, Country)
Q10_1.estonia <- dat.estonia %>% select(Q10_1, Country)
Q10_1.d <- rbind(Q10_1.chile, Q10_1.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Dem_might_have_problems.pdf") # The height of the plot in inches
# lattice::histogram(~ Q10_1.d$Q10_1 | Q10_1.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "Democracy may have problems, but it is better than other forms of government.") 
# dev.off()

# Democracy might have problems but it's better... RECODED
dat.chile$Q10_1.r  <- recode_factor(as.factor(dat.chile$Q10_1),  
                                    "Agree completely" = "Agree",
                                    "Agree to some extent" = "Agree",
                                    "Somewhat disagree" = "Disagree",
                                    "Completely disagree" = "Disagree",
                                    .ordered = TRUE)

dat.estonia$Q10_1.r  <- recode_factor(as.factor(dat.estonia$Q10_1),  
                                      "Agree completely" = "Agree",
                                      "Agree to some extent" = "Agree",
                                      "Somewhat disagree" = "Disagree",
                                      "Completely disagree" = "Disagree",
                                      .ordered = TRUE)

# Democracy is not an effective form of government...better a strong leader
dat.chile$Q10_2  <- recode_factor(as.factor(dat.chile$Q10_2),  
                                  "Completamente de acuerdo" = "Agree completely", 
                                  "Un poco de acuerdo" = "Agree to some extent",
                                  "Un poco en desacuerdo" = "Somewhat disagree",
                                  "Completamente en desacuerdo" = "Completely disagree",
                                  .ordered = TRUE)

dat.estonia$Q10_2  <- recode_factor(as.factor(dat.estonia$Q10_2),
                                    "Täiesti nõus" = "Agree completely",
                                    "Nõus" = "Agree to some extent",
                                    "Ei ole nõus" = "Somewhat disagree",
                                    "Üldse ei ole nõus" = "Completely disagree",
                                    .ordered = TRUE)

Q10_2.chile <- dat.chile %>% select(Q10_2, Country)
Q10_2.estonia <- dat.estonia %>% select(Q10_2, Country)
Q10_2.d <- rbind(Q10_2.chile, Q10_2.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Dem_not_efficient_strong_leader.pdf") # The height of the plot in inches
# lattice::histogram(~ Q10_2.d$Q10_2 | Q10_2.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "Democracy is not an efficient form of government,\nand it would be better for [country] to be governed by a strong leader\nwho does not have to worry about winning elections.") 
# dev.off()

# Democracy is not an effective form of government...better a strong leader RECODED
dat.chile$Q10_2.r  <- recode_factor(as.factor(dat.chile$Q10_2),  
                                    "Agree completely" = "Agree", 
                                    "Agree to some extent" = "Agree",
                                    "Somewhat disagree" = "Disagree",
                                    "Completely disagree" = "Disagree",
                                    .ordered = TRUE)

dat.estonia$Q10_2.r  <- recode_factor(as.factor(dat.estonia$Q10_2),
                                      "Agree completely" = "Agree", 
                                      "Agree to some extent" = "Agree",
                                      "Somewhat disagree" = "Disagree",
                                      "Completely disagree" = "Disagree",
                                      .ordered = TRUE)

# Civil rights that guarantee political protest should not be restricted
dat.chile$Q10_3  <- recode_factor(as.factor(dat.chile$Q10_3),  # right to protest
                                  "Completamente de acuerdo" = "Agree completely", 
                                  "Un poco de acuerdo" = "Agree to some extent",
                                  "Un poco en desacuerdo" = "Somewhat disagree",
                                  "Completamente en desacuerdo" = "Completely disagree",
                                  .ordered = TRUE)

dat.estonia$Q10_3  <- recode_factor(as.factor(dat.estonia$Q10_3),  # right to protest
                                    "Täiesti nõus" = "Agree completely",
                                    "Nõus" = "Agree to some extent",
                                    "Ei ole nõus" = "Somewhat disagree",
                                    "Üldse ei ole nõus" = "Completely disagree",
                                    .ordered = TRUE)

Q10_3.chile <- dat.chile %>% select(Q10_3, Country)
Q10_3.estonia <- dat.estonia %>% select(Q10_3, Country)
Q10_3.d <- rbind(Q10_3.chile, Q10_3.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Right_to_protest.pdf") # The height of the plot in inches
# lattice::histogram(~ Q10_3.d$Q10_3 | Q10_3.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "Civil rights that guarantee political protest should not be restricted.") 
# dev.off()


# Civil rights that guarantee political protest should not be restricted... RECODED
dat.chile$Q10_3.r  <- recode_factor(as.factor(dat.chile$Q10_3),  # right to protest
                                  "Agree completely" = "Agree", 
                                  "Agree to some extent" = "Agree",
                                  "Somewhat disagree" = "Disagree",
                                  "Completely disagree" = "Disagree",
                                  .ordered = TRUE)

dat.estonia$Q10_3.r  <- recode_factor(as.factor(dat.estonia$Q10_3),  # right to protest
                                    "Agree completely" = "Agree", 
                                    "Agree to some extent" = "Agree",
                                    "Somewhat disagree" = "Disagree",
                                    "Completely disagree" = "Disagree",
                                    .ordered = TRUE)

# It is important that there are free and politically independent media in [country]
dat.chile$Q10_4  <- recode_factor(as.factor(dat.chile$Q10_4),
                                  "Completamente de acuerdo" = "Agree completely", 
                                  "Un poco de acuerdo" = "Agree to some extent",
                                  "Un poco en desacuerdo" = "Somewhat disagree",
                                  "Completamente en desacuerdo" = "Completely disagree",
                                  .ordered = TRUE)

dat.estonia$Q10_4  <- recode_factor(as.factor(dat.estonia$Q10_4),
                                    "Täiesti nõus" = "Agree completely",
                                    "Nõus" = "Agree to some extent",
                                    "Ei ole nõus" = "Somewhat disagree",
                                    "Üldse ei ole nõus" = "Completely disagree",
                                    .ordered = TRUE)

Q10_4.chile <- dat.chile %>% select(Q10_4, Country)
Q10_4.estonia <- dat.estonia %>% select(Q10_4, Country)
Q10_4.d <- rbind(Q10_4.chile, Q10_4.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Free_media.pdf") # The height of the plot in inches
# lattice::histogram(~ Q10_4.d$Q10_4 | Q10_4.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "It is important that there are free and politically independent media in [country].") 
# dev.off()

# It is important that there are free and politically independent media in [country]... RECODED
dat.chile$Q10_4.r  <- recode_factor(as.factor(dat.chile$Q10_4),
                                  "Agree completely" = "Agree", 
                                  "Agree to some extent" = "Agree",
                                  "Somewhat disagree" = "Disagree",
                                  "Completely disagree" = "Disagree",
                                  .ordered = TRUE)

dat.estonia$Q10_4.r  <- recode_factor(as.factor(dat.estonia$Q10_4),
                                    "Agree completely" = "Agree", 
                                    "Agree to some extent" = "Agree",
                                    "Somewhat disagree" = "Disagree",
                                    "Completely disagree" = "Disagree",
                                    .ordered = TRUE)

# Governments should tax the rich to help the poor
dat.chile$Q12_1 = recode_factor(as.factor(dat.chile$Q12_1),  
              "1" = "Not at all",
              "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
              "10" = "Definitely essential",
              .ordered = TRUE)

dat.estonia$Q12_1 = recode_factor(as.factor(dat.estonia$Q12_1),  
                                  "1" = "Not at all",
                                  "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                  "10" = "Definitely essential",
                                  .ordered = TRUE)


Q12_1.chile <- dat.chile %>% select(Q12_1, Country)
Q12_1.estonia <- dat.estonia %>% select(Q12_1, Country)
Q12_1.d <- rbind(Q12_1.chile, Q12_1.estonia)
# lattice::histogram(~ Q12_1.d$Q12_1 | Q12_1.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "Governments should tax the rich to help the poor\nAn essential characteristic of democracy...") 


# Governments should tax the rich to help the poor... RECODED
dat.chile$Q12_1.r = recode_factor(as.factor(dat.chile$Q12_1),  
                                "Not at all" = "Not essential",
                                "2" = "Not essential",
                                "3" = "Not essential",
                                "4" = "Not essential",
                                "5" = "Intermediate",
                                "6" = "Intermediate",
                                "7" = "Definitely essential",
                                "8" = "Definitely essential",
                                "9" = "Definitely essential",
                                "Definitely essential" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_1.r = recode_factor(as.factor(dat.estonia$Q12_1),  
                                    "Not at all" = "Not essential",
                                    "2" = "Not essential",
                                    "3" = "Not essential",
                                    "4" = "Not essential",
                                    "5" = "Intermediate",
                                    "6" = "Intermediate",
                                    "7" = "Definitely essential",
                                    "8" = "Definitely essential",
                                    "9" = "Definitely essential",
                                    "Definitely essential" = "Definitely essential",
                                    .ordered = TRUE)

# Thinking on a scale where one means far left and ten means far right, where do you place yourself?
dat.chile$Q8_1 = recode_factor(as.factor(dat.chile$Q8_1),  
                                "1" = "Left",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Right",
                                .ordered = TRUE)

dat.estonia$Q8_1 = recode_factor(as.factor(dat.estonia$Q8_1),  
                               "1" = "Left",
                               "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                               "10" = "Right",
                               .ordered = TRUE)


Q8_1.chile <- dat.chile %>% select(Q8_1, Country)
Q8_1.estonia <- dat.estonia %>% select(Q8_1, Country)
Q8_1.d <- rbind(Q8_1.chile, Q8_1.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Left_right.pdf") # The height of the plot in inches
# lattice::histogram(~ Q8_1.d$Q8_1 | Q8_1.d$Country , type = "percent", scales=list(y=list(rot=25), x=list(rot=25)), aspect=1, xlab = "Thinking on a scale where one means far left and ten means far right,\nwhere do you place yourself?") 
# dev.off()

# Thinking on a scale where one means far left and ten means far right, where do you place yourself?... RECODED
dat.chile$Q8_1.r = recode_factor(as.factor(dat.chile$Q8_1),  
                                 "Not at all" = "Left",
                                 "2" = "Left",
                                 "3" = "Left",
                                 "4" = "Left",
                                 "5" = "Center",
                                 "6" = "Center",
                                 "7" = "Right",
                                 "8" = "Right",
                                 "9" = "Right",
                                 "Right" = "Right",
                                 .ordered = TRUE)

dat.estonia$Q8_1.r = recode_factor(as.factor(dat.estonia$Q8_1),  
                                   "Not at all" = "Left",
                                   "2" = "Left",
                                   "3" = "Left",
                                   "4" = "Left",
                                   "5" = "Center",
                                   "6" = "Center",
                                   "7" = "Right",
                                   "8" = "Right",
                                   "9" = "Right",
                                   "Right" = "Right",
                                   .ordered = TRUE)

# Religious authorities have the final say in interpreting the country's laws.
dat.chile$Q12_2 = recode_factor(as.factor(dat.chile$Q12_2),  
                                  "1" = "Not at all",
                                  "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                  "10" = "Definitely essential",
                                  .ordered = TRUE)

dat.estonia$Q12_2 = recode_factor(as.factor(dat.estonia$Q12_2),  
                                "1" = "Not at all",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Definitely essential",
                                .ordered = TRUE)

Q12_2.chile <- dat.chile %>% select(Q12_2, Country)
Q12_2.estonia <- dat.estonia %>% select(Q12_2, Country)
Q12_2.d <- rbind(Q12_2.chile, Q12_2.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Rel_auth_final_say.pdf") # The height of the plot in inches
# lattice::histogram(~ Q12_2.d$Q12_2 | Q12_2.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "Religious authorities have the final say in interpreting the country's laws.\nAn essential characteristic of democracy...") 
# dev.off()

# Religious authorities have the final say in interpreting the country's laws... RECODED
dat.chile$Q12_2.r = recode_factor(as.factor(dat.chile$Q12_2),  
                                "Not at all" = "Not essential",
                                "2" = "Not essential",
                                "3" = "Not essential",
                                "4" = "Not essential",
                                "5" = "Intermediate",
                                "6" = "Intermediate",
                                "7" = "Definitely essential",
                                "8" = "Definitely essential",
                                "9" = "Definitely essential",
                                "Definitely essential" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_2.r = recode_factor(as.factor(dat.estonia$Q12_2),  
                                  "Not at all" = "Not essential",
                                  "2" = "Not essential",
                                  "3" = "Not essential",
                                  "4" = "Not essential",
                                  "5" = "Intermediate",
                                  "6" = "Intermediate",
                                  "7" = "Definitely essential",
                                  "8" = "Definitely essential",
                                  "9" = "Definitely essential",
                                  "Definitely essential" = "Definitely essential",
                                  .ordered = TRUE)

# The people should choose their leaders in free elections.
dat.chile$Q12_3 = recode_factor(as.factor(dat.chile$Q12_3),  
                                "1" = "Not at all",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_3 = recode_factor(as.factor(dat.estonia$Q12_3),  
                                  "1" = "Not at all",
                                  "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                  "10" = "Definitely essential",
                                  .ordered = TRUE)

Q12_3.chile <- dat.chile %>% select(Q12_3, Country)
Q12_3.estonia <- dat.estonia %>% select(Q12_3, Country)
Q12_3.d <- rbind(Q12_3.chile, Q12_3.estonia)
# lattice::histogram(~ Q12_3.d$Q12_3 | Q12_3.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "The people should choose their leaders in free elections.\nAn essential characteristic of democracy...") 

# The people should choose their leaders in free elections... RECODED
dat.chile$Q12_3.r = recode_factor(as.factor(dat.chile$Q12_3),  
                                  "Not at all" = "Not essential",
                                  "2" = "Not essential",
                                  "3" = "Not essential",
                                  "4" = "Not essential",
                                  "5" = "Intermediate",
                                  "6" = "Intermediate",
                                  "7" = "Definitely essential",
                                  "8" = "Definitely essential",
                                  "9" = "Definitely essential",
                                  "Definitely essential" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_3.r = recode_factor(as.factor(dat.estonia$Q12_3),  
                                    "Not at all" = "Not essential",
                                    "2" = "Not essential",
                                    "3" = "Not essential",
                                    "4" = "Not essential",
                                    "5" = "Intermediate",
                                    "6" = "Intermediate",
                                    "7" = "Definitely essential",
                                    "8" = "Definitely essential",
                                    "9" = "Definitely essential",
                                    "Definitely essential" = "Definitely essential",
                                  .ordered = TRUE)

# The Army should take control of the state when the Government is not functioning well.
dat.chile$Q12_5 = recode_factor(as.factor(dat.chile$Q12_5),  
                                "1" = "Not at all",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_5 = recode_factor(as.factor(dat.estonia$Q12_5),  
                                  "1" = "Not at all",
                                  "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                  "10" = "Definitely essential",
                                  .ordered = TRUE)

Q12_5.chile <- dat.chile %>% select(Q12_5, Country, winners.losers)
Q12_5.estonia <- dat.estonia %>% select(Q12_5, Country, winners.losers)
Q12_5.d <- rbind(Q12_5.chile, Q12_5.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Army_takes_control.pdf") # The height of the plot in inches
# lattice::histogram(~ Q12_5.d$Q12_5 | Q12_5.d$winners.losers * Q12_5.d$Country, type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "The Army should take control of the state when the Government is not functioning well.\nAn essential characteristic of democracy...") 
# dev.off()

# The Army should take control of the state when the Government is not functioning well... RECODED
dat.chile$Q12_5.r = recode_factor(as.factor(dat.chile$Q12_5),  
                                "Not at all" = "Not essential",
                                "2" = "Not essential",
                                "3" = "Not essential",
                                "4" = "Not essential",
                                "5" = "Intermediate",
                                "6" = "Intermediate",
                                "7" = "Definitely essential",
                                "8" = "Definitely essential",
                                "9" = "Definitely essential",
                                "Definitely essential" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_5.r = recode_factor(as.factor(dat.estonia$Q12_5),  
                                    "Not at all" = "Not essential",
                                    "2" = "Not essential",
                                    "3" = "Not essential",
                                    "4" = "Not essential",
                                    "5" = "Intermediate",
                                    "6" = "Intermediate",
                                    "7" = "Definitely essential",
                                    "8" = "Definitely essential",
                                    "9" = "Definitely essential",
                                    "Definitely essential" = "Definitely essential",
                                  .ordered = TRUE)

# The state should ensure that wages are more equal.
dat.chile$Q12_7 = recode_factor(as.factor(dat.chile$Q12_7),  
                                "1" = "Not at all",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_7 = recode_factor(as.factor(dat.estonia$Q12_7),  
                                "1" = "Not at all",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Definitely essential",
                                .ordered = TRUE)

Q12_7.chile <- dat.chile %>% select(Q12_7, Country)
Q12_7.estonia <- dat.estonia %>% select(Q12_7, Country)
Q12_7.d <- rbind(Q12_7.chile, Q12_7.estonia)
# lattice::histogram(~ Q12_7.d$Q12_7 | Q12_7.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "The state should ensure that wages are more equal.\nAn essential characteristic of democracy...") 

# People should always obey their rulers.
dat.chile$Q12_8 = recode_factor(as.factor(dat.chile$Q12_8),  
                                "1" = "Not at all",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_8 = recode_factor(as.factor(dat.estonia$Q12_8),  
                                  "1" = "Not at all",
                                  "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                  "10" = "Definitely essential",
                                  .ordered = TRUE)

Q12_8.chile <- dat.chile %>% select(Q12_8, Country)
Q12_8.estonia <- dat.estonia %>% select(Q12_8, Country)
Q12_8.d <- rbind(Q12_8.chile, Q12_8.estonia)
# pdf(file = "/Users/hectorbahamonde/research/democratic_backsliding/Obey_rulers.pdf") # The height of the plot in inches
# lattice::histogram(~ Q12_8.d$Q12_8 | Q12_8.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "People should always obey their rulers.\nAn essential characteristic of democracy...") 
# dev.off()

# People should always obey their rulers... RECODED
dat.chile$Q12_8.r = recode_factor(as.factor(dat.chile$Q12_8),  
                                  "Not at all" = "Not essential",
                                  "2" = "Not essential",
                                  "3" = "Not essential",
                                  "4" = "Not essential",
                                  "5" = "Intermediate",
                                  "6" = "Intermediate",
                                  "7" = "Definitely essential",
                                  "8" = "Definitely essential",
                                  "9" = "Definitely essential",
                                  "Definitely essential" = "Definitely essential",
                                  .ordered = TRUE)

dat.estonia$Q12_8.r = recode_factor(as.factor(dat.estonia$Q12_8),  
                                    "Not at all" = "Not essential",
                                    "2" = "Not essential",
                                    "3" = "Not essential",
                                    "4" = "Not essential",
                                    "5" = "Intermediate",
                                    "6" = "Intermediate",
                                    "7" = "Definitely essential",
                                    "8" = "Definitely essential",
                                    "9" = "Definitely essential",
                                    "Definitely essential" = "Definitely essential",
                                    .ordered = TRUE)

# Women should have the same rights as men.
dat.chile$Q12_9 = recode_factor(as.factor(dat.chile$Q12_9),  
                                "1" = "Not at all",
                                "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                "10" = "Definitely essential",
                                .ordered = TRUE)

dat.estonia$Q12_9 = recode_factor(as.factor(dat.estonia$Q12_9),  
                                  "1" = "Not at all",
                                  "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9",
                                  "10" = "Definitely essential",
                                  .ordered = TRUE)

Q12_9.chile <- dat.chile %>% select(Q12_9, Country)
Q12_9.estonia <- dat.estonia %>% select(Q12_9, Country)
Q12_9.d <- rbind(Q12_9.chile, Q12_9.estonia)
# lattice::histogram(~ Q12_9.d$Q12_9 | Q12_9.d$Country , type = "percent", scales=list(y=list(rot=15), x=list(rot=15)), aspect=1, xlab = "Women should have the same rights as men.\nAn essential characteristic of democracy...") 

# Income Low/Mid/High
dat.chile$IncomeLowMidHigh <- recode_factor(
  dat.chile$Q6, 
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
  `No sabe / No contesta` = "Don't know",
  .ordered = TRUE)

dat.estonia$IncomeLowMidHigh <- recode_factor(dat.estonia$Q6, 
                                            `0-699` = "Low", 
                                            `700-1099` = "Low",
                                            `1100-1399` = "Mid", 
                                            `1400-1699` = "Mid",
                                            `1700-1999` = "Mid",
                                            `2000-2299` = "Mid",
                                            `2300-2899` = "High",
                                            `2900-3499` = "High",
                                            `3500-4199` = "High",
                                            `Rohkem kui 4200` = "High",
                                            .ordered = TRUE)

# Age young/old
dat.chile$Q3_young_old <- recode_factor(dat.chile$Q3, 
                                        `18-24` = "Young", 
                                        `25-34` = "Old",
                                        `35-44` = "Old", # ouch...
                                        `45-54` = "Old",
                                        `Más de 55` = "Old",
                                        .ordered = TRUE
                                        )

dat.estonia$Q3_young_old <- recode_factor(dat.estonia$Q3, 
                                          `18-24` = "Young", 
                                          `25-34` = "Old",
                                          `35-44` = "Old", # ouch...
                                          `45-54` = "Old",
                                          `Üle 55` = "Old",
                                          .ordered = TRUE)

# Education High/Low
dat.chile$Educ.HighLow <- recode_factor(
  dat.chile$Q5, 
  `Menos que educación básica (menos que octavo básico).` = "Low", 
  `Educación básica completa (hasta octavo básico).` = "Low",
  `Educación media completa.` = "Low",
  `Educación técnico-profesional completa.` = "Mid.",
  `Educación universitaria completa.` = "High", 
  `Magister o Doctorado completo.` = "High", 
  `Otro/Prefiero no decir` = "Other",
  .ordered = TRUE)

dat.estonia$Educ.HighLow <- recode_factor(
  dat.estonia$Q5,
  `Põhiariduseta` = "Low", #  "Without primary education"
  `Põhiharidus` = "Low", # "Primary education"
  `Keskharidus` = "Low", # "Secondary education"
  `Kutseharidus` = "Mid.", # "Vocational education"
  `Ülikooli bakalaureusekraad (3-4 aastat õpinguid)` = "High", # "University Bachelor's degree (3-4 years of study)"
  `Magistri- või doktorikraad` = "High", # "Master's or Doctorate degree"
  `Ei tea` = "Other", # "Don't know"
  `Muu` = "Other", # "Other"
  .ordered = TRUE)

# table(dat.chile$Educ.HighLow)
# table(dat.estonia$Educ.HighLow)

# generate id variable
dat.chile$respondent = 1:nrow(dat.chile)
dat.chile <- dat.chile %>% select(respondent, everything()) # reorder

dat.estonia$respondent = 1:nrow(dat.estonia)
dat.estonia <- dat.estonia %>% select(respondent, everything()) # reorder

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
          'Income')

# save dataset
save(dat.chile, file = "/Users/hectorbahamonde/research/democratic_backsliding/chile_data.RData")
save(dat.estonia, file = "/Users/hectorbahamonde/research/democratic_backsliding/estonia_data.RData")
## ----


########################################
# Conjoint Data Prep [Estonia]
########################################

cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# Load Data
load("/Users/hectorbahamonde/research/democratic_backsliding/estonia_data.RData") # Load data

# name structure is = [4 features][h tasks][2 candidates]

# rename
p_load("dplyr")
dat.estonia <- dat.estonia %>% 
  rename(
    # features
    "feature1a1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.1.1_CBCONJOINT",
    "feature2a1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.1.1_CBCONJOINT",
    "feature3a1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.1.1_CBCONJOINT",
    "feature4a1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.1.1_CBCONJOINT",
    "feature1a2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.1.2_CBCONJOINT",
    "feature2a2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.1.2_CBCONJOINT",
    "feature3a2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.1.2_CBCONJOINT",
    "feature4a2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.1.2_CBCONJOINT",
    "feature1b1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.2.1_CBCONJOINT",
    "feature2b1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.2.1_CBCONJOINT",
    "feature3b1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.2.1_CBCONJOINT",
    "feature4b1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.2.1_CBCONJOINT",
    "feature1b2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.2.2_CBCONJOINT",
    "feature2b2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.2.2_CBCONJOINT",
    "feature3b2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.2.2_CBCONJOINT",
    "feature4b2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.2.2_CBCONJOINT",
    "feature1c1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.3.1_CBCONJOINT",
    "feature2c1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.3.1_CBCONJOINT",
    "feature3c1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.3.1_CBCONJOINT",
    "feature4c1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.3.1_CBCONJOINT",
    "feature1c2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.3.2_CBCONJOINT",
    "feature2c2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.3.2_CBCONJOINT",
    "feature3c2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.3.2_CBCONJOINT",
    "feature4c2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.3.2_CBCONJOINT",
    "feature1d1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.4.1_CBCONJOINT",
    "feature2d1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.4.1_CBCONJOINT",
    "feature3d1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.4.1_CBCONJOINT",
    "feature4d1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.4.1_CBCONJOINT",
    "feature1d2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.4.2_CBCONJOINT",
    "feature2d2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.4.2_CBCONJOINT",
    "feature3d2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.4.2_CBCONJOINT",
    "feature4d2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.4.2_CBCONJOINT",
    "feature1e1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.5.1_CBCONJOINT",
    "feature2e1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.5.1_CBCONJOINT",
    "feature3e1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.5.1_CBCONJOINT",
    "feature4e1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.5.1_CBCONJOINT",
    "feature1e2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.5.2_CBCONJOINT",
    "feature2e2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.5.2_CBCONJOINT",
    "feature3e2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.5.2_CBCONJOINT",
    "feature4e2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.5.2_CBCONJOINT",
    "feature1f1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.6.1_CBCONJOINT",
    "feature2f1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.6.1_CBCONJOINT",
    "feature3f1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.6.1_CBCONJOINT",
    "feature4f1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.6.1_CBCONJOINT",
    "feature1f2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.6.2_CBCONJOINT",
    "feature2f2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.6.2_CBCONJOINT",
    "feature3f2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.6.2_CBCONJOINT",
    "feature4f2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.6.2_CBCONJOINT",
    "feature1g1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.7.1_CBCONJOINT",
    "feature2g1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.7.1_CBCONJOINT",
    "feature3g1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.7.1_CBCONJOINT",
    "feature4g1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.7.1_CBCONJOINT",
    "feature1g2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.7.2_CBCONJOINT",
    "feature2g2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.7.2_CBCONJOINT",
    "feature3g2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.7.2_CBCONJOINT",
    "feature4g2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.7.2_CBCONJOINT",
    "feature1h1" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.8.1_CBCONJOINT",
    "feature2h1" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.8.1_CBCONJOINT",
    "feature3h1" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.8.1_CBCONJOINT",
    "feature4h1" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.8.1_CBCONJOINT",
    "feature1h2" = "d2796cd0.1d9f.4eb0.ba54.81aa3835a25f.8.2_CBCONJOINT",
    "feature2h2" = "X33b5bd0f.eaf2.4b73.8859.ff542e570638.8.2_CBCONJOINT",
    "feature3h2" = "X6965f897.4fef.400d.b747.aa7fd2ab1dc6.8.2_CBCONJOINT",
    "feature4h2" = "X567734f3.ac66.4120.a1a8.ee68b0db61a4.8.2_CBCONJOINT",
    # choice
    "choice_a" = "C1" ,
    "choice_b" = "C2" ,
    "choice_c" = "C3" ,
    "choice_d" = "C4" ,
    "choice_e" = "C5" ,
    "choice_f" = "C6" ,
    "choice_g" = "C7" ,
    "choice_h" = "C8")

# keep conjoint columns
conjoint.d.estonia <- dat.estonia %>% dplyr:: select(grep("feature", names(dat.estonia)), 
                                                     grep("respondent", names(dat.estonia)),
                                                     grep("choice", names(dat.estonia)))

# CREGGG Approach
p_load(cregg,dplyr)
# https://thomasleeper.com/cregg/
# https://thomasleeper.com/cregg/reference/cj_tidy.html#examples
# "If a variable in the original format records which of the two profiles was chosen (e.g., “left” and “right”), it should go in task_variables"


## profile_variables
list1 <- list(
  feature1 = list( # feature 1
    names(conjoint.d.estonia)[grep("^feature1.{1}1", names(conjoint.d.estonia))],
    names(conjoint.d.estonia)[grep("^feature1.{1}2", names(conjoint.d.estonia))]
  ),
  feature2 = list(# feature 2
    names(conjoint.d.estonia)[grep("^feature2.{1}1", names(conjoint.d.estonia))],
    names(conjoint.d.estonia)[grep("^feature2.{1}2", names(conjoint.d.estonia))]
  ),
  feature3 = list(# feature 3
    names(conjoint.d.estonia)[grep("^feature3.{1}1", names(conjoint.d.estonia))],
    names(conjoint.d.estonia)[grep("^feature3.{1}2", names(conjoint.d.estonia))]
  ),
  feature4 = list(# feature 4
    names(conjoint.d.estonia)[grep("^feature4.{1}1", names(conjoint.d.estonia))],
    names(conjoint.d.estonia)[grep("^feature4.{1}2", names(conjoint.d.estonia))]
  )
)

# task variables 
list2 <- list(choice = paste0("choice_", letters[1:8]))

# perform reshape
conjoint.d.estonia <- cj_tidy(conjoint.d.estonia, 
                              profile_variables = list1,
                              task_variables = list2,
                              id = ~ respondent)

# checking (if nothing happens, it's true)
# stopifnot(nrow(conjoint.d.estonia) == nrow(dat.estonia)*8*2) # 8 tasks and 2 candidates

# recode outcome so it is coded sensibly
conjoint.d.estonia$chosen <- ifelse((conjoint.d.estonia$profile == "A" & conjoint.d.estonia$choice == 1) |
                                      (conjoint.d.estonia$profile == "B" & conjoint.d.estonia$choice == 2), 1, 0)

# rename features
# p_load("dplyr")
conjoint.d.estonia <- conjoint.d.estonia %>% 
  rename("attr.Gender" = "feature1", 
         "attr.Age" = "feature2",
         "attr.Protest" = "feature3",
         "attr.Pensions" = "feature4")

# features to factor
conjoint.d.estonia$attr.Gender = as.factor(conjoint.d.estonia$attr.Gender)
conjoint.d.estonia$attr.Age = as.factor(conjoint.d.estonia$attr.Age)
conjoint.d.estonia$attr.Protest = as.factor(conjoint.d.estonia$attr.Protest)
conjoint.d.estonia$attr.Pensions = as.factor(conjoint.d.estonia$attr.Pensions)

## Gender
conjoint.d.estonia$attr.Gender <- recode_factor(
  conjoint.d.estonia$attr.Gender, 
  `Mees` = "Man", 
  `Naine` = "Woman")

## Age
conjoint.d.estonia$attr.Age <- recode_factor(conjoint.d.estonia$attr.Age, 
                                             `Alla 35` = "Younger than 35 years old",
                                             `35-50` = "Between 35-50 years old", 
                                             `Üle 50` = "Over 50 years old")

## Protest
conjoint.d.estonia$attr.Protest <- recode_factor(
  conjoint.d.estonia$attr.Protest, 
  `Kandidaat TOETAB meeleavaldusi tänavatel praeguse valitsuse destabiliseerimiseks.` = 
    "The candidate SUPPORTS anti-government protest\nthat will seek to de-destabilize the current government", 
  `Kandidaat ON VASTU meeleavaldustele tänavatel praeguse valitsuse destabiliseerimiseks.` = 
    "The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government")

## Pensions
conjoint.d.estonia$attr.Pensions <- recode_factor(
  conjoint.d.estonia$attr.Pensions, 
  `Kandidaat TOETAB pensionite tõstmist.` = 
    "The candidate SUPPORTS increases in pensions for the elderly", 
  `Kandidaat ON VASTU pensionite tõstmisele.` = 
    "The candidate OPPOSES increases in pensions for the elderly")

##############################
# MERGING WITH LARGER DATASET [Estonia]
##############################

# Q10_1 # Democracy might have problems but it's better...
# Q10_2 # Democracy is not an effective form of government...better a strong leader
# Q10_3 # Civil rights that guarantee political protest should not be restricted
# Q10_4 # It is important that there are free and politically independent media in [country]
# Q12_1 # Governments should tax the rich to help the poor
# Q8_1 # Thinking on a scale where one means far left and ten means far right, where do you place yourself?
# Q12_2 # Religious authorities have the final say in interpreting the country's laws.
# Q12_3 # The people should choose their leaders in free elections.
# Q12_5 # The Army should take control of the state when the Government is not functioning well.
# Q12_7 # The state should ensure that wages are more equal.
# Q12_8 # People should always obey their rulers.
# Q12_9 # Women should have the same rights as men.
# IncomeLowMidHigh # Income Low/Mid/High
# Q3_young_old # Age young/old
# Educ.HighLow # Education High/Low
# Vote.Choice
# Language
# Q9_1 # Dem Satis

# subset vars from the big dataset to be merged to the conjoint dataset
dat.subset.estonia = dat.estonia %>% dplyr::select(
  respondent, 
  winners.losers,
  Q10_1, Q10_1.r, Q10_2, Q10_2.r, Q10_3, Q10_3.r, Q10_4, Q10_4.r, Q12_1, Q12_1.r, Q8_1, Q8_1.r, Q12_2, Q12_2.r, Q12_3, Q12_3.r, Q12_5, Q12_5.r,
  Q12_7, Q12_8, Q12_8.r, Q12_9, IncomeLowMidHigh, Q3_young_old, Educ.HighLow,
  Vote.Choice, Language, Q9_1
  )

# Merge
conjoint.d.estonia = merge(dat.subset.estonia, conjoint.d.estonia, by.x = "respondent")

########################################
# Conjoint Data Prep [Chile]
########################################

#cat("\014")
#rm(list=ls())
#setwd("/Users/hectorbahamonde/research/democratic_backsliding/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# Load Data
load("/Users/hectorbahamonde/research/democratic_backsliding/chile_data.RData") # Load data


## ---- conjoint:prep ----
# name structure is = [4 features][h tasks][2 candidates]

# rename
p_load("dplyr")
dat.chile <- dat.chile %>% 
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
    "choice_h" = "C8")

# keep conjoint columns
conjoint.d.chile <- dat.chile %>% dplyr:: select(grep("feature", names(dat.chile)), 
                                                 grep("respondent", names(dat.chile)),
                                                 grep("choice", names(dat.chile)))

# CREGGG Approach
p_load(cregg,dplyr)
# https://thomasleeper.com/cregg/
# https://thomasleeper.com/cregg/reference/cj_tidy.html#examples
# "If a variable in the original format records which of the two profiles was chosen (e.g., “left” and “right”), it should go in task_variables"

## profile_variables
list1 <- list(
  feature1 = list( # feature 1
    names(conjoint.d.chile)[grep("^feature1.{1}1", names(conjoint.d.chile))],
    names(conjoint.d.chile)[grep("^feature1.{1}2", names(conjoint.d.chile))]
  ),
  feature2 = list(# feature 2
    names(conjoint.d.chile)[grep("^feature2.{1}1", names(conjoint.d.chile))],
    names(conjoint.d.chile)[grep("^feature2.{1}2", names(conjoint.d.chile))]
  ),
  feature3 = list(# feature 3
    names(conjoint.d.chile)[grep("^feature3.{1}1", names(conjoint.d.chile))],
    names(conjoint.d.chile)[grep("^feature3.{1}2", names(conjoint.d.chile))]
  ),
  feature4 = list(# feature 4
    names(conjoint.d.chile)[grep("^feature4.{1}1", names(conjoint.d.chile))],
    names(conjoint.d.chile)[grep("^feature4.{1}2", names(conjoint.d.chile))]
  )
)

# task variables 
list2 <- list(choice = paste0("choice_", letters[1:8]))

# perform reshape
conjoint.d.chile <- cj_tidy(conjoint.d.chile, 
                            profile_variables = list1,
                            task_variables = list2,
                            id = ~ respondent)

# checking (if nothing happens, it's true)
# stopifnot(nrow(conjoint.d.chile) == nrow(dat.chile)*8*2) # 8 tasks and 2 candidates

# recode outcome so it is coded sensibly
conjoint.d.chile$chosen <- ifelse((conjoint.d.chile$profile == "A" & conjoint.d.chile$choice == 1) |
                                    (conjoint.d.chile$profile == "B" & conjoint.d.chile$choice == 2), 1, 0)

# rename features
# p_load("dplyr")
conjoint.d.chile <- conjoint.d.chile %>% 
  rename("attr.Gender" = "feature1", "attr.Age" = "feature2","attr.Protest" = "feature3","attr.Pensions" = "feature4")

# features to factor
conjoint.d.chile$attr.Gender = as.factor(conjoint.d.chile$attr.Gender)
conjoint.d.chile$attr.Age = as.factor(conjoint.d.chile$attr.Age)
conjoint.d.chile$attr.Protest = as.factor(conjoint.d.chile$attr.Protest)
conjoint.d.chile$attr.Pensions = as.factor(conjoint.d.chile$attr.Pensions)

# Translate // Recode

## Gender
conjoint.d.chile$attr.Gender <- recode_factor(conjoint.d.chile$attr.Gender, `Mujer` = "Woman", `Hombre` = "Man")

## Age
conjoint.d.chile$attr.Age <- recode_factor(conjoint.d.chile$attr.Age, 
                                           `Menos de 35 años` = "Younger than 35 years old",
                                           `Entre 35 y 50 años` = "Between 35-50 years old", 
                                           `Sobre 50 años` = "Over 50 years old")

## Protest
conjoint.d.chile$attr.Protest <- recode_factor(
  conjoint.d.chile$attr.Protest, 
  `El candidato APOYA protestas que busquen desestabilizar el actual gobierno.` = 
    "The candidate SUPPORTS anti-government protest\nthat will seek to de-destabilize the current government", 
  `El candidato SE OPONE a protestas que busquen desestabilizar el actual gobierno.` = 
    "The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government")

## Pensions
conjoint.d.chile$attr.Pensions <- recode_factor(
  conjoint.d.chile$attr.Pensions, 
  `El candidato APOYA un aumento en las pensiones para la tercera edad.` = 
    "The candidate SUPPORTS increases in pensions for the elderly", 
  `El candidato SE OPONE a un aumento en las pensiones para la tercera edad.` = 
    "The candidate OPPOSES increases in pensions for the elderly")

# use for analysis
# cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, id = ~ respondent)

# descriptive plotting
# plot(mm(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, id = ~ respondent), vline = 0.5)

##############################
# MERGING WITH LARGER DATASET
##############################

## Q10_1 # Democracy might have problems but it's better...
## Q10_2 # Democracy is not an effective form of government...better a strong leader
## Q10_3 # Civil rights that guarantee political protest should not be restricted
## Q10_4 # It is important that there are free and politically independent media in [country]
## Q12_1 # Governments should tax the rich to help the poor
## Q8_1 # Thinking on a scale where one means far left and ten means far right, where do you place yourself?
## Q12_2 # Religious authorities have the final say in interpreting the country's laws.
## Q12_3 # The people should choose their leaders in free elections.
## Q12_5 # The Army should take control of the state when the Government is not functioning well.
# Q12_7 # The state should ensure that wages are more equal.
## Q12_8 # People should always obey their rulers.
# Q12_9 # Women should have the same rights as men.
## IncomeLowMidHigh # Income Low/Mid/High
# Q3_young_old # Age young/old
## Educ.HighLow # Education High/Low
# Vote.Choice
# Q9_1 # Dem Satis

# subset vars from the big dataset to be merged to the conjoint dataset
dat.subset = dat.chile %>% dplyr::select(respondent, winners.losers, Educ.HighLow, 
                                         IncomeLowMidHigh, Q3, Q3_young_old, Q4, 
                                         Q10_1, Q10_1.r, Q10_2, Q10_2.r, Q10_3, Q10_3.r, Q10_4, Q10_4.r, Q12_1, Q12_1.r, Q8_1, Q8_1.r, 
                                         Q12_2, Q12_2.r, Q12_3, Q12_3.r, Q12_5, Q12_5.r, Q12_7, Q12_8, Q12_8.r, Q12_9, Vote.Choice, Q9_1)

# Merge
conjoint.d.chile = merge(dat.subset, conjoint.d.chile, by.x = "respondent")
## ----

##############################
# CONOINT Data Analyses
##############################

# We explore sub-group differences in the propensity to support anti-systemic action by 
# -respondents' partisanship, 
# -democratic satisfaction and 
# -support for democratic norms

p_load(ggplot2)


#####################################################
# Marginal Means // Subgroup Analyses: 
# Winners.Losers
#####################################################

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
# mm_Winner_Loser.p <- plot(mm_Winner_Loser.d, group = "winners.losers", vline = 0.5)
# mm_Winner_Loser.p %+% facet_wrap(~Country)

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
mm_Winner_Loser.p = ggplot(mm_Winner_Loser.d,
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

ggsave(mm_Winner_Loser.p, file="Conjoint_Winner_Loser.pdf", width=12, height=10)


#####################################################
# Marginal Means // Subgroup Analyses: 
# Vote.Choice
#####################################################

mm_Vote_Choice_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                             id = ~ respondent, 
                                             estimate = "mm", 
                                             by = ~Vote.Choice))

mm_Vote_Choice_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Vote.Choice))


# plot(mm_Vote_Choice_Estonia, group = "Vote.Choice", vline = 0.5)
# plot(mm_Vote_Choice_Chile, group = "Vote.Choice", vline = 0.5)

#######
# One plot per country
#######


# Chile plot
mm_Vote_Choice_Chile$level <- factor(mm_Vote_Choice_Chile$level, levels = desired_order)
mm_Vote_Choice_Chile = mm_Vote_Choice_Chile %>% filter(feature == "attr.Protest")


p_load(ggplot2)
mm_Vote_Choice_Chile.p = ggplot(mm_Vote_Choice_Chile,
                                aes(factor(level),
                                    y = estimate,
                                    ymin = lower,
                                    ymax = upper,
                                    color = factor(Vote.Choice))) + 
  geom_hline(yintercept = 0.5, colour = "black", lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.25) +
  #facet_wrap(~Country) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12), 
    axis.title.x = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12),
    strip.text.x = element_text(size = 12)
  ) +
  guides(colour = guide_legend(title = "Chile")) + 
  labs(x = "", y = "")

ggsave(mm_Vote_Choice_Chile.p, file="Conjoint_Vote_Choice_Chile.pdf", width=12, height=7)

# Estonia Plot
mm_Vote_Choice_Estonia$level <- factor(mm_Vote_Choice_Estonia$level, levels = desired_order)
mm_Vote_Choice_Estonia = mm_Vote_Choice_Estonia %>% filter(feature == "attr.Protest")

p_load(ggplot2)
mm_Vote_Choice_Estonia.p = ggplot(mm_Vote_Choice_Estonia,
                                  aes(factor(level),
                                      y = estimate,
                                      ymin = lower,
                                      ymax = upper,
                                      color = factor(Vote.Choice))) + 
  geom_hline(yintercept = 0.5, colour = "black", lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.25) +
  #facet_wrap(~Country) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12), 
    axis.title.x = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12),
    strip.text.x = element_text(size = 12)
  ) +
  guides(colour = guide_legend(title = "Estonia")) + 
  labs(x = "", y = "")

ggsave(mm_Vote_Choice_Estonia.p, file="Conjoint_Vote_Choice_Estonia.pdf", width=12, height=7)

#
p_load(ggpubr)
theme_set(theme_pubr())

mm_Vote_Choice_Separate.p = ggarrange(mm_Vote_Choice_Chile.p, mm_Vote_Choice_Estonia.p,
          labels = c("Chile", "Estonia"),
          ncol = 1,
          nrow = 2,
          widths = c(500),
          heights = c(1500, 1500))

ggexport(mm_Vote_Choice_Separate.p, filename = "mm_Vote_Choice_Separate_p.pdf",
         width = 15,
         #height = 480,
         #pointsize = 12,
         res = 250,
         verbose = F)

#######
# Combining both countries in one plot
#######

mm_Vote_Choice_Chile$Country <- "Chile"
mm_Vote_Choice_Estonia$Country <- "Estonia"

mm_Vote_Choice.d = rbind(mm_Vote_Choice_Chile, mm_Vote_Choice_Estonia)
# mm_Vote_Choice.p <- plot(mm_Vote_Choice.d, group = "winners.losers", vline = 0.5)
# mm_Vote_Choice.p %+% facet_wrap(~Country)

mm_Vote_Choice.d$level <- factor(mm_Vote_Choice.d$level, levels = desired_order)


p_load(ggplot2)
mm_Vote_Choice.p = ggplot(mm_Vote_Choice.d,
                          aes(factor(level),
                              y = estimate,
                              ymin = lower,
                              ymax = upper,
                              color = factor(Vote.Choice))) + 
  geom_hline(yintercept = 0.5, colour = "black", lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.25) +
  facet_wrap(~Country) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14), 
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    strip.text.x = element_text(size = 14)
  ) +
  guides(colour = guide_legend(title = "")) + 
  labs(x = "", y = "") +
  scale_color_manual(values = c(
    "Left" = "red",               # Red for left
    "Center-Left" = "purple",     # Blended color between red and blue for center-left
    "Center" = "darkgrey",        # Neutral color for center
    "Center-Right" = "lightblue", # Blended color between blue and red for center-right
    "Right" = "blue",             # Blue for right
    "Far-Right" = "darkblue",     # Darker blue for far-right
    "I did not vote" = "orange",  # Characteristic color for non-voters
    "Other" = "green"             # Characteristic color for other
  ))

ggsave(mm_Vote_Choice.p, file="Conjoint_Vote_Choice.pdf", width=12, height=7)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Language
# ONLY ESTONIA
#####################################################

mm_Language_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~ respondent, 
                                              estimate = "mm", 
                                              by = ~Language))
plot(mm_Language_Estonia, group = "Language", vline = 0.5)


#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_3 # The people should choose their leaders in free elections.
#####################################################

mm_Free_Elec_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                          id = ~ respondent, 
                                          estimate = "mm", 
                                          by = ~Q12_3))

mm_Free_Elec_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Q12_3))


mm_Free_Elec_Chile$Country <- "Chile"
mm_Free_Elec_Estonia$Country <- "Estonia"

mm_Free_Elec.d = rbind(mm_Free_Elec_Chile, mm_Free_Elec_Estonia)
mm_Free_Elec.p <- plot(mm_Free_Elec.d, group = "Q12_3", vline = 0.5)
mm_Free_Elec.p %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_3 # The people should choose their leaders in free elections... RECODED
#####################################################

mm_Free_Elec_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Q12_3.r))

mm_Free_Elec_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~ respondent, 
                                              estimate = "mm", 
                                              by = ~Q12_3.r))


mm_Free_Elec_Chile.r$Country <- "Chile"
mm_Free_Elec_Estonia.r$Country <- "Estonia"

mm_Free_Elec.d.r = rbind(mm_Free_Elec_Chile.r, mm_Free_Elec_Estonia.r)
mm_Free_Elec.p.r <- plot(mm_Free_Elec.d.r, group = "Q12_3.r", vline = 0.5)
mm_Free_Elec.p.r %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_2 # Religious authorities have the final say in interpreting the country's laws.
#####################################################

mm_Rel_Auth_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                         id = ~ respondent, 
                                         estimate = "mm", 
                                         by = ~Q12_2))

mm_Rel_Auth_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                           id = ~ respondent, 
                                           estimate = "mm", 
                                           by = ~Q12_2))


mm_Rel_Auth_Chile$Country <- "Chile"
mm_Rel_Auth_Estonia$Country <- "Estonia"

mm_Rel_Auth.d = rbind(mm_Rel_Auth_Chile, mm_Rel_Auth_Estonia)
mm_Rel_Auth.p <- plot(mm_Rel_Auth.d, group = "Q12_2", vline = 0.5)
mm_Rel_Auth.p %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_2 # Religious authorities have the final say in interpreting the country's laws... RECODED
#####################################################

mm_Rel_Auth_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                           id = ~ respondent, 
                                           estimate = "mm", 
                                           by = ~Q12_2.r))

mm_Rel_Auth_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                             id = ~ respondent, 
                                             estimate = "mm", 
                                             by = ~Q12_2.r))


mm_Rel_Auth_Chile.r$Country <- "Chile"
mm_Rel_Auth_Estonia.r$Country <- "Estonia"

mm_Rel_Auth.d.r = rbind(mm_Rel_Auth_Chile.r, mm_Rel_Auth_Estonia.r)
mm_Rel_Auth.p.r <- plot(mm_Rel_Auth.d.r, group = "Q12_2.r", vline = 0.5)
mm_Rel_Auth.p.r %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_8 # People should always obey their rulers.
#####################################################

mm_Obey_Rulers_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Q12_8))

mm_Obey_Rulers_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~ respondent, 
                                              estimate = "mm", 
                                              by = ~Q12_8))


mm_Obey_Rulers_Chile$Country <- "Chile"
mm_Obey_Rulers_Estonia$Country <- "Estonia"

mm_Obey_Rulers.d = rbind(mm_Obey_Rulers_Chile, mm_Obey_Rulers_Estonia)
mm_Obey_Rulers.p <- plot(mm_Obey_Rulers.d, group = "Q12_8", vline = 0.5)
mm_Obey_Rulers.p %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_8 # People should always obey their rulers... RECODED
#####################################################

mm_Obey_Rulers_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~ respondent, 
                                              estimate = "mm", 
                                              by = ~Q12_8.r))

mm_Obey_Rulers_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                                id = ~ respondent, 
                                                estimate = "mm", 
                                                by = ~Q12_8.r))


mm_Obey_Rulers_Chile.r$Country <- "Chile"
mm_Obey_Rulers_Estonia.r$Country <- "Estonia"

mm_Obey_Rulers.d.r = rbind(mm_Obey_Rulers_Chile.r, mm_Obey_Rulers_Estonia.r)
mm_Obey_Rulers.p.r <- plot(mm_Obey_Rulers.d.r, group = "Q12_8.r", vline = 0.5)
mm_Obey_Rulers.p.r %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q10_4 # It is important that there are free and politically independent media in [country]
#####################################################
mm_Free_Media_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                           id = ~ respondent, 
                                           estimate = "mm", 
                                           by = ~Q10_4))

mm_Free_Media_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                             id = ~ respondent, 
                                             estimate = "mm", 
                                             by = ~Q10_4))


mm_Free_Media_Chile$Country <- "Chile"
mm_Free_Media_Estonia$Country <- "Estonia"

mm_Free_Media.d = rbind(mm_Free_Media_Chile, mm_Free_Media_Estonia)
mm_Free_Media.p <- plot(mm_Free_Media.d, group = "Q10_4", vline = 0.5)
mm_Free_Media.p %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q10_4.r # It is important that there are free and politically independent media in [country]... RECODED
#####################################################
mm_Free_Media_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                             id = ~ respondent, 
                                             estimate = "mm", 
                                             by = ~Q10_4.r))

mm_Free_Media_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                               id = ~ respondent, 
                                               estimate = "mm", 
                                               by = ~Q10_4.r))


mm_Free_Media_Chile.r$Country <- "Chile"
mm_Free_Media_Estonia.r$Country <- "Estonia"

mm_Free_Media.d.r = rbind(mm_Free_Media_Chile.r, mm_Free_Media_Estonia.r)
mm_Free_Media.p.r <- plot(mm_Free_Media.d.r, group = "Q10_4.r", vline = 0.5)
mm_Free_Media.p.r %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses
# Q12_5 : The Army should take control of the state when the Government is not functioning well.
#####################################################
mm_Army_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                       id = ~ respondent, 
                                       estimate = "mm", 
                                       by = ~Q12_5))

mm_Army_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                     id = ~respondent, 
                                     estimate = "mm", 
                                     by = ~Q12_5))

mm_Army_Chile$Country <- "Chile"
mm_Army_Estonia$Country <- "Estonia"

mm_Army.d = rbind(mm_Army_Chile, mm_Army_Estonia)
mm_Army.p <- plot(mm_Army.d, group = "Q12_5", vline = 0.5)
mm_Army.p %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses
# Q12_5 : The Army should take control of the state when the Government is not functioning well... RECODED
#####################################################
mm_Army_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                         id = ~ respondent, 
                                         estimate = "mm", 
                                         by = ~Q12_5.r))

mm_Army_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                       id = ~respondent, 
                                       estimate = "mm", 
                                       by = ~Q12_5.r))

mm_Army_Chile.r$Country <- "Chile"
mm_Army_Estonia.r$Country <- "Estonia"

mm_Army.d.r = rbind(mm_Army_Chile.r, mm_Army_Estonia.r)
mm_Army.p.r <- plot(mm_Army.d.r, group = "Q12_5.r", vline = 0.5)
mm_Army.p.r %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q9_1: # Democratic satisfaction
########################################################
mm_DemSatis_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                           id = ~respondent, 
                                           estimate = "mm", 
                                           by = ~Q9_1))

mm_DemSatis_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                             id = ~respondent, 
                                             estimate = "mm", 
                                             by = ~Q9_1))


# mm_DemSatis_Chile.r$Country <- "Chile"
# mm_DemSatis_Estonia.r$Country <- "Estonia"
# mm_DemSatis.d.r = rbind(mm_DemSatis_Chile.r, mm_DemSatis_Estonia.r)
# mm_DemSatis.p.r <- plot(mm_DemSatis.d.r, group = "Q9_1", vline = 0.5)
# mm_DemSatis.p.r %+% facet_wrap(~Country)

mm_DemSatis_Chile.r$Country <- "Chile"
mm_DemSatis_Estonia.r$Country <- "Estonia"

mm_DemSatis.d = rbind(mm_DemSatis_Chile.r, mm_DemSatis_Estonia.r)
# mm_DemSatis.p <- plot(mm_DemSatis.d, group = "winners.losers", vline = 0.5)
# mm_DemSatis.p %+% facet_wrap(~Country)

p_load(ggplot2)
mm_DemSatis.p = ggplot(mm_DemSatis.d,
                       aes(factor(level),
                           y=estimate,
                           ymin=lower,
                           ymax=upper,
                           color=factor(Q9_1))) + 
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

ggsave(mm_DemSatis.p, file="Conjoint_DemSatis.pdf", width=12, height=10)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q9_1: # Democratic satisfaction (short)
# HERE
########################################################
mm_DemSatis_Chile.short.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest, 
                                                 id = ~respondent, 
                                                 estimate = "mm", 
                                                 by = ~Q9_1))

mm_DemSatis_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest, 
                                             id = ~respondent, 
                                             estimate = "mm", 
                                             by = ~Q9_1))


# mm_DemSatis_Chile.short.r$Country <- "Chile"
# mm_DemSatis_Estonia.r$Country <- "Estonia"
# mm_DemSatis.d.r = rbind(mm_DemSatis_Chile.short.r, mm_DemSatis_Estonia.r)
# mm_DemSatis.p.r <- plot(mm_DemSatis.d.r, group = "Q9_1", vline = 0.5)
# mm_DemSatis.p.r %+% facet_wrap(~Country)

mm_DemSatis_Chile.short.r$Country <- "Chile"
mm_DemSatis_Estonia.r$Country <- "Estonia"

mm_DemSatis.d = rbind(mm_DemSatis_Chile.short.r, mm_DemSatis_Estonia.r)
# mm_DemSatis.p <- plot(mm_DemSatis.d, group = "winners.losers", vline = 0.5)
# mm_DemSatis.p %+% facet_wrap(~Country)

## Recode
mm_DemSatis.d$level <- recode_factor(mm_DemSatis.d$level, 
                                     `The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government` = "The candidate OPPOSES post-electoral protests", 
                                     `The candidate SUPPORTS anti-government protest\nthat will seek to de-destabilize the current government` = "The candidate SUPPORTS post-electoral protests")


p_load(ggplot2)
mm_DemSatis.short.p = ggplot(mm_DemSatis.d,
                       aes(factor(level),
                           y=estimate,
                           ymin=lower,
                           ymax=upper,
                           color=factor(Q9_1))) + 
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

ggsave(mm_DemSatis.short.p, file="Conjoint_DemSatis_short.pdf", width=12, height=10)

# amce

f1 <- chosen ~ attr.Gender + attr.Age + attr.Protest

amce_DemSatis_Chile.short.r <- suppressWarnings(cj(conjoint.d.chile, f1, 
                                                   id = ~respondent, 
                                                   estimate = "amce"))

amce_DemSatis_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, f1, 
                                               id = ~respondent, 
                                               estimate = "amce"))


# amce_DemSatis_Chile.short.r$Country <- "Chile"
# amce_DemSatis_Estonia.r$Country <- "Estonia"
# amce_DemSatis.d.r = rbind(amce_DemSatis_Chile.short.r, amce_DemSatis_Estonia.r)
# amce_DemSatis.p.r <- plot(amce_DemSatis.d.r, group = "Q9_1", vline = 0.5)
# amce_DemSatis.p.r %+% facet_wrap(~Country)

amce_DemSatis_Chile.short.r$Country <- "Chile"
amce_DemSatis_Estonia.r$Country <- "Estonia"

amce_DemSatis.d = rbind(amce_DemSatis_Chile.short.r, amce_DemSatis_Estonia.r)
# amce_DemSatis.p <- plot(amce_DemSatis.d, group = "winners.losers", vline = 0.5)
# amce_DemSatis.p %+% facet_wrap(~Country)

## Recode
amce_DemSatis.d$level <- recode_factor(amce_DemSatis.d$level, 
                                       `The candidate OPPOSES anti-government protest\nthat will seek to de-destabilize the current government` = "The candidate OPPOSES post-electoral protests", 
                                       `The candidate SUPPORTS anti-government protest\nthat will seek to de-destabilize the current government` = "The candidate SUPPORTS post-electoral protests")

p_load(ggplot2)
amce_DemSatis.short.p = ggplot(amce_DemSatis.d, aes(y=estimate, x=level)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
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

ggsave(amce_DemSatis.short.p, file="Conjoint_DemSatis_short_AMCE.pdf", width=12, height=10)


#####################################################
# Marginal Means // Subgroup Analyses
# Q10_1 # Democracy might have problems but it's better...
#####################################################
mm_DemBetter_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Q10_1))

mm_DemBetter_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                          id = ~ respondent, 
                                          estimate = "mm", 
                                          by = ~Q10_1))

mm_DemBetter_Chile$Country <- "Chile"
mm_DemBetter_Estonia$Country <- "Estonia"

mm_DemBetter.d = rbind(mm_DemBetter_Chile, mm_DemBetter_Estonia)
mm_DemBetter.p <- plot(mm_DemBetter.d, group = "Q10_1", vline = 0.5)
mm_DemBetter.p %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q10_1.r: # Democratic satisfaction
########################################################
mm_DemBetter_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~respondent, 
                                            estimate = "mm", 
                                            by = ~Q10_1.r))

mm_DemBetter_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~respondent, 
                                              estimate = "mm", 
                                              by = ~Q10_1.r))


# mm_DemBetter_Chile.r$Country <- "Chile"
# mm_DemBetter_Estonia.r$Country <- "Estonia"
# mm_DemBetter.d.r = rbind(mm_DemBetter_Chile.r, mm_DemBetter_Estonia.r)
# mm_DemBetter.p.r <- plot(mm_DemBetter.d.r, group = "Q10_1.r", vline = 0.5)
# mm_DemBetter.p.r %+% facet_wrap(~Country)

mm_DemBetter_Chile.r$Country <- "Chile"
mm_DemBetter_Estonia.r$Country <- "Estonia"

mm_DemBetter.d = rbind(mm_DemBetter_Chile.r, mm_DemBetter_Estonia.r)
# mm_DemBetter.p <- plot(mm_DemBetter.d, group = "winners.losers", vline = 0.5)
# mm_DemBetter.p %+% facet_wrap(~Country)

p_load(ggplot2)
mm_DemBetter.p = ggplot(mm_DemBetter.d,
                        aes(factor(level),
                            y=estimate,
                            ymin=lower,
                            ymax=upper,
                            color=factor(Q10_1.r))) + 
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

ggsave(mm_DemBetter.p, file="Conjoint_DemBetter.pdf", width=12, height=10)

#####################################################
# Marginal Means // Subgroup Analyses
# Q10_1.r # Democracy might have problems but it's better... RECODED
#####################################################
mm_DemBetter_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~ respondent, 
                                              estimate = "mm", 
                                              by = ~Q10_1.r))

mm_DemBetter_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Q10_1.r))

mm_DemBetter_Chile.r$Country <- "Chile"
mm_DemBetter_Estonia.r$Country <- "Estonia"

mm_DemBetter.d.r = rbind(mm_DemBetter_Chile.r, mm_DemBetter_Estonia.r)
mm_DemBetter.p.r <- plot(mm_DemBetter.d.r, group = "Q10_1.r", vline = 0.5)
mm_DemBetter.p.r %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q10_2: Democracy is not an effective form of government...better a strong leader
########################################################
mm_StrongLeader_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                             id = ~respondent, 
                                             estimate = "mm", 
                                             by = ~Q10_2))

mm_StrongLeader_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                               id = ~respondent, 
                                               estimate = "mm", 
                                               by = ~Q10_2))


mm_StrongLeader_Chile$Country <- "Chile"
mm_StrongLeader_Estonia$Country <- "Estonia"

mm_StrongLeader.d = rbind(mm_StrongLeader_Chile, mm_StrongLeader_Estonia)
mm_StrongLeader.p <- plot(mm_StrongLeader.d, group = "Q10_2", vline = 0.5)
mm_StrongLeader.p %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q10_2: # Democracy is not an effective form of government...better a strong leader RECODED
########################################################
mm_StrongLeader_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                               id = ~respondent, 
                                               estimate = "mm", 
                                               by = ~Q10_2.r))

mm_StrongLeader_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                                 id = ~respondent, 
                                                 estimate = "mm", 
                                                 by = ~Q10_2.r))


# mm_StrongLeader_Chile.r$Country <- "Chile"
# mm_StrongLeader_Estonia.r$Country <- "Estonia"
# mm_StrongLeader.d.r = rbind(mm_StrongLeader_Chile.r, mm_StrongLeader_Estonia.r)
# mm_StrongLeader.p.r <- plot(mm_StrongLeader.d.r, group = "Q10_2.r", vline = 0.5)
# mm_StrongLeader.p.r %+% facet_wrap(~Country)

mm_StrongLeader_Chile.r$Country <- "Chile"
mm_StrongLeader_Estonia.r$Country <- "Estonia"

mm_StrongLeader.d = rbind(mm_StrongLeader_Chile.r, mm_StrongLeader_Estonia.r)
# mm_StrongLeader.p <- plot(mm_StrongLeader.d, group = "winners.losers", vline = 0.5)
# mm_StrongLeader.p %+% facet_wrap(~Country)

p_load(ggplot2)
mm_StrongLeader.p = ggplot(mm_StrongLeader.d,
                           aes(factor(level),
                               y=estimate,
                               ymin=lower,
                               ymax=upper,
                               color=factor(Q10_2.r))) + 
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

ggsave(mm_StrongLeader.p, file="Conjoint_StrongLeader.pdf", width=12, height=10)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q10_3: Civil rights that guarantee political protest should not be restricted
########################################################
mm_RightToProtest_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                               id = ~respondent, 
                                               estimate = "mm", 
                                               by = ~Q10_3))

mm_RightToProtest_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                                 id = ~respondent, 
                                                 estimate = "mm", 
                                                 by = ~Q10_3))



mm_RightToProtest_Chile$Country <- "Chile"
mm_RightToProtest_Estonia$Country <- "Estonia"

mm_RightToProtest.d = rbind(mm_RightToProtest_Chile, mm_RightToProtest_Estonia)
mm_RightToProtest.p <- plot(mm_RightToProtest.d, group = "Q10_3", vline = 0.5)
mm_RightToProtest.p %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q10_3.r: Civil rights that guarantee political protest should not be restricted... RECODED
########################################################
mm_RightToProtest_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                                 id = ~respondent, 
                                                 estimate = "mm", 
                                                 by = ~Q10_3.r))

mm_RightToProtest_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                                   id = ~respondent, 
                                                   estimate = "mm", 
                                                   by = ~Q10_3.r))



# mm_RightToProtest_Chile.r$Country <- "Chile"
# mm_RightToProtest_Estonia.r$Country <- "Estonia"
# mm_RightToProtest.d.r = rbind(mm_RightToProtest_Chile.r, mm_RightToProtest_Estonia.r)
# mm_RightToProtest.p.r <- plot(mm_RightToProtest.d.r, group = "Q10_3.r", vline = 0.5)
# mm_RightToProtest.p.r %+% facet_wrap(~Country)

mm_RightToProtest_Chile.r$Country <- "Chile"
mm_RightToProtest_Estonia.r$Country <- "Estonia"

mm_RightToProtest.d = rbind(mm_RightToProtest_Chile.r, mm_RightToProtest_Estonia.r)
#colnames(mm_RightToProtest.d)[colnames(mm_RightToProtest.d)=="Q10_3.r"] <- "RighttoProtest"
# mm_RightToProtest.p <- plot(mm_RightToProtest.d, group = "winners.losers", vline = 0.5)
# mm_RightToProtest.p %+% facet_wrap(~Country)

p_load(ggplot2)
mm_RightToProtest.p = ggplot(mm_RightToProtest.d,
                             aes(factor(level),
                                 y=estimate,
                                 ymin=lower,
                                 ymax=upper,
                                 color=factor(Q10_3.r))) + 
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

ggsave(mm_RightToProtest.p, file="Conjoint_RightToProtest.pdf", width=12, height=10)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q8_1: Thinking on a scale where one means far left and ten means far right, where do you place yourself?
########################################################
mm_LeftRight_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                          id = ~respondent, 
                                          estimate = "mm", 
                                          by = ~Q8_1))

mm_LeftRight_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~respondent, 
                                            estimate = "mm", 
                                            by = ~Q8_1))

mm_LeftRight_Chile$Country <- "Chile"
mm_LeftRight_Estonia$Country <- "Estonia"

mm_LeftRight.d = rbind(mm_LeftRight_Chile, mm_LeftRight_Estonia)
mm_LeftRight.p <- plot(mm_LeftRight.d, group = "Q8_1", vline = 0.5)
mm_LeftRight.p %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# Q8_1: Thinking on a scale where one means far left and ten means far right, where do you place yourself?... RECODED
########################################################
mm_LeftRight_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~respondent, 
                                            estimate = "mm", 
                                            by = ~Q8_1.r))

mm_LeftRight_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~respondent, 
                                              estimate = "mm", 
                                              by = ~Q8_1.r))

mm_LeftRight_Chile.r$Country <- "Chile"
mm_LeftRight_Estonia.r$Country <- "Estonia"

mm_LeftRight.d.r = rbind(mm_LeftRight_Chile.r, mm_LeftRight_Estonia.r)
mm_LeftRight.p.r <- plot(mm_LeftRight.d.r, group = "Q8_1.r", vline = 0.5)
mm_LeftRight.p.r %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_7 # The state should ensure that wages are more equal.
#####################################################

mm_Wages_Equal_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~ respondent, 
                                            estimate = "mm", 
                                            by = ~Q12_7))

mm_Wages_Equal_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~ respondent, 
                                              estimate = "mm", 
                                              by = ~Q12_7))


mm_Wages_Equal_Chile$Country <- "Chile"
mm_Wages_Equal_Estonia$Country <- "Estonia"

mm_Wages_Equal.d = rbind(mm_Wages_Equal_Chile, mm_Wages_Equal_Estonia)
mm_Wages_Equal.p <- plot(mm_Wages_Equal.d, group = "Q12_7", vline = 0.5)
mm_Wages_Equal.p %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_1 # Governments should tax the rich to help the poor
#####################################################

mm_Tax_Rich_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                         id = ~ respondent, 
                                         estimate = "mm", 
                                         by = ~Q12_1))

mm_Tax_Rich_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                           id = ~ respondent, 
                                           estimate = "mm", 
                                           by = ~Q12_1))


mm_Tax_Rich_Chile$Country <- "Chile"
mm_Tax_Rich_Estonia$Country <- "Estonia"

mm_Tax_Rich.d = rbind(mm_Tax_Rich_Chile, mm_Tax_Rich_Estonia)
mm_Tax_Rich.p <- plot(mm_Tax_Rich.d, group = "Q12_1", vline = 0.5)
mm_Tax_Rich.p %+% facet_wrap(~Country)

#####################################################
# Marginal Means // Subgroup Analyses: 
# Q12_1 # Governments should tax the rich to help the poor... RECODED
#####################################################

mm_Tax_Rich_Chile.r <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                           id = ~ respondent, 
                                           estimate = "mm", 
                                           by = ~Q12_1.r))

mm_Tax_Rich_Estonia.r <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                             id = ~ respondent, 
                                             estimate = "mm", 
                                             by = ~Q12_1.r))


mm_Tax_Rich_Chile.r$Country <- "Chile"
mm_Tax_Rich_Estonia.r$Country <- "Estonia"

mm_Tax_Rich.d.r = rbind(mm_Tax_Rich_Chile.r, mm_Tax_Rich_Estonia.r)
mm_Tax_Rich.p.r <- plot(mm_Tax_Rich.d.r, group = "Q12_1.r", vline = 0.5)
mm_Tax_Rich.p.r %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# Educ.HighLow
########################################################

mm_EducHighLow_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                            id = ~respondent, 
                                            estimate = "mm", 
                                            by = ~Educ.HighLow))

mm_EducHighLow_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                              id = ~respondent, 
                                              estimate = "mm", 
                                              by = ~Educ.HighLow))

mm_EducHighLow_Chile$Country <- "Chile"
mm_EducHighLow_Estonia$Country <- "Estonia"

mm_EducHighLow.d = rbind(mm_EducHighLow_Chile, mm_EducHighLow_Estonia)
mm_EducHighLow.p <- plot(mm_EducHighLow.d, group = "Educ.HighLow", vline = 0.5)
mm_EducHighLow.p %+% facet_wrap(~Country)

########################################################
# Marginal Means // Subgroup Analyses: 
# IncomeLowMidHigh
########################################################

mm_IncomeHighMidLow_Chile <- suppressWarnings(cj(conjoint.d.chile, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                                 id = ~respondent, 
                                                 estimate = "mm", 
                                                 by = ~IncomeLowMidHigh))

mm_IncomeHighMidLow_Estonia <- suppressWarnings(cj(conjoint.d.estonia, chosen ~ attr.Gender + attr.Age + attr.Protest + attr.Pensions, 
                                                   id = ~respondent, 
                                                   estimate = "mm", 
                                                   by = ~IncomeLowMidHigh))

# mm_IncomeHighMidLow_Chile$Country <- "Chile"
# mm_IncomeHighMidLow_Estonia$Country <- "Estonia"
# mm_IncomeHighMidLow.d = rbind(mm_IncomeHighMidLow_Chile, mm_IncomeHighMidLow_Estonia)
# mm_IncomeHighMidLow.p <- plot(mm_IncomeHighMidLow.d, group = "IncomeLowMidHigh", vline = 0.5)
# mm_IncomeHighMidLow.p %+% facet_wrap(~Country)

mm_IncomeHighMidLow_Chile$Country <- "Chile"
mm_IncomeHighMidLow_Estonia$Country <- "Estonia"

mm_IncomeHighMidLow.d = rbind(mm_IncomeHighMidLow_Chile, mm_IncomeHighMidLow_Estonia)
# mm_IncomeHighMidLow.p <- plot(mm_IncomeHighMidLow.d, group = "winners.losers", vline = 0.5)
# mm_IncomeHighMidLow.p %+% facet_wrap(~Country)

p_load(ggplot2)
mm_IncomeHighMidLow.p = ggplot(mm_IncomeHighMidLow.d,
                               aes(factor(level),
                                   y=estimate,
                                   ymin=lower,
                                   ymax=upper,
                                   color=factor(IncomeLowMidHigh))) + 
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

ggsave(mm_IncomeHighMidLow.p, file="Conjoint_IncomeHighMidLow.pdf", width=12, height=10)


############################################################################## END RUN













################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
writeLines(paste("Citizen's support for democracy is central for democratic stability, yet recent research has begun to question the depth of this commitment in both new and established democracies. Contrary to most research that concentrates on potential breaches of democratic values by the 'winners,' we turn our attention to the 'losers.' In particular, we seek to understand if individuals who sided with the losing candidate are more open to supporting anti-systemic actions against the government, and whether their stance is influenced by their country's regime type. To do this, we carried out a novel survey experiment in two new democracies, Estonia and Chile (n=", chile.sample.size, ")", ", probing into the willingness of these 'losers' to tolerate transgressions against democratic principles", sep=""), fileConn)
close(fileConn)
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




##########
# VDEM
##########

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import data
vdem.d <- readRDS("/Users/hectorbahamonde/research/democratic_backsliding/data/vdem.rds")

# Keep Chile and Estonia
vdem.d <- vdem.d[which(vdem.d$country_name=='Chile'  | vdem.d$country_name=='Estonia'),]

# restrict series 1990 to today
vdem.d <- vdem.d[which(vdem.d$year>=1990),]

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

p1 = ggplot(vdem.d, aes(year, v2elfrfair, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Election free and fair") + theme(legend.position = "none", aspect.ratio=1)
p2 = ggplot(vdem.d, aes(year, v2elaccept, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Election losers accept results") + theme(legend.position = "none", aspect.ratio=1)
p3 = ggplot(vdem.d, aes(year, v2psbars, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Barriers to parties") + theme(legend.position = "none", aspect.ratio=1)
p4 = ggplot(vdem.d, aes(year, v2psparban, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Party ban") + theme(legend.position = "none", aspect.ratio=1)
p5 = ggplot(vdem.d, aes(year, v2exrescon, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Executive respects constitution") + theme(legend.position = "none", aspect.ratio=1)
p6 = ggplot(vdem.d, aes(year, v2regimpgroup, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Regime most important support group") + theme(legend.position = "none", aspect.ratio=1)
p7 = ggplot(vdem.d, aes(year, v2regsupgroupssize, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Regime support groups size") + theme(legend.position = "none", aspect.ratio=1)
p8 = ggplot(vdem.d, aes(year, v2regsuploc, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Regime support location") + theme(legend.position = "none", aspect.ratio=1)
p9 = ggplot(vdem.d, aes(year, v2regproreg, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Strongest pro-regime preferences") + theme(legend.position = "none", aspect.ratio=1)
p10 = ggplot(vdem.d, aes(year, v2regantireg, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Strongest anti-regime preferences") + theme(legend.position = "none", aspect.ratio=1)
p11 = ggplot(vdem.d, aes(year, v2regpower, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Most powerful group in affecting regime duration and change") + theme(legend.position = "none", aspect.ratio=1)
p12 = ggplot(vdem.d, aes(year, v2dlcommon, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Common good") + theme(legend.position = "none", aspect.ratio=1)
p13 = ggplot(vdem.d, aes(year, v2dlcountr, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Respect counterarguments") + theme(legend.position = "none", aspect.ratio=1)
p14 = ggplot(vdem.d, aes(year, v2dlengage, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Engaged society") + theme(legend.position = "none", aspect.ratio=1)
p15 = ggplot(vdem.d, aes(year, v2dlencmps, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Particularistic or public goods") + theme(legend.position = "none", aspect.ratio=1)
p16 = ggplot(vdem.d, aes(year, v2jupoatck, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Government attacks on judiciary") + theme(legend.position = "bottom", aspect.ratio=1)
#
p17 = ggplot(vdem.d, aes(year, v2clrspct, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Rigorous and impartial public administration") + theme(legend.position = "none", aspect.ratio=1)
p18 = ggplot(vdem.d, aes(year, v2clacjust, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Social class equality in respect for civil liberty") + theme(legend.position = "none", aspect.ratio=1)
p19 = ggplot(vdem.d, aes(year, v2clacfree, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Freedom of academic and cultural expression") + theme(legend.position = "none", aspect.ratio=1)
p20 = ggplot(vdem.d, aes(year, v2clstown, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="State ownership of economy") + theme(legend.position = "none", aspect.ratio=1)
p21 = ggplot(vdem.d, aes(year, v2clprptym, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Property rights for men") + theme(legend.position = "none", aspect.ratio=1)
p22 = ggplot(vdem.d, aes(year, v2clprptyw, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Property rights for women") + theme(legend.position = "none", aspect.ratio=1)
p23 = ggplot(vdem.d, aes(year, v2stcritapparm, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Criteria for appointment decisions in the armed forces") + theme(legend.position = "none", aspect.ratio=1)
p24 = ggplot(vdem.d, aes(year, v2strenarm, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Remuneration in the Armed Forces") + theme(legend.position = "none", aspect.ratio=1)
p25 = ggplot(vdem.d, aes(year, v2mecenefm, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Government censorship effort — Media") + theme(legend.position = "none", aspect.ratio=1)
p26 = ggplot(vdem.d, aes(year, v2mecenefi, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Internet censorship effort")  + theme(legend.position = "none", aspect.ratio=1)
p27 = ggplot(vdem.d, aes(year, v2mecrit, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Print/broadcast media critical")  + theme(legend.position = "none", aspect.ratio=1)
p28 = ggplot(vdem.d, aes(year, v2merange, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Print/broadcast media perspectives") + theme(legend.position = "none", aspect.ratio=1)
p29 = ggplot(vdem.d, aes(year, v2meharjrn, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Harassment of journalists") + theme(legend.position = "none", aspect.ratio=1)
p30 = ggplot(vdem.d, aes(year, v2meslfcen, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Media self-censorship") + theme(legend.position = "none", aspect.ratio=1)
p31 = ggplot(vdem.d, aes(year, v2mebias, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Media bias") + theme(legend.position = "none", aspect.ratio=1)
p32 = ggplot(vdem.d, aes(year, v2pepwrses, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Power distributed by socioeconomic position") + theme(legend.position = "bottom", aspect.ratio=1)
#
p33 = ggplot(vdem.d, aes(year, v2pepwrsoc, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Power distributed by social group") + theme(legend.position = "none", aspect.ratio=1)
p34 = ggplot(vdem.d, aes(year, v2peapsecon, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Access to public services distributed by socio-economic position") + theme(legend.position = "none", aspect.ratio=1)
p35 = ggplot(vdem.d, aes(year, v2peasjsoecon, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Access to state jobs by socio-economic position") + theme(legend.position = "none", aspect.ratio=1)
p36 = ggplot(vdem.d, aes(year, v2clpolcl, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Political group equality in respect for civil liberties") + theme(legend.position = "none", aspect.ratio=1)
p37 = ggplot(vdem.d, aes(year, v2cacamps, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Political polarization") + theme(legend.position = "none", aspect.ratio=1)
p38 = ggplot(vdem.d, aes(year, v2caviol, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Political violence") + theme(legend.position = "none", aspect.ratio=1)
p39 = ggplot(vdem.d, aes(year, v2caassemb, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Freedom of peaceful assembly") + theme(legend.position = "none", aspect.ratio=1)
p40 = ggplot(vdem.d, aes(year, v2cagenmob, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Mass mobilization") + theme(legend.position = "none", aspect.ratio=1)
p41 = ggplot(vdem.d, aes(year, v2caconmob, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Mass mobilization concentration") + theme(legend.position = "none", aspect.ratio=1)
p42 = ggplot(vdem.d, aes(year, v2cademmob, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Mobilization for democracy") + theme(legend.position = "none", aspect.ratio=1)
p43 = ggplot(vdem.d, aes(year, v2caautmob, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Mobilization for autocracy") + theme(legend.position = "none", aspect.ratio=1)
p44 = ggplot(vdem.d, aes(year, v2catrauni, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Engagement in independent trade unions") + theme(legend.position = "none", aspect.ratio=1)
p45 = ggplot(vdem.d, aes(year, v2capolit, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Engagement in independent political associations") + theme(legend.position = "none", aspect.ratio=1)
p46 = ggplot(vdem.d, aes(year, v2canonpol, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Engagement in independent non-political associations") + theme(legend.position = "none", aspect.ratio=1)
p47 = ggplot(vdem.d, aes(year, v2cauni, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Existence of universities") + theme(legend.position = "none", aspect.ratio=1)
p48 = ggplot(vdem.d, aes(year, v2caprotac, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Constitutional protection for academic freedom") + theme(legend.position = "bottom", aspect.ratio=1)
#
p49 = ggplot(vdem.d, aes(year, v2cafres, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Freedom to research and teach") + theme(legend.position = "none", aspect.ratio=1)
p50 = ggplot(vdem.d, aes(year, v2cainsaut, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Institutional autonomy") + theme(legend.position = "none", aspect.ratio=1)
p51 = ggplot(vdem.d, aes(year, v2cacritic, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Academics as critics") + theme(legend.position = "none", aspect.ratio=1)
p52 = ggplot(vdem.d, aes(year, v3partyid, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Party identification") + theme(legend.position = "none", aspect.ratio=1)
p53 = ggplot(vdem.d, aes(year, v3lgbudglo, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Lower chamber budget") + theme(legend.position = "none", aspect.ratio=1)
p54 = ggplot(vdem.d, aes(year, v2xnp_client, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Clientelism Index") + theme(legend.position = "none", aspect.ratio=1)
p55 = ggplot(vdem.d, aes(year, v2x_rule, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Rule of law index") + theme(legend.position = "none", aspect.ratio=1)
p56 = ggplot(vdem.d, aes(year, v2smgovdom, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Government dissemination of false information domestic") + theme(legend.position = "none", aspect.ratio=1)
p57 = ggplot(vdem.d, aes(year, v2smgovab, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Government dissemination of false information abroad") + theme(legend.position = "none", aspect.ratio=1)
p58 = ggplot(vdem.d, aes(year, v2smgovsmmon, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Government social media monitoring") + theme(legend.position = "none", aspect.ratio=1)
p59 = ggplot(vdem.d, aes(year, v2smgovsmcenprc, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Government social media censorship in practice") + theme(legend.position = "none", aspect.ratio=1)
p60 = ggplot(vdem.d, aes(year, v2smregcap, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Government capacity to regulate online content") + theme(legend.position = "none", aspect.ratio=1)
p61 = ggplot(vdem.d, aes(year, v2smarrest, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Arrests for political content") + theme(legend.position = "none", aspect.ratio=1)
p62 = ggplot(vdem.d, aes(year, v2smpolsoc, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Polarization of society") + theme(legend.position = "none", aspect.ratio=1)
p63 = ggplot(vdem.d, aes(year, v2smpolhate, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Political parties hate speech") + theme(legend.position = "none", aspect.ratio=1)
p64 = ggplot(vdem.d, aes(year, e_polcomp, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Political competition") + theme(legend.position = "none", aspect.ratio=1)
p65 = ggplot(vdem.d, aes(year, e_ti_cpi, col=country_name)) + geom_smooth() + theme_light() + 
  labs(y="Corruption perception index") + theme(legend.position = "bottom", aspect.ratio=1)

p_load(cowplot)
cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16, align = "hv",axis = "b", ncol = 4) 
cowplot::plot_grid(p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, align = "hv",axis = "b", ncol = 4) 
cowplot::plot_grid(p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, align = "hv",axis = "b", ncol = 4) 
cowplot::plot_grid(p49, p50, p51, p52, p53, p55, p56, p57, p58, p59, p60, p61, p62, p63, p64, p65, align = "hv",axis = "b", ncol = 4) 

# selection of 2 dimensions for paper
p_load(ggplot2)

p1 = ggplot(vdem.d, aes(year, v2elfrfair, col=country_name)) + geom_smooth(se=F) + theme_light() + labs(y="Election free and fair") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="none",
  aspect.ratio=1,
  axis.text.y = element_text(size=14), 
  axis.text.x = element_text(size=14), 
  axis.title.y = element_text(size=14), 
  axis.title.x = element_text(size=14), 
  legend.text=element_text(size=14), 
  legend.title=element_text(size=14),
  plot.title = element_text(size=14),
  strip.text.x = element_text(size=14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")

p1.b = ggplot(vdem.d, aes(year, v2elfrfair, col=country_name)) + geom_smooth() + theme_light() + labs(y="Election free and fair") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="bottom",
  aspect.ratio=1,
  axis.text.y = element_text(size=35), 
  axis.text.x = element_text(size=35), 
  axis.title.y = element_text(size=35), 
  axis.title.x = element_text(size=35), 
  legend.text=element_text(size=35), 
  legend.title=element_text(size=35),
  plot.title = element_text(size=35),
  strip.text.x = element_text(size=35)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")

p2 = ggplot(vdem.d, aes(year, v2elaccept, col=country_name)) + geom_smooth(se=F) + theme_light() + labs(y="Election losers accept results") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="bottom",
  aspect.ratio=1,
  axis.text.y = element_text(size=14), 
  axis.text.x = element_text(size=14), 
  axis.title.y = element_text(size=14), 
  axis.title.x = element_text(size=14), 
  legend.text=element_text(size=14), 
  legend.title=element_text(size=14),
  plot.title = element_text(size=14),
  strip.text.x = element_text(size=14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")

p2.b = ggplot(vdem.d, aes(year, v2elaccept, col=country_name)) + geom_smooth(se=F) + theme_light() + labs(y="Election losers accept results") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="bottom",
  aspect.ratio=1,
  axis.text.y = element_text(size=35), 
  axis.text.x = element_text(size=35), 
  axis.title.y = element_text(size=35), 
  axis.title.x = element_text(size=35), 
  legend.text=element_text(size=35), 
  legend.title=element_text(size=35),
  plot.title = element_text(size=35),
  strip.text.x = element_text(size=35)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")


p5 = ggplot(vdem.d, aes(year, v2exrescon, col=country_name)) + geom_smooth() + theme_light() + labs(y="Executive respects constitution") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="none",
  aspect.ratio=1,
  axis.text.y = element_text(size=14), 
  axis.text.x = element_text(size=14), 
  axis.title.y = element_text(size=14), 
  axis.title.x = element_text(size=14), 
  legend.text=element_text(size=14), 
  legend.title=element_text(size=14),
  plot.title = element_text(size=14),
  strip.text.x = element_text(size=14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")

p13 = ggplot(vdem.d, aes(year, v2dlcountr, col=country_name)) + geom_smooth() + theme_light() + labs(y="Respect counterarguments") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="none",
  aspect.ratio=1,
  axis.text.y = element_text(size=14), 
  axis.text.x = element_text(size=14), 
  axis.title.y = element_text(size=14), 
  axis.title.x = element_text(size=14), 
  legend.text=element_text(size=14), 
  legend.title=element_text(size=14),
  plot.title = element_text(size=14),
  strip.text.x = element_text(size=14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")

p39 = ggplot(vdem.d, aes(year, v2caassemb, col=country_name)) + geom_smooth() + theme_light() + labs(y="Freedom of peaceful assembly") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="bottom",
  aspect.ratio=1,
  axis.text.y = element_text(size=14), 
  axis.text.x = element_text(size=14), 
  axis.title.y = element_text(size=14), 
  axis.title.x = element_text(size=14), 
  legend.text=element_text(size=14), 
  legend.title=element_text(size=14),
  plot.title = element_text(size=14),
  strip.text.x = element_text(size=14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")

p39.b = ggplot(vdem.d, aes(year, v2caassemb, col=country_name)) + geom_smooth() + theme_light() + labs(y="Freedom of peaceful assembly") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="bottom",
  aspect.ratio=1,
  axis.text.y = element_text(size=35), 
  axis.text.x = element_text(size=35), 
  axis.title.y = element_text(size=35), 
  axis.title.x = element_text(size=35), 
  legend.text=element_text(size=35), 
  legend.title=element_text(size=35),
  plot.title = element_text(size=35),
  strip.text.x = element_text(size=35)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "")

p43 = ggplot(vdem.d, aes(year, v2caautmob, col=country_name)) + geom_smooth(se=F) + theme_light() + labs(y="Mobilization for autocracy") + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="none",
  aspect.ratio=1,
  axis.text.y = element_text(size=14), 
  axis.text.x = element_text(size=14), 
  axis.title.y = element_text(size=14), 
  axis.title.x = element_text(size=14), 
  legend.text=element_text(size=14), 
  legend.title=element_text(size=14),
  plot.title = element_text(size=14),
  strip.text.x = element_text(size=14)) +
  guides(colour=guide_legend(title="")) + 
  labs(x = "",  caption = "Source: VDEM Data V12 2022 (Coppedge et al., 2022).")

vdem.chile.estonia.p = cowplot::plot_grid(p1,p2,p43, align = "hv",axis = "b", ncol = 3) 
ggsave(vdem.chile.estonia.p, file="VD_Chile_Estonia.pdf", width=12, height=6)

# for presentation, below graphs
ggsave(p1.b, file="VD_Chile_Estonia_ElecFree.pdf", width=10, height=10)
ggsave(p2.b, file="VD_Chile_Estonia_LosersConsent.pdf", width=10, height=10)
ggsave(p39.b, file="VD_Chile_Estonia_FreedomAssam.pdf", width=10, height=10)


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


