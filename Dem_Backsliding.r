cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

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

# Democratization variable (socialized in democracy: YES/NO)
wvs.d$soc.in.dem = ifelse(
  wvs.d$COW_ALPHA=='CHL', ifelse(wvs.d$X002>=1989,0,1),ifelse(wvs.d$COW_ALPHA=='EST',ifelse(wvs.d$X002>=1991,0,1),NA))

## Notes: Chile did have dictatorships even before Pinochet (Ibanez del Campo, etc.). Thus, older folks did NOT strictly get socialized during proper democracy. 
## They did get socialized before a *major* dictatorship with a repressive apparatus and a clear political ideology. Prior dictators were more like "caudillos" WITHOUT a clear ideological agenda (in general).

# Pop-Eleches and Tucker use a continuous measure of number of years the individual has lived under the communist regime
wvs.d$years.lived.in.dictatorship = ifelse(
  wvs.d$COW_ALPHA=='CHL', wvs.d$X002-1989, ifelse(wvs.d$COW_ALPHA=='EST',wvs.d$X002-1991,NA)
)
  
# free up memory
gc()

# re-order dataset
wvs.d <- wvs.d %>% 
  dplyr::select(c("S020", "COW_ALPHA", "X002", "X003", "soc.in.dem", "years.lived.in.dictatorship", "E235"), everything()
                )

# Not so many obs of folks "socialized in dictatorship"
table(wvs.d[wvs.d$COW_ALPHA=='CHL',]$soc.in.dem)
table(wvs.d[wvs.d$COW_ALPHA=='EST',]$soc.in.dem)

# Not so many younger folks (especially for Estonia)
max(wvs.d[wvs.d$COW_ALPHA=="CHL",]$X002)
max(wvs.d[wvs.d$COW_ALPHA=="EST",]$X002)

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