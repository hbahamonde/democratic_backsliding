cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/democratic_backsliding/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

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
