
# install.packages(c('tidyverse','fixest', 'survey', 'modelsummary', 'patchwork', 'scales'))
rm(list=ls())
library(tidyverse)
library(splines)
library(fixest)
library(survey)
library(modelsummary)
library(patchwork)
library(scales)
library(arrow)
library(dplyr)
library(gtsummary)



# Load Data of concatenated labor force surveys (auxiliary/get_eec_from_2010_to_2015.R) -------------------

eec_outpath <- paste0(getwd(),"/Data/eec_did/eec_did.parquet")

data_raw <- read_parquet(eec_outpath)

# Select population of interest -----------------

# Wage earners, age 15-75, years 2010-2015, non-missing absence information,
# not self-employed, not business owners, 
# for now not restricting public sector to central civil service
# sanity check = at least 1 active pop in household

data_raw <- data_raw %>% 
  mutate(is_salaried = case_when(STC==2 & ANNEE <2013 ~TRUE, # has a job and is employee of firm (change of code between years)
                                 STC==3 & ANNEE >= 2013 ~TRUE, 
                                 TRUE ~FALSE),
         has_job = (TRAREF == 1) | (TRAREF==2 & PASTRA == 1)) %>% 
  mutate(valid_observation = (AGE >=15 & AGE <= 75) &(is_salaried) & (has_job) & (NBACTOP>0)) %>% 
  group_by(id_panel)%>%
  filter(all(valid_observation==TRUE)) %>%
  ungroup()


table(data_raw$ANNEE)

  # Create variable for survey wave

pop_interest<- data_raw %>% 
  arrange(id_panel, ANNEE, TRIM) %>% 
  group_by(id_panel) %>% 
  mutate(wave = row_number()) %>% 
  ungroup()


summary(pop_interest$wave)

  # Clean variables -------------------------------------------------
pop_interest <- pop_interest%>%mutate( 
  # Time
  quarter = factor(TRIM),
  year = factor(ANNEE),
  
  # Outcome: sick leave
  sick_leave = coalesce(if_else((RABS == 2)|(EMPABS==1), 1, 0), 0), # extensive margin
  sick_leave_spell = if_else(sick_leave==1, pmax(coalesce(EMPANH, 0), coalesce(RABSP, 0)), NA), # intensive margin
  sick_leave_short_term = coalesce(sick_leave_spell<=7, 0),
  sick_leave_long_term = coalesce(sick_leave_spell>7, 0),
  sick_leave_short_term_bins = cut(sick_leave_spell, breaks=c(0, 2, 7), include.lowest=T, right=F),
  sick_leave_long_term_bins =  cut(sick_leave_spell, breaks=c(7, 30*3, Inf), include.lowest=T, right=F), # 3m cut
  
  # Secondary outcome (robustness check):
  absence = if_else(TRAREF==2 & PASTRA==1, 1, 0), #extensive margin

  # Variable of interest: Migrant
  migrant = if_else((NFR%in%c(2,3)) & (LNAIS == 2), 1, 0), # insee definition = born foreigner in foreign contry (can become french)
  foreign_migrant = if_else((NFR==3) & (LNAIS == 2), 1, 0), # foreigner (subcategory of migrants)
  migrant_factor = factor(migrant, labels = c('Native', 'Migrant')),
  
  # see \\casd-src6\Projets\ENSAE05\Data\EE_EEC_2010\Documentation\Codif_pays.xls
  origin_migrant = case_when(
    NATIO %in% c("001", "031",
                 "033",
                 "047",
                 "372",
                 "377",
                 '458',
                 "462",
                 "468",
                 "496",
                 "811",
                 "830"
                 )~ NA_character_, # French
    substr(NATIO, 1, 1) %in% c("0") ~'Europe',
    
    NATIO %in% c("204", "208", "210","212","216", "220") ~ 'Maghreb', 
    
    substr(NATIO, 1, 1) %in% c("2", "3") ~ 'Subsaharian Africa',

    substr(NATIO, 1, 1) %in% c("6", "7") ~ 'Middle East and Asia', 
    
    TRUE ~ 'Other'
  ),
  
  origin_migrant = if_else(migrant==1, origin_migrant, NA_character_),
  
  
  # Treatment: 
  public_sector=if_else((CHPUB%in%c(1,2,5) & ANNEE<2013)|(CHPUB%in%c(3,4,2) & ANNEE>=2013),
                       1, 0), 
  
  # Treatment additional variables: 
  public_sector_factor = factor(public_sector, labels = c('Private Sector', 'Public Sector')),
  
  central_civil_service=if_else((CHPUB==1 & ANNEE<2013)|(CHPUB==3 & ANNEE>=2013),
                       1, 0), 
  
  territorial_civil_service=if_else((CHPUB==2 & ANNEE<2013)|(CHPUB==4 & ANNEE>=2013),
                               1, 0),
  
  public_company_civil_service=if_else((CHPUB==5 & ANNEE<2013)|(CHPUB==2 & ANNEE>=2013),
                                   1, 0),
  
  triple_interaction_group = case_when(
    public_sector==1 & migrant == 0 ~ "Native - Public",
    public_sector==0 & migrant == 0 ~ "Native - Private", 
    public_sector==1 & migrant == 1 ~ "Migrant - Public", 
    public_sector==0 & migrant == 1 ~ "Migrant - Private", 
    TRUE ~ NA_character_
  ), 
  triple_interaction_group = factor(triple_interaction_group, 
                                    labels = c("Migrant - Private","Migrant - Public", 
                                               "Native - Private", "Native - Public")),
  
  
  
  # Controls
  ## Sociodemographic
  female = if_else(SEXE==2, 1, 0),
  gender = factor(female,labels=c('Male', 'Female')),
  age = AGE, 
  age_group = cut(AGE, breaks=c(15,25,35,45,55,65, Inf), include.lowest=T, right=F),
  age_group3 = cut(AGE, breaks=c(15,30,50, Inf), include.lowest=T, right=F),
  single_parent_family = if_else(TYPMEN5==2, 1, 0), 
  kids = if_else(NBENFIND > 0, 1, 0),
  nb_kids = if_else(NBENFIND>0,NBENFIND, NA),
  higher_education = if_else(DDIPL %in% c(1,3), 1, 0),
  no_diploma = if_else(DDIPL==7, 1, 0),
  education = factor(DDIPL, labels = c('BAC2','BAC12', 'BAC', 'CAPBEP',
                                      'middle_school', 'primary_or_no_diploma')),
  single_income_household = if_else(NBACTOP==1, 1, 0), #nb active pop in household =1
  
  ## Health
  disabled = if_else(ADMHAND==1, 1, 0),
  disabled_procedure = if_else(ADMHAND==2, 1, 0),
  general_health = if_else(SANTGEN<6, 6-SANTGEN, NA),
  chronic_disease = if_else(CHRON==1, 1, 0),
  
  ## Form of contract, tenure, wage, number of jobs
  multiple_jobs = if_else(NBTEMP %in% c(2,3), 1, 0), 
  hours_worked_usual = round(HHC), 
  parttime = if_else(TPP == 2, 1, 0), 
  short_term=if_else(STATUTR>1 & STATUTR<5, 1, 0),
  log_wage=if_else(salred>0, log(salred),0),
  tenure_factor=factor(ANCENTR4, labels=c('less_1_year', '1_5_y','5_10_y', 'more_10_y')),
  tenure_factor_pretty = fct_recode(tenure_factor, 
                                    'Less than one year' = 'less_1_year',
                                      '1-5 years'='1_5_y' , 
                                      '5-10 years'= '5_10_y' ,
                                      'More than 10 years'= 'more_10_y'),
  same_firm_before=if_else(DEJTRA==1, 1, 0),
  some_management = if_else(ENCADR==1, 1, 0),
  ## History of unemployment
  
  #to fill
  
  ## Occupation
  pcs = factor(CSTOTR, labels=c('Not_Coded', 'Agriculture', 'Business_owner','Executive_High_Prof',
                                'Intermediate', 'Clerical_service', 'Manual_Worker', 'Not_Coded')),
  pcs_pretty = fct_recode(pcs, 'Not Coded' = 'Not_Coded', 
                         'Agriculture' = 'Agriculture',
                         'Business owner' = 'Business_owner',
                         'Executives and Intellectual Occupations' = 'Executive_High_Prof',
                         'Intermediate Occupations' = 'Intermediate',
                         'Clerical_service' = 'Clerical_service',
                         'Manual Worker' = 'Manual_Worker'),
  # unskilled = if_else((CSTOTR %in% c(52, 62)) & (APCS1Q_Y %notin% c(NA, '00')), 1, 0),
  
  # Characteristics of migrants
  ## Administrative constraints
  year_since_arrival=ANNEE-NRESID,
  residence = cut(year_since_arrival, breaks=c(0, 5, 10, Inf), include.lowest=T, right=F),
  residence_more_than_five = case_when(year_since_arrival>=5 ~1, 
                                       year_since_arrival<5 ~0, 
                                       TRUE ~NA),
  
  
  ## Filters for identification:
  
  same_sector_before = case_when(public_sector &
                                   (ECHPUB%in%c(1,2,5) & ANNEE<2013)|
                                   (ECHPUB%in%c(3,4,2) & ANNEE>=2013) ~1,
                                 (!public_sector) &
                                   (ECHPUB==6 & ANNEE<2013)|
                                   (ECHPUB==1 & ANNEE>=2013) ~1, 
                                 TRUE ~ 0),
  
  
  work_in_alsace_moselle = if_else(DEPETA %in% c("68", "67", "57"), 1, 0)
  
  
  
  

  
  )




# Filter to get right sample --------------------------


# 1. Shouldn't work in Alsace Moselle (old region = 3 départements which are
# Haut Rhin, Bas Rhin and Moselle) because specific Social Security regime. 


pop_interest <- pop_interest %>% group_by(id_panel) %>%
  filter(all(work_in_alsace_moselle==FALSE, na.rm = T)) %>%
  ungroup()



# 2. Shouldn't switch states (from public to private) during their interrogation period. 

pop_interest <- pop_interest %>% 
  group_by(id_panel) %>%
  filter(n_distinct(public_sector)==1)%>%
  ungroup()



# Select relevant variables for balance table -----------------------------

treatments <- c('migrant', 'foreign_migrant', 'public_sector', "central_civil_service")

treatments_hetero <- c("territorial_civil_service",
                       "public_company_civil_service")

outcomes<-c("sick_leave", "sick_leave_spell", "sick_leave_short_term_spell",
            "sick_leave_long_term_spell",
            "sick_leave_short_term", "sick_leave_long_term", "absence",
            "sick_leave_short_term_bins", "sick_leave_long_term_bins")

controls <- c('year','quarter', 'age', 'age_group3','female', 'single_parent_family', 'kids','nb_kids', 'higher_education',
              'no_diploma','education', 'single_income_household', 'disabled', 'disabled_procedure', 
              'general_health', 'chronic_disease', 'multiple_jobs','hours_worked_usual',
              'parttime', 'short_term', 'log_wage', 'tenure_factor', 'same_firm_before', 'some_management','pcs')

controls <- c(controls, c('tenure_factor_pretty', 'pcs_pretty')) # for balancing tables
  
controls_migrants <- c('origin_migrant', 'residence', 'residence_more_than_five')

rm(eec_outpath, data_raw)











  