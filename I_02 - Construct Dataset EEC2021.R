rm(list=ls())
library(dplyr)
library(here)
library(lubridate)
library(plm)
library(haven)
library(arrow)
library(fixest)
library(ggplot2)
library(knitr)
library(kableExtra)
library(srvyr)
library(purrr)
library(survey)
library(fastDummies)
library(modelsummary)

setwd(dir = "C:/Users/Public/Documents/Nadia_Haytham/")
eec <- read_parquet("Data/eec_2021.parquet") # constructed in script auxiliary/get_eec_2021.R
`%notin%` = Negate(`%in%`)


# Enquête Emploi 2021  -----------------------------------------------

# We use EEC 2021 because available questions on french proficiency, 
# year of arrival in France, reason of arrival (ad hoc module on migrants)


# Check number of occurences of an individual in the dataset because interrogated multiple times

table(eec%>%group_by(id_panel)%>%summarise(n_quarters=n())%>%ungroup()%>%pull(n_quarters)) # up to 4 quarters per individual

table(eec%>%group_by(id_panel)%>%summarise(n_weights=n_distinct(EXTRI))%>%ungroup()%>%pull(n_weights)) # composition != n_quarters



# 1. Mapping codes to labels in EEC ----------------------------------------
# - use CASD online doc or "\\casd-src6\Projets\ENSAE05\Data\EE_EEC_2021\Documentation\EEC 2021 _ Dictionnaire des codes _ Base Z_2024_10_04.pdf"

eec_model<-eec%>%mutate(across(
    .cols = -c('DEPCOM', 'PCSL_Y', 'id_panel', 'id_pooled', 'SIRETRF'), #depcom and PCS logement
    .fns = ~ as.numeric(.)
  ))
  
eec_model <- eec_model %>% 
    mutate(is_salaried = (STC == 3),
           not_in_agriculture = (APCS1_Y!=2),
           has_job = (TRAREF == 1) | (TRAREF==2 & PASTRA == 1),
           unemployed_jobseeker = (ACTEU ==2 & DISPONE==1)) %>% 
    filter(AGE >=15 & AGE <= 75)%>%
    filter((is_salaried) & (has_job)) # employed and unemployed seeking a job
  
  
  # Rename variables like in I_02
  
eec_model <- eec_model%>%
    mutate(
      
      # Time
      week_ref = ymd(DATDEB),
      quarter = factor(TRIM),
      
      # Outcome: sick leave
      sick_leave = coalesce(if_else(RABS == 2, 1, 0), 0), # extensive margin
      sick_leave_spell = ABSDUR_MAL,
      sick_leave_spell_more_than_3_months = if_else(sick_leave==1 & DURABSEMP == 2, 1, if_else(sick_leave==1, 0, NA)),
      sick_leave_spell_less_than_3_months = if_else(sick_leave==1 & DURABSEMP == 1, 1, if_else(sick_leave==1, 0, NA)),
      
      
      # Secondary outcome (robustness check):
      absence = if_else(ABSDUR_TOT>0, 1, 0), #extensive margin
      absence_days = ABSDUR_TOT, # intensive margin
      
      # Treatment: Migrant
      migrant = if_else((NATIO_Y%in%c(3,4)) & (LNAIS == 2), 1, 0), # insee definition = born foreigner in foreign contry
      migrant_factor = factor(migrant, labels = c('Native', 'Migrant')),
      origin_migrant = case_when(PAYSNAISR2 == 20 ~ 'Maghreb', 
                                 PAYSNAISR2 == 21 ~ 'Subsaharian Africa',
                                 PAYSNAISR2 == 30 ~ 'Middle East', 
                                 PAYSNAISR2 %in%c(40,41) ~ 'EU27',
                                 PAYSNAISR2 == 42 ~ 'Rest of Europe',
                                 PAYSNAISR2 == 31 ~ 'Asia', 
                                 PAYSNAISR2 %in%c(50,60,99) ~ 'Other',
                                 TRUE ~ NA
      ),
      
      # Controls
      ## Sociodemographic
      female = if_else(SEXE==2, 1, 0),
      gender = factor(female,labels=c('Male', 'Female')),
      age = AGE, 
      age_group = cut(AGE, breaks=c(15,25,35,45,55,65, Inf), include.lowest=TRUE),
      age_group3 = cut(AGE, breaks=c(15,30,50, Inf), include.lowest=TRUE),
      age_group3 = relevel(age_group3, ref="(30,50]"),
      single_parent_family = if_else(TYPMEN5==2, 1, 0), 
      kids = if_else(NBENFIND_LOGCHAMP > 0, 1, 0),
      nb_kids = NBENFIND_LOGCHAMP,
      higher_education = if_else(TYPDIP == 2, 1, 0),
      no_diploma = if_else(TYPDIP==3, 1, 0),
      education = factor(DIP7, labels = c('BAC5', 'BAC34', 'BAC2', 'BAC', 'CAPBEP',
                                          'middle_school', 'primary_or_no_diploma', NA)),
      single_income_household = if_else(PCSL_Y %in% c('II-B', 'III-B', 'IV-A',' VI-A', 'VI-B', 'VII-A'), 1, 0),
      
      ## Health
      disabled = if_else(ADMHANDR==1, 1, 0),
      disabled_procedure = if_else(ADMHANDR==2, 1, 0),
      general_health = if_else(SANTGEN_Y<6, 6-SANTGEN_Y, NA),
      chronic_disease = if_else(CHRON_Y==1, 1, 0),
      
      ## Form of contract, tenure, wage, number of jobs
      active_remunerated = if_else(ACTOPREM==1, 1, 0),
      multiple_jobs = if_else(NBEMP %in% c(2,3), 1, 0), 
      hours_worked = HEFFTOT, 
      hours_worked_usual = HHABTOT, 
      hours_worked_usual_relative_to_35 = hours_worked_usual - 35,
      paid_overtime = if_else(HSREFCOMP<3, 1, 0),
      parttime = if_else(TPPRED == 2, 1, 0), 
      short_term=if_else(SALTYP>2 & SALTYP<9, 1, 0),
      public_sector=if_else((CHPUB%in%c(3,4,2)),
                            1, 0), 
      log_wage=if_else(SALRED_Y>0, log(SALRED_Y),0),
      tenure=ANCEMPLA, 
      tenure_factor=factor(ANCEMPL4, labels=c('less_1_year', '1_5_y','5_10_y', 'more_10_y',NA)),
      same_firm_before=if_else(DEJTRA_Y==1, 1, 0),
      some_management = if_else(ENCADR<3, 1, 0),
      DEPCOM_employer = DEPETRF,
      work_in_alsace_moselle = if_else(DEPETRF %in% c("68", "67", "57"), 1, 0),
      
      
      
      ## History of unemployment
      
      
      ## Firm indicator
      siret = SIRETRF,
      
      ## Occupation
      pcs = factor(APCS1_Y, labels=c('Not_Coded', 'Agriculture', 'Business_owner','Executive_High_Prof',
                                     'Intermediate', 'Clerical_service', 'Manual_Worker')),
      unskilled = ifelse((APCS1Q_Y %in% c(52, 62)) & (APCS1Q_Y %notin% c(NA, '00')), 1, 0),
      
      # Characteristics of migrants
      ## Administrative constraints
      year_since_arrival=2021-ANMIG,
      residence = cut(year_since_arrival, breaks=c(0, 5, 10, Inf), include.lowest=TRUE),
      ##French Proficiency
      french_proficiency = factor(case_when(
        MAD21_LANGHOST_Y%in%c(1:4) ~ 5-as.numeric(MAD21_LANGHOST_Y),
        MAD21_LANGHOST_Y==5 ~ 5,
        TRUE~NA
      ))
    )%>% group_by(id_panel) %>%
    filter(all(work_in_alsace_moselle==FALSE, na.rm = T)) %>%
    ungroup()
  
  eec_model <- eec_model %>% select(
    id_panel,
    id_pooled,# ids
    DEPCOM,# municipality
    EXTRID,
    EXTRI, 
    quarter, 
    not_in_agriculture,
    sick_leave,
    sick_leave_spell,
    sick_leave_spell_more_than_3_months, 
    sick_leave_spell_less_than_3_months,
    absence_days,
    absence,
    migrant,
    migrant_factor,
    origin_migrant,
    female,
    gender,
    age,
    age_group,
    age_group3,
    single_parent_family,
    kids,
    nb_kids,
    higher_education,
    no_diploma,
    education,
    single_income_household,
    disabled,
    disabled_procedure,
    general_health,
    chronic_disease,
    active_remunerated,
    multiple_jobs,
    hours_worked,
    hours_worked_usual,
    hours_worked_usual_relative_to_35, 
    paid_overtime,
    parttime,
    short_term,
    public_sector,
    log_wage,
    tenure,
    tenure_factor,
    same_firm_before,
    some_management,
    siret,
    pcs,
    unskilled,
    year_since_arrival,
    residence,
    french_proficiency
  )
  

# Save to Stata the pre-processed database

write_dta(eec_model%>%select(-hours_worked_usual_relative_to_35), "Data/eec_2021_salaried.dta")










