

#####DESCRIPTIVE STATISTICS########################
#####Balance Table#################################

setwd(dir = "C:/Users/Public/Documents/Nadia_Haytham/")

# 0. Build clean dataset

source("R Codes/I_02 - Construct Dataset EEC2021.R")
source("R Codes/auxiliary/create_survey_tables.R")
rm(eec)
gc()
# Statistics on sample ----------------------------

## Balance table function with all our characteristics -------------------------


  
outcomes<-c('sick_leave', 'sick_leave_spell')
  
controls <- c('quarter','age', 'age_group3','female', 'single_parent_family', 'kids','nb_kids', 'higher_education',
                'no_diploma','education', 'single_income_household', 'disabled', 'disabled_procedure', 
                'general_health', 'chronic_disease', 'multiple_jobs', 'hours_worked', 'hours_worked_usual',
                'paid_overtime', 'parttime', 'short_term', 'public_sector', 'log_wage', 'tenure', 
                'same_firm_before', 'some_management', 'unskilled','pcs')
  
  
## Different specifications for the weighting and the sample ----------------

results<-c()

eec_balance<-eec_model%>%select('migrant', 'quarter', 'EXTRID', 'EXTRI', 'id_pooled', 'id_panel',
                                                 c(controls, outcomes))
  
  ### 1. Use only first interview, EXTRID weights, id_pooled=id_panel as cluster. 

  # We would like to have a sample with non null weighting of EXTRID 
# because it's our main weighting variable since most of the variables we use
# are collected only at the first interview of each individual and it is advised
# by Insee to use these weights 
results[[1]]<-create_survey_tables_migrants(
    data = eec_balance%>%filter(EXTRID>0), 
    target_var = "migrant", 
    vars_to_summarize = c(controls, outcomes), 
    weights_col = "EXTRID",
    cluster_col = "id_pooled"
  )

write.csv(results[[1]], 'Outputs/I_03/Balance_Table_rga1_extrid.csv')

### 2. Use all interviews, EXTRI/4 weights, id_pooled as cluster (individual = person*quarter). 
  # as advised by Insee, divide by 4 weights of survey. 

results[[2]]<-create_survey_tables_migrants(
  data = eec_balance%>%select(-EXTRID)%>%mutate(EXTRI = EXTRI/4), 
  target_var = "migrant", 
  vars_to_summarize = c(controls, outcomes), 
  weights_col = "EXTRI",
  cluster_col = "id_pooled"
)

write.csv(results[[2]], 'Outputs/I_03/Balance_Table_extri_div_4_cluster_id_pooled.csv')


### 2. Use all interviews, EXTRI/4 weights, id_panel as cluster (individual = perso). 

results[[3]]<-create_survey_tables_migrants(
  data = eec_balance%>%select(-EXTRID)%>%mutate(EXTRI = EXTRI/4), 
  target_var = "migrant", 
  vars_to_summarize = c(controls, outcomes), 
  weights_col = "EXTRI",
  cluster_col = "id_panel"
)

write.csv(results[[3]], 'Outputs/I_03/Balance_Table_extri_div_4_cluster_id_panel.csv')


## Check that migration status is balanced across quarters ----------------
# Over or undersampling during some quarters may result in biased estimates
# of the sick leave gap since sick leaves are seasonal. 

design <- svydesign(ids = reformulate('id_pooled'),
                    weights = reformulate('EXTRID'), 
                    data = eec_balance%>%filter(EXTRID>0))

tab_counts<-svytable(reformulate("migrant + quarter"), design)

tab_prop <- prop.table(tab_counts, margin = 2)

test_result<-svychisq(reformulate("migrant + quarter"), design, statistic="Wald")
# Design-based Wald test of association
# 
# data:  svychisq(reformulate("migrant + quarter"), design, statistic = "Wald")
# F = 1.9223, ndf = 3, ddf = 25872, p-value = 0.1235

write.csv(tab_counts, 'Outputs/I_03/migrant_number_by_quarter.csv')

tab_prop
write.csv(tab_prop, 'Outputs/I_03/migrant_share_by_quarter.csv')






## Descriptive table of migrants ---------------------------

controls_migrants <- c('origin_migrant', 'year_since_arrival', 'residence', 'french_proficiency')

datasummary_balance( ~ 1,
                     data = eec_model%>%filter(EXTRID>0)%>%
                       filter(migrant==1)%>%
                       mutate(weights=EXTRID, cluster=id_pooled)%>%
                       select(
                       c(outcomes, controls, controls_migrants, c('weights', 'cluster'))
                     ),
                     dinm = TRUE,
                     fmt=2,
                     title = "Balance Table of characteristics across migrant and native workers",
                     notes = "Source: Continuous Labour Force Survey (EEC) - 2021. Absences on week of reference. the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted. ",
                     output = 'Outputs/I_03/descriptive_table_migrants.tex'
                     )


datasummary_balance( ~ migrant_factor,
                     data = eec_model%>%filter(EXTRID>0)%>%
                       # filter(migrant==1)%>%
                       mutate(weights=EXTRID)%>%
                       select(
                         c(outcomes, controls, c('weights', 'migrant_factor'))
                       ),
                     dinm = TRUE,
                     fmt=2,
                     title = "Balance Table of characteristics across migrant and native workers",
                     notes = "Source: Continuous Labour Force Survey (EEC) - 2021. Absences on week of reference. the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted. ",
                     output = 'Outputs/I_03/balance_table_migrants_extrid_rga_1_modelsummary.tex'
)

datasummary_balance( ~ migrant_factor,
                     data = eec_model%>%
                       mutate(weights=EXTRI)%>%
                       select(
                         c(outcomes, controls, c('weights', 'migrant_factor'))
                       ),
                     dinm = TRUE,
                     fmt=2,
                     title = "Balance Table of characteristics across migrant and native workers",
                     notes = "Source: Continuous Labour Force Survey (EEC) - 2021. Absences on week of reference. the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted. ",
                     output = 'Outputs/I_03/balance_table_migrants_extri_modelsummary.tex'
)

datasummary_balance( ~ migrant_factor,
                     data = eec_model%>%
                       mutate(q99weights = quantile(EXTRI, 0.99, na.rm=T))%>%
                       group_by(id_panel)%>%
                       filter(!any(EXTRI > q99weights, na.rm=T))%>%
                       mutate(weights=mean(EXTRI), clusters=id_panel)%>%
                       ungroup()%>%
                       select(
                         c(outcomes, controls, c('weights', 'migrant_factor'))
                       ),
                     dinm = TRUE,
                     fmt=2,
                     title = "Balance Table of characteristics across migrant and native workers",
                     notes = "Source: Continuous Labour Force Survey (EEC) - 2021. Absences on week of reference. the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted. ",
                     output = 'Outputs/I_03/balance_table_migrants_reg_weigths_modelsummary.tex'
)


