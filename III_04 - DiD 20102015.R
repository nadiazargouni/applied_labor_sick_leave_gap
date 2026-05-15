################################################################################
################################################################################
############ DiD and DiDiD (Graphs Pre-Post) ###################################
################################################################################
################################################################################

setwd(dir = "C:/Users/Public/Documents/Nadia_Haytham/")

# 0. Build clean dataset ---------------------------------------------

source("R Codes/III_03 - DiD Pretrends 20102015.R")
source("R Codes/auxiliary/get_coef_map.R")
source("R codes/auxiliary/create_survey_tables.R")

# Formula ---------------------------------------------------

# treat = public * post
did_formula <- function(outcome,
                        data,
                        control_variable,
                        panel = F,
                        weighted_reg = F, 
                        didid = F,
                        year = F,
                        quarter = F,
                        hetero_var = NULL) {
  

  if (didid) {
    if (year) {
      interactions <- c(
        'public',
        'migrant',
        'public_x_mig',
        'post12',
        'post13',
        'treat12',
        'treat13',
        'post12_x_mig',
        'post13_x_mig',
        'ddd_2012',
        'ddd_2013'

      )
    }
    
    
    else if (quarter) {
      interactions <- c(
        'public',
        'migrant',
        'public_x_mig',
        'post_q1',
        'post_q2',
        'post_q3',
        'post_q4',
        'treat_q1',
        'treat_q2',
        'treat_q3',
        'treat_q4',
        'post_q1_x_mig',
        "post_q2_x_mig",
        "post_q3_x_mig",
        "post_q4_x_mig",
        'ddd_q1',
        'ddd_q2',
        'ddd_q3',
        'ddd_q4'

      )
    }
    
    
    else{
      interactions <- c('treat',
                        'post',
                        'public',
                        'migrant',
                        'post_x_mig',
                        'public_x_mig',
                        'ddd')
    }
    
    
    
  }else{
    
    
    interactions <- c('treat', 'post', 'public')}
  
  
  rhs <- c(interactions, control_variable)
  
  if(panel){
    if(weighted_reg){feols(
      fml = reformulate(termlabels = rhs, response = outcome),
      data = data$did_data, 
      weights = ~weights,
      cluster=~id_panel)}
    
    
    if(!is.null(hetero_var)){feols(
      fml = reformulate(termlabels = rhs, response = outcome),
      data = data$did_data, 
      cluster=~id_panel,
      fsplit = reformulate(hetero_var)
    )}else{feols(
      fml = reformulate(termlabels = rhs, response = outcome),
      data = data$did_data, 
      cluster=~id_panel
    )}
    
    }else{lm(
    formula = reformulate(termlabels = rhs, response=outcome),
    data = data$did_data
  )}
  
  
}

#Data 
outcomes <- c('sl_all', 'sl_short_1d', 'sl_short_2_7d', 'sl_long_lt3m',
              'sl_long_gt3m')

outcome_labels <- c('Any sick leave', 'Sick leave less than 2 days',
                    'Sick leave 2-7 days', 'Sick leave 1week-3months',
                    'Sick leave more than 3months')


controls <- c(
  'age_group3',
  'female',
  'single_parent_family',
  'kids',
  'higher_education',
  'no_diploma',
  'single_income_household',
  'disabled',
  'disabled_procedure',
  'general_health',
  'chronic_disease',
  'multiple_jobs',
  'hours_worked_usual',
  'parttime',
  'short_term',
  'log_wage',
  'tenure_factor',
  'same_firm_before',
  'some_management'
)



coef_did<-c(
  'post' = 'Post ', 
  'public' = 'Public ',
  "treat" = "Public x Post ",  
  "treat12" = "Public x 2012", 
  "treat13" = "Public x 2013", 
  "treat_q1" = "Public x Post x Q1 (Winter)",
  "treat_q2" = "Public x Post x Q2 (Spring)",
  "treat_q3" = "Public x Post x Q3 (Summer)",
  "treat_q4" = "Public x Post x Q4 (Fall)"
  
)

coef_ddd <- c(
  'post' = 'Post ', 
  'public' = 'Public ',
  'migrant' = 'Migrant',
  "treat" = "Public x Post ", 
  "treat12" = "Public x 2012 ", 
  "treat13" = "Public x 2013 ", 
  "treat_q1" = "Public x Post x Q1 (Winter) ",
  "treat_q2" = "Public x Post x Q2 (Spring) ",
  "treat_q3" = "Public x Post x Q3 (Summer) ",
  "treat_q4" = "Public x Post x Q4 (Fall) ",
  'public_x_mig' ='Public x Migrant',
'post_x_mig' = "Post x Migrant",
'post12_x_mig' = "2012 x Migrant",
'post13_x_mig' = "2013 x Migrant",
'post_q1_x_mig' = "Post x Migrant x Q1 (Winter)",
'post_q2_x_mig' = "2012 x Migrant x Q2 (Spring)",
'post_q3_x_mig' = "2013 x Migrant x Q3 (Summer)",
'post_q4_x_mig' = "2013 x Migrant x Q4 (Fall)",

'ddd' = "Public x Post x Migrant",
'ddd_2012' = "Public x 2012 x Migrant",
'ddd_2013' = "Public x 2013 x Migrant",

"ddd_q1" = "Public x Post x Migrant x Q1 (Winter)",
"ddd_q2" = "Public x Post x Q2 (Spring)",
"ddd_q3" = "Public x Post x Q3 (Summer)",
"ddd_q4" = "Public x Post x Q4 (Fall)"
)




# Balance table to check if we have enough power ---------------------------------
table_sample_did <- create_survey_tables(data = did_input$did_data%>%select(
  'id_panel', 'weights', names(coef_ddd)
), 
                                         vars_to_summarize = names(coef_ddd), 
                                         weights_col = 'weights', 
                                         cluster_col = 'id_panel')

View(table_sample_did)
write.csv(table_sample_did, 'Outputs/III_04/didid_central_public_sector_table_n_obs.csv')

kableExtra::kable(table_sample_did, format="latex", output = 'Outputs/III_04/didid_central_public_sector_table_n_obs.tex')
# 
# weighted_table <- did_data_svy %>% 
#   tbl_svysummary(
#     by = year,
#     include = names(coef_ddd), 
#     statistic = list(
#       all_categorical() ~ "{p}% ({n_unweighted}, {N})",
#       all_continuous() ~"{mean} ({sd})"
#     ),
#     missing = "no",
#     label = coef_map_svysummary(coef_ddd, names(coef_ddd))
#   ) 



# Results -------------------------------------------------

models_result <-c()


# Results for PANEL OLS DIDID for all outcomes: 
for (outcome in outcomes){
  
  
  models_result[[paste('Model', outcome)]] <- did_formula(outcome, data = did_input,
                                                          control_variable = NULL, 
                                                          panel = T, didid = T)
  
  summary(models_result[[paste('Model', outcome)]])
}

modelsummary(models_result, 
             stars = stars, 
             coef_map = c(coef_ddd, coef_map[names(coef_map)%notin%names(coef_ddd)]),
             gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
             title = "DiDiD Sick Leave Gap", 
             output = 'Outputs/III_04/didid_central_public_sector_all_durations.tex'
)





# Model 1: POOLED OLS DID


models_result[['Model 1']] <- did_formula('sl_all', data = did_input,
                                          control_variable = NULL, 
                                          panel = F, didid = F)

summary(models_result[['Model 1']])
# Model 2: PANEL OLS DID

models_result[['Model 2']] <- did_formula('sl_all', data = did_input,
                                          control_variable = NULL, 
                                          panel = T, didid = F)
summary(models_result[['Model 2']])


# Model 3: POOLED OLS DIDID

models_result[['Model 3']] <- did_formula('sl_all', data = did_input,
                                          control_variable = NULL, 
                                          panel = F, didid = T)

summary(models_result[['Model 3']])


# Model 4: PANEL OLS DIDID


models_result[['Model 4']] <- did_formula('sl_all', data = did_input,
                                          control_variable = NULL, 
                                          panel = T, didid = T)



summary(models_result[['Model 4']])



# Model 5: PANEL OLS DIDID by year

models_result[['Model 5']] <- did_formula('sl_all', data = did_input,
                                          control_variable = NULL, 
                                          panel = T, didid = T, year = T)

summary(models_result[['Model 5']])

# Model 6: PANEL OLS DIDID by quarter

models_result[['Model 6']] <- did_formula('sl_all', data = did_input,
                                          control_variable = NULL, 
                                          panel = T, didid = T, quarter = T)

summary(models_result[['Model 6']])

# Model 7: Socio demographic controls ## TO IMPROVE
quarter_fe <- c('quarter_f')
socio_health <- c('female', 'age_group3', 'higher_education', 'no_diploma')


models_result[['Model 7']] <- did_formula('sl_all', data = did_input,
                                          control_variable = quarter_fe,
                                          panel = T, didid = T, hetero_var = 'age_group3')

models_result[['Model 7bis']] <- did_formula('sl_all', data = did_input,
                                          control_variable = quarter_fe,
                                          panel = T, didid = T, hetero_var = 'higher_education')



# Model 8: Socio demographic controls + work-related ## TO IMPROVE
work_related <- c('short_term','tenure_factor', 'same_firm_before', 'some_management')


models_result[['Model 8']] <- did_formula('sl_all', data = did_input,
                                          control_variable = 'short_term',
                                          panel = T, didid = T)

models_result[['Model 8bis']] <- did_formula('sl_all', data = did_input,
                                             control_variable = c('quarter_f', 'tenure_factor'),
                                             panel = T, didid = T)

models_result[['Model 8ter']] <- did_formula('sl_all', data = did_input,
                                          control_variable = quarter_fe,
                                          panel = T, didid = T, hetero_var = 'tenure_factor')

summary(models_result[['Model 8bis']])


# Model 9: precarious constraints ## TO IMPROVE
precarious <- c('multiple_jobs', 'single_income_household', 'log_wage')


models_result[['Model 9']] <- did_formula('sl_all', data = did_input,
                                          control_variable = c('quarter_f', 'log_wage'),
                                          panel = T, didid = T)

summary(models_result[['Model 9']])




# Saving models to summary table -----------------------------------------------------------------------


# Did vs DiDiD
modelsummary(models_result, 
             stars = stars, 
             coef_map = c(coef_ddd, coef_map[names(coef_map)%notin%names(coef_ddd)]),
             gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
             title = "DiD vs. DiDiD Sick Leave Gap", 
             notes = "Source: Continuous Labour Force Survey (EEC) - 2010-2013. Survey weights not applied. Outcome = Declared being on sick leave during the week of reference of the survey. Sample of salaried wage earners.",
             output = 'Outputs/III_04/didid_central_public_sector.tex'
)




