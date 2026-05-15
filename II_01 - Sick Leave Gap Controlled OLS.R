
# Ethnic gap in sick leaves

setwd(dir = "C:/Users/Public/Documents/Nadia_Haytham/")

# 0. Build clean dataset

source("R Codes/I_02 - Construct Dataset EEC2021.R")
source("R Codes/auxiliary/get_coef_map.R")

rm(eec)
gc()

# Formula

controlled_ols_gap_sick_leaves<-function(data, control_variable, weights_col, cluster_col){
  
  # factor levels for categorical variables 
  if(length(control_variable)!=0){  subset_df <- data[control_variable]
  is_cat <- sapply(subset_df, function(x) is.factor(x)|| is.character(x))
  cols_to_dummy<-names(subset_df)[is_cat]
  
  # cols_to_dummy<-cols_to_dummy[cols_to_dummy%notin%c('id_panel', 'id_pooled')]
  
  print(paste("Columns that are factors", cols_to_dummy))
  
  if(length(cols_to_dummy)>0){data_final <- dummy_cols(data, select_columns = cols_to_dummy,
                                                       remove_selected_columns = T, 
                                                       remove_first_dummy = T)
  
  data_final<-data_final%>%rename_with(~.x %>% gsub("(", "", ., fixed = T)%>%
                                          gsub(")", "", ., fixed = T)%>%
                                          gsub("[", "", ., fixed = T)%>%
                                          gsub("]", "", ., fixed = T)%>%
                                         gsub(",", "", ., fixed = T))
  
  print(names(data_final))
  
  print(paste('Dummies created with columns:', cols_to_dummy))
  
  new_dummies <- grep(paste0("^(", paste(cols_to_dummy, collapse='|'), ')'), names(data_final), value=T)
  control_variable<-c(control_variable[control_variable%notin%cols_to_dummy], 
                      new_dummies)
  
  # also Removing columns with huge imbalances in NA shares: 
  imbalanced_na_share_cols <- c('chronic_disease', 'general_health', 'pcs', 'log_wage', 'same_firm_before')
  control_variable<-control_variable[control_variable%notin%imbalanced_na_share_cols]
                      
                      
  print(paste('Vars to add as controls:', new_dummies))
  print(paste('Control variables:', control_variable))
  }else{data_final<-data}}else{data_final<-data}

  
  
  
  design <- svydesign(ids = reformulate(cluster_col),
                      weights = reformulate(weights_col), 
                      data = data_final)
  
  rhs = c('migrant', control_variable)
  

    print(reformulate(termlabels = rhs, response='sick_leave'))
    model_propensity<-svyglm(
    formula = reformulate(termlabels = rhs, response='sick_leave'),
    design = design, 
    family = quasibinomial()
    )

  

  
  return(model_propensity)
  
}

#Data 

outcomes<-c('sick_leave', 'sick_leave_spell')

controls <- c('age_group3', 'female', 'single_parent_family', 'kids',
              'higher_education', 'no_diploma','disabled_procedure', 
              'single_income_household',
              'disabled', 
              # Work related
              'short_term','tenure_factor', 'some_management', 'unskilled', 'public_sector',
              # Working hours
              'parttime','hours_worked_usual_relative_to_35',
              # Precarity constraints
              'multiple_jobs')

eec_balance<-eec_model%>%select('migrant', 'quarter', 'EXTRID', 'EXTRI', 'id_pooled', 'id_panel',
                                c(controls, outcomes))%>%filter(EXTRID>0)


eec_balance_extri_weights<-eec_model%>%select('migrant', 'quarter', 'EXTRI', 'id_pooled', 'id_panel',
                                c(controls, outcomes))%>%
  mutate(q99weights = quantile(EXTRI, 0.99, na.rm=T))%>%
  group_by(id_panel)%>%
  filter(!any(EXTRI > q99weights, na.rm=T))%>%
  mutate(weights=mean(EXTRI), clusters=id_panel)%>%
  ungroup()

# Results

models_result <-c()
models_result_extri_weights <- c()
# Model 1: no controls

models_result[['Model 1']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance, 
  control_variable = NULL,
  weights_col = "EXTRID",
  cluster_col = "id_pooled"
  )


models_result_extri_weights[['Model 1']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance_extri_weights, 
  control_variable = NULL,
  weights_col = "weights",
  cluster_col = "id_panel"
)

# Model 2: Quarter FE

models_result[['Model 2']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance, 
  control_variable = c('quarter'),
  weights_col = "EXTRID",
  cluster_col = "id_pooled"
)

models_result_extri_weights[['Model 2']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance_extri_weights, 
  control_variable = c('quarter'),
  weights_col = "weights",
  cluster_col = "id_panel"
)

# Model 3: socio-demographic controls
socio_dem <- c('female', 'age_group3', 'higher_education', 'no_diploma')
models_result[['Model 3']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance, 
  control_variable = c('quarter', socio_dem),
  weights_col = "EXTRID",
  cluster_col = "id_pooled"
)

models_result_extri_weights[['Model 3']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance_extri_weights, 
  control_variable = c('quarter', socio_dem),
  weights_col = "weights",
  cluster_col = "id_panel"
)


# Model 4: socio-demographic controls + Health
health <- c('disabled', 'disabled_procedure')
models_result[['Model 4']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance, 
  control_variable = c('quarter', socio_dem, health),
  weights_col = "EXTRID",
  cluster_col = "id_pooled"
)

models_result_extri_weights[['Model 4']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance_extri_weights, 
  control_variable = c('quarter', socio_dem, health),
  weights_col = "weights",
  cluster_col = "id_panel"
)


# Model5: socio-demographic controls + Health + working hours
working_hours <- c('hours_worked_usual_relative_to_35', 'parttime')
models_result[['Model 5']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance, 
  control_variable = c('quarter', socio_dem, health, working_hours),
  weights_col = "EXTRID",
  cluster_col = "id_pooled"
)

models_result_extri_weights[['Model 5']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance_extri_weights, 
  control_variable = c('quarter', socio_dem, health, working_hours),
  weights_col = "weights",
  cluster_col = "id_panel"
)


# Model6: socio-demographic controls + Health + Work related
work_related <- c('short_term','tenure_factor', 'some_management', 'unskilled', 'public_sector')
models_result[['Model 6']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance, 
  control_variable = c('quarter', socio_dem, health, working_hours, work_related),
  weights_col = "EXTRID",
  cluster_col = "id_pooled"
)

models_result_extri_weights[['Model 6']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance_extri_weights, 
  control_variable = c('quarter', socio_dem, health, working_hours, work_related),
  weights_col = "weights",
  cluster_col = "id_panel"
)

# Model7: socio-demographic controls + Health + Work related + working hours + more precarious constraints
precarious <- c('multiple_jobs', 'single_income_household')
models_result[['Model 7']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance, 
  control_variable = c('quarter', socio_dem, health, work_related, working_hours, precarious),
  weights_col = "EXTRID",
  cluster_col = "id_pooled"
)


models_result_extri_weights[['Model 7']] <- controlled_ols_gap_sick_leaves(
  data = eec_balance_extri_weights, 
  control_variable = c('quarter', socio_dem, health, working_hours, work_related, precarious),
  weights_col = "weights",
  cluster_col = "id_panel"
)


# Save all models -------------------------

modelsummary(models_result, 
             stars = stars, 
             coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
             title = "Sick Leave Gap controlled for different characteristics", 
             notes = "Source: Continuous Labour Force Survey (EEC) - 2021. Survey weights applied. Outcome = Declared being on sick leave during the week of reference of the survey. Sample of wage earners on first interrogation.",
             output = 'Outputs/II_01/modelsummary_extrid.tex'
)


modelsummary(models_result_extri_weights, 
             stars = stars, 
             coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
             title = "Sick Leave Gap controlled for different characteristics", 
             notes = "Source: Continuous Labour Force Survey (EEC) - 2021. Survey weights applied = à la Cazenave-Lacroutz & Godzinski (2017). Outcome = Declared being on sick leave during the week of reference of the survey. Sample of wage earners",
             # output = 'Outputs/II_01/modelsummary_extri_reg_weights.tex'
)


rm(list=ls())
gc()

