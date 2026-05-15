
#####DESCRIPTIVE STATISTICS########################
#####Balance Table#################################

setwd(dir = "C:/Users/Public/Documents/Nadia_Haytham/")

# 0. Build clean dataset

source("R Codes/III_01 - DiD Construct Dataset EEC20102015.R")
source("R Codes/auxiliary/get_coef_map.R")

# Statistics on whole sample ----------------------------


## Define variables in stat desc -------------------------------

variables_stats_desc <- c(outcomes, treatments,
treatments_hetero, controls, controls_migrants)


non_numeric_variables_stats_desc <- pop_interest%>%
  select(variables_stats_desc)%>%
  select(!where(is.numeric))%>%
  names()







# EXTRID weights first interrogation only = 1 row per individual ---------------------

## create survey object ------------------------------------

pop_interest_svy_extrid <- svydesign(ids = ~id_panel,
                                     weights = ~EXTRID,
                                     data = pop_interest%>%filter(wave==1)%>%filter(!is.na(EXTRID)))


## create table ------------------------------------

datasummary_balance( ~ 1,
                     data = pop_interest%>%filter(wave==1)%>%filter(!is.na(EXTRID))%>%
                       mutate(weights=EXTRID)%>%
                       select(
                         c(variables_stats_desc, c('weights'))
                       ) %>% rename(!!!coef_map_stat_desc(variables_stats_desc)),
                     dinm = TRUE,
                     fmt=2,
                     title = "Table of characteristics",
                     notes = "Source: Continuous Labour Force Survey (EEC) - 2010-2015. Weights = 1st interrogation weights. Absences on week of reference. the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted. ",
                     output = 'Outputs/III_02/descriptive_table_did_extrid_datasummary.tex'
)


weighted_table <- pop_interest_svy_extrid %>% 
  tbl_svysummary(
    by = NULL,
    include = variables_stats_desc, 
    statistic = list(
      all_categorical() ~ "{p}% ({p.std.error})",
      all_continuous() ~"{mean} ({sd})"
    ),
    missing = "no",
    label = coef_map_svysummary(variables_stats_desc)
  ) 


gt::gtsave(weighted_table%>%as_gt(), filename = 'Outputs/III_02/descriptive_table_did_extrid.tex')

# EXTRI weights = n_waves row per individual = cluster se at id_panel level -------------

pop_interest_svy_extri <- svydesign(ids = ~id_panel,
                                     weights = ~EXTRI,
                                     data = pop_interest%>%filter(!is.na(EXTRI))
                                    )


datasummary_balance( ~ 1,
                     data = pop_interest%>%
                       mutate(weights=EXTRI, clusters = id_panel)%>%
                       select(
                         c(variables_stats_desc, c('weights', 'clusters'))
                       ) %>% rename(!!!coef_map_stat_desc_rename),
                     dinm = TRUE,
                     fmt=2,
                     title = "Table of characteristics",
                     notes = "Source: Continuous Labour Force Survey (EEC) - 2010-2015. Weights = wave specific weights. Absences on week of reference. the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted. ",
                     output = 'Outputs/III_02/descriptive_table_did_extri_datasummary.tex'
)

weighted_table <- pop_interest_svy_extri %>% 
  tbl_svysummary(
    by = NULL,
    include = variables_stats_desc, 
    statistic = list(
      all_categorical() ~ "{p}% ({n_unweighted})",
      all_continuous() ~"{mean} ({sd})"
    ),
    digits = 2, 
    missing = "no",
    label = coef_map_svysummary(variables_stats_desc)
  ) 



gt::gtsave(weighted_table%>%as_gt(), filename = 'Outputs/III_02/descriptive_table_did_extri.tex')



# Cazenave-Lacroutz & Godzinski (2017) weights
# weights = avg(max(EXTRI, p99(EXTRI))), n_waves row per individual = cluster se at id_panel level

pop_interest_weightreg <- svydesign(ids = ~ id_panel, 
                                    weights = ~ weights, 
                                    pop_interest%>%
                                      mutate(q99weights = quantile(EXTRI, 0.99, na.rm=T))%>%
                                      group_by(id_panel)%>%
                                      filter(!any(EXTRI > q99weights, na.rm=T))%>%
                                      mutate(weights=mean(EXTRI), clusters=id_panel)%>%
                                      ungroup())


weighted_table <- pop_interest_svy_extri %>% 
  tbl_svysummary(
    by = NULL,
    include = variables_stats_desc, 
    statistic = list(
      all_categorical() ~ "{p}% ({p.std.error})",
      all_continuous() ~"{mean} ({sd})"
    ),
    missing = "no",
    label = coef_map_svysummary(variables_stats_desc)
  ) 

weighted_table


gt::gtsave(weighted_table%>%as_gt(), filename = 'Outputs/III_02/descriptive_table_did_weightreg.tex')




datasummary_balance( ~ 1,
                     data = pop_interest%>%
                       mutate(q99weights = quantile(EXTRI, 0.99, na.rm=T))%>%
                       group_by(id_panel)%>%
                       filter(!any(EXTRI > q99weights, na.rm=T))%>%
                       mutate(weights=mean(EXTRI), clusters=id_panel)%>%
                       ungroup()%>%
                       select(
                         c(variables_stats_desc, c('weights', 'clusters'))
                       ) %>% rename(!!!coef_map_stat_desc_rename),
                     dinm = TRUE,
                     fmt=2,
                     title = "Table of characteristics",
                     notes = "Source: Continuous Labour Force Survey (EEC) - 2010-2015. Weights = see Cazenave-Lacroutz & Godzinski (2017). Absences on week of reference. the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted. ",
                     output = 'Outputs/III_02/descriptive_table_did_weightreg_datasummary.tex'
)





# DiD balance tables -------------------------------------------------------

## First create additional variables for the DiD ---------------------------


weighted_table <- pop_interest_svy_extri %>% 
  tbl_svysummary(
    by = migrant,
    include = variables_stats_desc, 
    statistic = list(
      all_categorical() ~ "{p}% ({n_unweighted})",
      all_continuous() ~"{mean} ({sd})"
    ),
    digits = 2, 
    missing = "no",
    label = coef_map_svysummary(variables_stats_desc)
  ) %>% 
  add_p() %>% 
  separate_p_footnotes()

weighted_table


gt::gtsave(weighted_table%>%as_gt(), filename = 'Outputs/III_02/balance_table_did_migrant.tex')

weighted_table <- pop_interest_svy_extri %>% 
  tbl_svysummary(
    by = public_sector,
    include = variables_stats_desc, 
    statistic = list(
      all_categorical() ~ "{p}% ({n_unweighted})",
      all_continuous() ~"{mean} ({sd})"
    ),
    digits = 2, 
    missing = "no",
    label = coef_map_svysummary(variables_stats_desc)
  ) %>% 
  add_p() %>%
  separate_p_footnotes()

weighted_table


gt::gtsave(weighted_table%>%as_gt(), filename = 'Outputs/III_02/balance_table_did_public_sector.tex')



weighted_table <- pop_interest_svy_extri %>% 
  tbl_svysummary(
    by = triple_interaction_group,
    include = variables_stats_desc, 
    statistic = list(
      all_categorical() ~ "{p}% ({n_unweighted})",
      all_continuous() ~"{mean} ({sd})"
    ),
    digits = 2, 
    missing = "no",
    label = coef_map_svysummary(variables_stats_desc)
  ) %>% 
  add_p() %>%
  separate_p_footnotes()

weighted_table


gt::gtsave(weighted_table%>%as_gt(), filename = 'Outputs/III_02/balance_table_did_migrant_public_sector.tex')







