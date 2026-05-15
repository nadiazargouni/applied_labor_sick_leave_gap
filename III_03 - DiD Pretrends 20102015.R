################################################################################
################################################################################
############ DiD Descriptive Statistics (Graphs Pre-Post) ######################
################################################################################
################################################################################

setwd(dir = "C:/Users/Public/Documents/Nadia_Haytham/")

# 0. Build clean dataset

source("R Codes/III_01 - DiD Construct Dataset EEC20102015.R")
source("R Codes/auxiliary/get_coef_map.R")

# Prepare data (pre post, treatment variables) ---------------------------------

prepare_did_data <- function(publicvar, data){
  
  did_data <- data  %>%
    
    
    mutate(q99weights = quantile(EXTRI, 0.99, na.rm=T))%>%
    group_by(id_panel)%>%
    filter(!any(EXTRI > q99weights, na.rm=T))%>%
    mutate(weights=mean(EXTRI), clusters=id_panel)%>%
    ungroup() %>% # weights à la Cazenave-Lacroutz & Godzinski (2017)
    
    
    mutate(public = case_when(
    public_sector == 0 ~ 0, # control group is always private sector
    !!sym(publicvar) == 1 ~1, # different possible subgroups of public sector to test
    TRUE ~NA
  ), 
  public_factor = factor(public, labels = c('Private sector', 'Public sector')),
  group_real = case_when(
    public==1 & migrant == 0 ~ "Native - Public",
    public==0 & migrant == 0 ~ "Native - Private", 
    public==1 & migrant == 1 ~ "Migrant - Public", 
    public==0 & migrant == 1 ~ "Migrant - Private", 
    TRUE ~ NA_character_
  ), 
  group = factor(group_real, labels = c("Migrant - Private","Migrant - Public", 
                                   "Native - Private", "Native - Public"))) %>%
    filter(!is.na(public))
  
  

  
did_data <- did_data %>% mutate(
  year_f = year, quarter_f = quarter, # factors
  year = ANNEE, quarter = TRIM, # back to integers
  time_id = paste0(year, "Q", quarter),
  
  
  # Treatment variables = POST (reform starts in 2012)
  post = as.integer(year %in% c(2012, 2013)),
  post12 = as.integer(year == 2012), 
  post13 = as.integer(year == 2013),
  
  
  # Quarterly treatment interactions
  post_q1 = post*as.integer(quarter==1),
  post_q2 = post*as.integer(quarter==2),
  post_q3 = post*as.integer(quarter==3),
  post_q4 = post*as.integer(quarter==4),
  
  
  # DiD treatment dummies (first DiD)
  treat = public * post, 
  treat12 = public * post12, 
  treat13 = public * post13, 
  treat_q1 = public * post_q1,
  treat_q2 = public * post_q2,
  treat_q3 = public * post_q3,
  treat_q4 = public * post_q4,
  
  
  # DiDiD interaction terms (public * post * migrant)
  public_x_mig = public * migrant,
  post_x_mig = post * migrant, 
  ddd = post * migrant * public,
  ddd_2012 = public * post12 * migrant,
  ddd_2013 = public * post13 * migrant,
  post12_x_mig = post12 * migrant, 
  post13_x_mig = post13 * migrant, 
  post_q1_x_mig = post_q1 * migrant, 
  post_q2_x_mig = post_q2 * migrant, 
  post_q3_x_mig = post_q3 * migrant, 
  post_q4_x_mig = post_q4 * migrant, 
  ddd_q1 = public * post_q1 * migrant,
  ddd_q2 = public * post_q2 * migrant,
  ddd_q3 = public * post_q3 * migrant,
  ddd_q4 = public * post_q4 * migrant,
  
  sl_all = as.integer(sick_leave==1), 
  sick_leave_bins =  cut(sick_leave_spell, breaks=c(0, 2, 7, 30*3, Inf), include.lowest=T, right=F), # 3m cut
  sl_short_1d = as.integer(sick_leave_spell=="[0,2)"),
  sl_short_2_7d = as.integer(sick_leave_spell=="[2,7)"),
  
  sl_long_lt3m = as.integer(sick_leave_spell=="[7,90)"),
  sl_long_gt3m = as.integer(sick_leave_spell=="[90,Inf]"),
  
  
  
  absence = as.integer(absence)
  
)





outcomes <- c('sl_all', 'sl_short_1d', 'sl_short_2_7d', 'sl_long_lt3m',
              'sl_long_gt3m', 'absence')

outcome_labels <- c('Any sick leave', 'Sick leave less than 2 days',
                    'Sick leave 2-7 days', 'Sick leave 1week-3months',
              'Sick leave more than 3months')


did_data_svy <- svydesign(
  ids = ~id_panel, 
  weights = ~weights,
  data = as.data.frame(did_data%>% select(c('id_panel','weights','year_f', 'quarter_f', 'year', 'quarter',
                                                       'time_id', 
                                            'public_factor',
                                                       'group', outcomes),  starts_with('post'), 
                                                     starts_with('treat'), starts_with('ddd'), 
                                          starts_with('migrant'), starts_with('public')))
)





return(list(did_data=did_data, did_data_svy = did_data_svy, 
            outcomes=outcomes, outcome_labels=outcome_labels))

}






# Setup: computing prevalence, layout of graphs ---------------------------------------------------------------------

svy_tidy <-function(outcome, by_vars, design, as_tibble = T){

  fml_outcome<-reformulate(outcome)
  fml_by <- reformulate(termlabels = by_vars)
  
  
  if(as_tibble){return(svyby(formula = fml_outcome, 
        by = fml_by, 
        design = design, 
        FUN = svymean, 
        na.rm =T, 
        covmat = T,
        vartype = "ci") %>%
    as_tibble())}else{
      return(svyby(formula = fml_outcome, 
                     by = fml_by, 
                     design = design, 
                     FUN = svymean, 
                     na.rm =T, 
                     covmat = T,
                     vartype = "ci"))
             }
}


parse_qdate<-function(tid){
  yr<-as.integer(substr(tid, 1, 4))
  qtr<-as.integer(substr(tid, 6, 6))
  
  return(as.Date(paste0(yr, "-", sprintf("%02d", (qtr-1)*3+1), "-01")))
}


intro_date <- as.Date("2011-12-31")
repeal_date<-as.Date("2014-01-01")


policy_lines <- list(
  geom_vline(xintercept = intro_date, linetype = "dashed", linewidth = 0.5),
  geom_vline(xintercept = repeal_date, linetype = "dashed", linewidth = 0.5),
  annotate("text", x = intro_date, y=Inf, label = "Introduction", 
           hjust = -0.05, vjust = 1.4, size = 2.8),
  annotate("text", x = repeal_date, y=Inf, label = "Repeal", 
           hjust = -0.05, vjust = 1.4, size = 2.8)
  
  
)


minimal_theme <- theme_minimal(base_size = 11) + theme(
  legend.position = "bottom", 
  legend.title = element_blank(),
  panel.grid.minor = element_blank()
)

# I. Treatment = Public sector as a whole ----------------------------------------
## 0. Define data

did_input <- prepare_did_data('public_sector', pop_interest)
outcomes = did_input$outcomes
outcome_labels = did_input$outcome_labels
did_data_svy = did_input$did_data_svy


## 1. Public vs. Private --------------------------

prev_sector <- bind_rows(
  lapply(setNames(outcomes, outcomes), function(out){
    svy_tidy(out, c('public_factor', 'time_id'), did_data_svy)
  }), 
  .id = "outcome"
) %>% 
  mutate(date = parse_qdate(time_id))





for(out in c('sl_all', 'sl_short_1d', 'sl_long_lt3m', 'sl_long_gt3m')){
  

  p_a <- ggplot(prev_sector%>%filter(!is.na(!!sym(out))), aes(x = date, y=!!sym(out), 
                                                          group = public_factor, 
                                                          colour = public_factor, 
                                                          fill = public_factor)) +
    geom_ribbon(aes(ymin=ci_l, ymax = ci_u), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 0.8) + 
    geom_point(size = 1.4) + 
    policy_lines + 
    labs(x = "Date", y = "Prevalence") + minimal_theme
  
  print(p_a)
  
  
  ggsave(p_a, filename = paste0('Outputs/III_03/trends_',out,'_public_sector.png'))}


## 2. Migrants and Natives x Public and Private --------------------------


prev_sector_migrant <- bind_rows(
  lapply(setNames(outcomes, outcomes), function(out){
    svy_tidy(out, c('group', 'time_id'), did_data_svy)
  }), 
  .id = "outcome"
) %>% 
  mutate(date = parse_qdate(time_id))
for(out in c('sl_all', 'sl_short_1d', 'sl_long_lt3m', 'sl_long_gt3m')){
  
  p_b <- ggplot(prev_sector_migrant%>%filter(!is.na(!!sym(out))), aes(x = date, y=!!sym(out), 
                                                          group = group, 
                                                          colour = group, 
                                                          fill = group)) +
    geom_ribbon(aes(ymin=ci_l, ymax = ci_u), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 0.8) + 
    geom_point(size = 1.4) + 
    policy_lines + 
    labs(x = "Date", y = "Prevalence") + minimal_theme
  
  print(p_b)
  
  ggsave(p_b, filename = paste0('Outputs/III_03/trends_',out,'_migrant_public_sector.png'))}



## 3. Gap ----------------------------------------------------------------

# First we focus on *any sick leave* 

for(out in c('sl_all', 'sl_short_1d', 'sl_long_lt3m', 'sl_long_gt3m')){
  
  prev_gap_svy_object <- svy_tidy(out, c('migrant', 'public', 'time_id'), did_data_svy, as_tibble = F)
  
  quarters <- unique(prev_gap_svy_object$time_id)
  
  results <- lapply(quarters, function(t){
    nm <- function(pub, mig){paste0(mig, ".", pub, ".", t)}
    
    gap_private_sector <- svycontrast(prev_gap_svy_object, 
                                      setNames(c(-1,1), 
                                               c(nm(0, 0), nm(0,1))))
    
    gap_public_sector <- svycontrast(prev_gap_svy_object, 
                                     setNames(c(-1,1), 
                                              c(nm(1, 0), nm(1,1))))
    
    
    data.frame(
      time_id = t, 
      sector = c('Private sector', 'Public sector'), 
      gap = c(coef(gap_private_sector), coef(gap_public_sector)), 
      se = c(SE(gap_private_sector), SE(gap_public_sector)),
      ci_l = c(coef(gap_private_sector), coef(gap_public_sector)) - 1.96*c(SE(gap_private_sector), SE(gap_public_sector)), 
      ci_u = c(coef(gap_private_sector), coef(gap_public_sector)) + 1.96*c(SE(gap_private_sector), SE(gap_public_sector))
      
    )
    
  }
  
  
  )
  
  
  
  prev_gap <- do.call(rbind, results)%>% 
    mutate(date = parse_qdate(time_id))
  
  
  p_c <- ggplot(prev_gap%>%filter(!is.na(gap)), aes(x = date, y=gap, 
                                                    group = sector, 
                                                    colour = sector, 
                                                    fill = sector)) +
    geom_ribbon(aes(ymin=ci_l, ymax = ci_u), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 0.8) + 
    geom_point(size = 1.4) + 
    policy_lines + 
    labs(x = "Date", y = "Prevalence_Migrants - Prevalence_Natives") + minimal_theme
  
  print(p_c)
  
  ggsave(p_c, filename = paste0('Outputs/III_03/trends_',out ,'_gap_by_sector.png'))
  
  rm(p_c, prev_gap, prev_gap_svy_object)
  
}


## 4. Filter Data : from 2010Q1 to 2013Q4 ---------------------------------------------------

did_input <- prepare_did_data('public_sector', pop_interest%>%filter(year %in% c(2010:2013)))



# II. Central civic service as public sector ---------------------------------------------------



## 0. Define data

did_input <- prepare_did_data('central_civil_service', pop_interest)
outcomes = did_input$outcomes
outcome_labels = did_input$outcome_labels
did_data_svy = did_input$did_data_svy


## 1. Public vs. Private --------------------------

prev_sector <- bind_rows(
  lapply(setNames(outcomes, outcomes), function(out){
    svy_tidy(out, c('public_factor', 'time_id'), did_data_svy)
  }), 
  .id = "outcome"
) %>% 
  mutate(date = parse_qdate(time_id))





for(out in c('sl_all', 'sl_short_1d', 'sl_long_lt3m', 'sl_long_gt3m')){
  
  
  p_a <- ggplot(prev_sector%>%filter(!is.na(!!sym(out))), aes(x = date, y=!!sym(out), 
                                                              group = public_factor, 
                                                              colour = public_factor, 
                                                              fill = public_factor)) +
    geom_ribbon(aes(ymin=ci_l, ymax = ci_u), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 0.8) + 
    geom_point(size = 1.4) + 
    policy_lines + 
    labs(x = "Date", y = "Prevalence") + minimal_theme
  
  print(p_a)
  
  
  ggsave(p_a, filename = paste0('Outputs/III_03/central_public_trends_',out,'_public_sector.png'))}


## 2. Migrants and Natives x Public and Private --------------------------


prev_sector_migrant <- bind_rows(
  lapply(setNames(outcomes, outcomes), function(out){
    svy_tidy(out, c('group', 'time_id'), did_data_svy)
  }), 
  .id = "outcome"
) %>% 
  mutate(date = parse_qdate(time_id))
for(out in c('sl_all', 'sl_short_1d', 'sl_long_lt3m', 'sl_long_gt3m')){
  
  p_b <- ggplot(prev_sector_migrant%>%filter(!is.na(!!sym(out))), aes(x = date, y=!!sym(out), 
                                                                      group = group, 
                                                                      colour = group, 
                                                                      fill = group)) +
    geom_ribbon(aes(ymin=ci_l, ymax = ci_u), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 0.8) + 
    geom_point(size = 1.4) + 
    policy_lines + 
    labs(x = "Date", y = "Prevalence") + minimal_theme
  
  print(p_b)
  
  ggsave(p_b, filename = paste0('Outputs/III_03/central_public_trends_',out,'_migrant_public_sector.png'))}



## 3. Gap ----------------------------------------------------------------

# First we focus on *any sick leave* 

for(out in c('sl_all', 'sl_short_1d', 'sl_long_lt3m', 'sl_long_gt3m')){
  
  prev_gap_svy_object <- svy_tidy(out, c('migrant', 'public', 'time_id'), did_data_svy, as_tibble = F)
  
  quarters <- unique(prev_gap_svy_object$time_id)
  
  results <- lapply(quarters, function(t){
    nm <- function(pub, mig){paste0(mig, ".", pub, ".", t)}
    
    gap_private_sector <- svycontrast(prev_gap_svy_object, 
                                      setNames(c(-1,1), 
                                               c(nm(0, 0), nm(0,1))))
    
    gap_public_sector <- svycontrast(prev_gap_svy_object, 
                                     setNames(c(-1,1), 
                                              c(nm(1, 0), nm(1,1))))
    
    
    data.frame(
      time_id = t, 
      sector = c('Private sector', 'Public sector'), 
      gap = c(coef(gap_private_sector), coef(gap_public_sector)), 
      se = c(SE(gap_private_sector), SE(gap_public_sector)),
      ci_l = c(coef(gap_private_sector), coef(gap_public_sector)) - 1.96*c(SE(gap_private_sector), SE(gap_public_sector)), 
      ci_u = c(coef(gap_private_sector), coef(gap_public_sector)) + 1.96*c(SE(gap_private_sector), SE(gap_public_sector))
      
    )
    
  }
  
  
  )
  
  
  
  prev_gap <- do.call(rbind, results)%>% 
    mutate(date = parse_qdate(time_id))
  
  
  p_c <- ggplot(prev_gap%>%filter(!is.na(gap)), aes(x = date, y=gap, 
                                                    group = sector, 
                                                    colour = sector, 
                                                    fill = sector)) +
    geom_ribbon(aes(ymin=ci_l, ymax = ci_u), alpha = 0.14, colour = NA) +
    geom_line(linewidth = 0.8) + 
    geom_point(size = 1.4) + 
    policy_lines + 
    labs(x = "Date", y = "Prevalence_Migrants - Prevalence_Natives") + minimal_theme
  
  print(p_c)
  
  ggsave(p_c, filename = paste0('Outputs/III_03/central_public_trends_',out ,'_gap_by_sector.png'))
  
  rm(p_c, prev_gap, prev_gap_svy_object)
  
}


## 4. Filter Data : from 2010Q1 to 2013Q4 ---------------------------------------------------

did_input <- prepare_did_data('central_civil_service', pop_interest%>%filter(year %in% c(2010:2013)))

