source("R/function_import_data.R")
source("R/function_generate_outbreak.R")
source("R/function_figures.R")

## Import odin.dust model
si_age <- odin.dust::odin_dust("R/model_odin_dust.R")
options(scipen = 999)

## Import libraries 

library(dplyr)
library(socialmixr)
library(odin.dust)
library(mcstate)
library(tictoc)
library(tidyr)
library(ggplot2)
library(data.table)



create_scenario <- function(scenario_name){
  
  
  scenario <- scenario_name
  # Number of simulations per sample
  n_part <- 25
  n_samples <- 10
  
  #### Import data and model fit ####
  
  year_start <- 2010
  N_year <- 10
  N_time <- t_tot <- 365 * N_year
  
  
  age <- c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
           "[15,20)", "[20,30)", "[30,40)", "[40,100]")
  regions <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
               "West Midlands", "East", "London", "South East", "South West")
  
  ## Import the different data streams into a list.
  # Use scenario to move between vaccine scenarios (early / early_timely etc..)
  all_data <- import_all_data(year_start = year_start, N_year = N_year, 
                              scenario = scenario, vax = "cprd", regions = regions, 
                              age = age)
  
  
  ## Import the parameter estimates
  pars_pmcmc_run <- readRDS("Output/cprd_degree/since_vax.RDS")$pars
  ## Remove burnin
  samples <- pars_pmcmc_run[-c(1:5000),]
  ## Thin the estimates
  samples <- (samples[seq(1, nrow(samples), nrow(samples)/n_samples),] %>% unique)
  if(any(colnames(samples) == "vacc1")) 
    colnames(samples)[colnames(samples) == "vacc1"] <- "vacc"
  
  ## Define array output_sim, containing the number of individuals in each compartment 
  ## per day in each simulation
  states <- c("new_IS", "new_IV1", "new_IV2")
  all_output <- array(NA, dim = c(2 + length(states) * all_data$N_age * all_data$N_reg, 
                                  nrow(samples) * n_part, t_tot))
  
  
  ## Rename the rows of output_sim, as the name of the state + region + age group
  ## The first row of output_sim contains the time
  rownames(all_output) <- c("Time", "iter", 
                            paste0(rep(paste0(rep(states, each = all_data$N_reg), "_reg", 
                                              rep(seq_len(all_data$N_reg), length(states))), 
                                       each = all_data$N_age), "_age", seq_len(all_data$N_age))
  )
  all_output["Time",,] <- as.Date(paste0(year_start, "-01-01")) + all_output["Time",,] - 1
  
  ## Generate n_part simulation per sampled parameter estimate
  for(k in seq_len(nrow(samples))){
    all_output[, seq((k-1) * n_part + 1, (k) * n_part),] <- 
      generate_outbreaks_1sample(sample = samples[k,], model = si_age, data = all_data,
                                 states = rownames(all_output), n_part = n_part, 
                                 waning = "since_vax", 
                                 year_start = year_start, N_year = N_year)
    
  }
  if(exists("year_start")){
    all_output["Time",,] <- as.Date(paste0(year_start, "-01-01")) + all_output["Time",,] - 1
  }
  return(all_output)  
}



#### Analysis of all_output ####
reference <- create_scenario(scenario_name = "reference")
early_second <- create_scenario(scenario_name = "early slow")
MMR2_at_5 <- create_scenario(scenario_name = "MMR2_at5")
#late_second <- create_scenario(scenario_name = "old")
D2_earlyplus1<- create_scenario(scenario_name = "D2_earlyplus1")
D1_1 <- create_scenario(scenario_name = "D1_1")
MMR2_as_MMR1<- create_scenario(scenario_name = "MMR2_as_MMR1")
D2_minus3 <- create_scenario(scenario_name = "earlyminus3")
D2_minus5 <- create_scenario(scenario_name = "earlyminus5")

#---table of cases per scenario
s1 <- (summary(apply(reference[grep("new_I", rownames(reference)), ,], 2, sum)))
s2 <- (summary(apply(early_second[grep("new_I", rownames(early_second)), ,], 2, sum)))
s3 <- (summary(apply(MMR2_at_5[grep("new_I", rownames(MMR2_at_5)), ,], 2, sum)))
s4 <- (summary(apply(D1_1[grep("new_I", rownames(D1_1)), ,], 2, sum)))
s5 <- (summary(apply(MMR2_as_MMR1[grep("new_I", rownames(MMR2_as_MMR1)), ,], 2, sum)))
s6 <- (summary(apply(D2_minus3[grep("new_I", rownames(D2_minus3)), ,], 2, sum)))
s7 <- (summary(apply(D2_minus5[grep("new_I", rownames(D2_minus5)), ,], 2, sum)))


summary_tab <- rbind(s1, s2, s3, s4, s5, s6, s7)
summary_tab <- cbind(c("reference", "early second",
                       "late second", "MMR1 +1",
                       "early MMR2 like MMR1",
                       "MMR2 -3", "MMR2 -5"), summary_tab)
summary_tab <- as.data.table(summary_tab)
summary_tab[, result := paste0(`Median`, " (", `1st Qu.`, " ;", `3rd Qu.`, ")")]
med_ref <- as.numeric(summary_tab$Median[1])
summary_tab[, Median := as.numeric(Median)]
summary_tab[, `1st Qu.` := as.numeric(`1st Qu.`)]
summary_tab[, `3rd Qu.` := as.numeric(`3rd Qu.`)]
summary_tab[, diff_per := paste0(round(100-((Median/med_ref)*100),digits = 2),
                                 " (" ,round(100-((`1st Qu.`/med_ref)*100), digits = 2),
                                 " ;", round(100-((`3rd Qu.`/med_ref)*100), digits = 2), ")")]

write.csv2(summary_tab, file = file_path)



#changing schedule
plot1 <- yearly_cases_fig_flexible_higher_y(reference, MMR2_at_5,
                                            "Reference","School entry MMR2", 
                                            "#2c5985","#c4263e")
plot2<- yearly_cases_fig_flexible(reference, early_second,
                                  "Reference","Early MMR2", 
                                  "#2c5985","#ed5f54")
plot3 <- yearly_cases_fig_flexible(early_second, D1_1,
                                   "Early MMR2","MMR +1%", 
                                   "#ed5f54","#3a95b1")
plot4 <- yearly_cases_fig_flexible(D1_1, MMR2_as_MMR1,
                                   "MMR1 +1%","Early MMR2 like MMR1", 
                                   "#3a95b1","#f77964")
plot5 <- yearly_cases_fig_flexible(early_second, D2_minus3,
                                   "Early MMR2","Early MMR2 -3%", 
                                   "#ed5f54","#2e5b88")
plot6 <- yearly_cases_fig_flexible(early_second, D2_minus5,
                                   "Early MMR2","Early MMR2 -5%", 
                                   "#ed5f54","#2a5783")

#improving coverage vs the schedule
plt <- cowplot::plot_grid(plot1, plot2, plot3, plot4, 
                 plot5, plot6,
                 ncol = 2, nrow = 3, 
                 labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)
ggsave(file_path,
       plt,
       width =  12,
       height = 14,
       bg = "white")

