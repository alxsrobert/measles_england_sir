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
  n_samples <- 100
  
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
  pars_pmcmc_run <- readRDS("Output/cprd_degree/no.RDS")$pars
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
                                 waning = "no", 
                                 year_start = year_start, N_year = N_year)
    
  }
  if(exists("year_start")){
    all_output["Time",,] <- as.Date(paste0(year_start, "-01-01")) + all_output["Time",,] - 1
  }
 return(all_output)  
}
 


#### Analysis of all_output ####
reference <- create_scenario(scenario_name = "reference")
saveRDS(reference, file="Output/models/reference.rda")
rm(reference)
gc()

early_second <- create_scenario(scenario_name = "early slow")
saveRDS(early_second, file="Output/models/early_second.rda")
rm(early_second)
gc()

early_second_speedy <- create_scenario(scenario_name = "early speedy")
saveRDS(early_second_speedy, file="Output/models/early_second_speedy.rda")
rm(early_second_speedy)
gc()


MMR2_at_5 <- create_scenario(scenario_name = "MMR2_at5")
saveRDS(MMR2_at_5, file="Output/models/MMR2_at_5.rda")
rm(MMR2_at_5)
gc()

MMR2_as_MMR1 <- create_scenario(scenario_name = "MMR2_as_MMR1")
saveRDS(MMR2_as_MMR1, file="Output/models/MMR2_as_MMR1.rda")
rm(MMR2_as_MMR1)
gc()

D2_earlyplus025 <- create_scenario(scenario_name = "D2_earlyplus025")
saveRDS(D2_earlyplus025, file="Output/models/D2_earlyplus025.rda")
rm(D2_earlyplus025)
gc()


D2_earlyplus05 <- create_scenario(scenario_name = "D2_earlyplus05")
saveRDS(D2_earlyplus05, file="Output/models/D2_earlyplus05.rda")
rm(D2_earlyplus05)
gc()


D2_earlyplus1 <- create_scenario(scenario_name = "D2_earlyplus1")
saveRDS(D2_earlyplus1, file="Output/models/D2_earlyplus1.rda")
rm(D2_earlyplus1)
gc()


D2_3 <- create_scenario(scenario_name = "D2_3")
saveRDS(D2_3, file="Output/models/D2_3.rda")
rm(D2_3)
gc()

D1_1 <- create_scenario(scenario_name = "D1_1")
saveRDS(D1_1, file="Output/models/D1_1.rda")
rm(D1_1)
gc()

D2_1 <- create_scenario(scenario_name = "D2_1")
saveRDS(D2_1, file="Output/models/D2_1.rda")
rm(D2_1)
gc()

D1_05 <- create_scenario(scenario_name = "D1_05")
saveRDS(D1_05, file="Output/models/D1_05.rda")
rm(D1_05)
gc()

D2_05 <- create_scenario(scenario_name = "D2_05")
saveRDS(D2_05, file="Output/models/D2_05.rda")
rm(D2_05)
gc()

D1_025 <- create_scenario(scenario_name = "D1_025")
saveRDS(D1_025, file="Output/models/D1_025.rda")
rm(D1_025)
gc()


D2_025 <- create_scenario(scenario_name = "D2_025")
saveRDS(D2_025, file="Output/models/D2_025.rda")
rm(D2_025)
gc()


D2_minus3 <- create_scenario(scenario_name = "earlyminus3")
saveRDS(D2_minus3, file="Output/models/D2_minus3.rda")
rm(D2_minus3)
gc()

D2_minus5 <- create_scenario(scenario_name = "earlyminus5")
saveRDS(D2_minus5, file="Output/models/D2_minus5.rda")
rm(D2_minus5)
gc()



#---summary table for results
all_models <- list.files("Output/models/")
tmp <- readRDS(paste0("Output/models/", all_models[1]))
tmp <- tmp[grep("new_I", rownames(tmp)), ,]
summary_table <- summary(apply(tmp, 2, sum))
rm(tmp)
gc()



for(i in 2:length(all_models)){
  tmp <- readRDS(paste0("Output/models/", all_models[i]))
  tmp <- tmp[grep("new_I", rownames(tmp)), ,]
  row <- summary(apply(tmp, 2, sum))
  rm(tmp)
  summary_table <- rbind(summary_table, row)
  gc()
}


summary_table <- cbind(c("MMR1 +0.25","MMR1 +0.5","MMR1 +1",
                         "MMR2 +0.25","MMR2 +0.5","MMR2 +1",
                         "MMR2 + 3",
                         "early +0.25", "early +0.5","early +1",
                         "MMR2 -3","MMR2 -5","early second",
                          "early second fast","early MMR2 like MMR1",
                         "MMR2 at 5", "reference"), summary_table)
summary_table <- as.data.table(summary_table)
summary_table[, result := paste0(`Median`, " (", `1st Qu.`, " ;", `3rd Qu.`, ")")]
med_ref <- as.numeric(summary_table$Median[1])
summary_table[, Median := as.numeric(Median)]
summary_table[, `1st Qu.` := as.numeric(`1st Qu.`)]
summary_table[, `3rd Qu.` := as.numeric(`3rd Qu.`)]
summary_table[, diff_per := paste0(round(100-((Median/med_ref)*100),digits = 2),
                                 " (" ,round(100-((`1st Qu.`/med_ref)*100), digits = 2),
                                 " ;", round(100-((`3rd Qu.`/med_ref)*100), digits = 2), ")")]

write.csv2(summary_table, file = file_path)


#comparing the scenarios in graphs
#imrproving coverage
plot1 <- yearly_cases_fig_flexible_new("reference.rda","D2_1.rda"  , 
                                   "Reference","MMR2 +1%",
                                   "#2c5985", "#c4263e")
plot2 <- yearly_cases_fig_flexible_new("D2_1.rda", "D2_3.rda",
                                   "MMR2 + 1%","MMR2 +3%", 
                                   "#c4263e", "#3a95b1")
plot3 <- yearly_cases_fig_flexible_new("D2_3.rda", "D1_1.rda",
                                   "MMR2 +3%","MMR1 +1%", 
                                   "#3a95b1","#ed5f54" )

library(cowplot)
  plt <- plot_grid(plot1, plot2, plot3,
                   ncol = 1, nrow = 3, 
                   labels = c('A', 'B', 'C'),
                   label_size = 22,
                   label_y = 1.01,
                   label_x = 0.01,
                   scale = 0.9)
  ggsave(file_path,
         plt,
         width =  6,
         height = 14,
         bg = "white")

#changing schedule
plot1 <- yearly_cases_fig_flexible_higher_y("reference.rda", "MMR2_at_5.rda",
                                           "Reference","School entry MMR2", 
                                           "#2c5985","#c4263e")
plot2<- yearly_cases_fig_flexible_new("reference.rda", "early_second.rda",
                                   "Reference","Early MMR2", 
                                   "#2c5985","#ed5f54")
plot3 <- yearly_cases_fig_flexible_new("early_second.rda", "D2_earlyplus1.rda",
                                   "Early MMR2","Early MMR2 +1%", 
                                   "#ed5f54","#3a95b1")
plot4 <- yearly_cases_fig_flexible_new("D2_earlyplus1.rda", "MMR2_as_MMR1.rda",
                                   "Early MMR2 +1%","Early MMR2 like MMR1", 
                                   "#3a95b1","#f77964")
plot5 <- yearly_cases_fig_flexible_new("early_second.rda", "D2_minus3.rda",
                                   "Early MMR2","Early MMR2 -3%", 
                                   "#ed5f54","#2e5b88")
plot6 <- yearly_cases_fig_flexible_new("early_second.rda", "D2_minus5.rda",
                                   "Early MMR2","Early MMR2 -5%", 
                                   "#ed5f54","#2a5783")

#improving coverage vs the schedule
plt <- plot_grid(plot1, plot2, plot3, plot4, 
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

