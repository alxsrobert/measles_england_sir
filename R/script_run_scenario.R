
source("R/function_import_data.R")
source("R/function_generate_outbreak.R")

## Import odin.dust model
si_age <- odin.dust::odin_dust("R/model_odin_dust.R")

## Import libraries 

library(dplyr)
library(socialmixr)
library(odin.dust)
library(mcstate)
library(tictoc)
library(tidyr)
library(ggplot2)
library(data.table)


scenario <- "reference"
# Number of simulations per sample
n_part <- 100
n_samples <- 10

#### Import data and model fit ####

year_start <- 2010
N_year <- 10
N_time <- t_tot <- 365 * N_year


age <- c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
         "[15,20)", "[20,30)", "[30,40)", "[40,100]"
)
regions <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
             "West Midlands", "East", "London", "South East", "South West")

## Import the different data streams into a list.
# Use scenario to move between vaccine scenarios (early / early_timely etc..)
all_data <- import_all_data(year_start = year_start, N_year = N_year, 
                            scenario = scenario, vax = "cprd", regions = regions, 
                            age = age)


## Import the parameter estimates
pars_pmcmc_run <- readRDS("Output/v1_cprd_10k_peryear_catchup_pars.RDS")
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
                               waning = "no", scenario_import = "per_year", 
                               year_start = year_start, N_year = N_year)
}



#### Analysis of all_output ####

sum_i <- numeric(ncol(all_output))

for(i in seq_along(sum_i)) 
  sum_i[i] <- sum(all_output[grep("new_I", rownames(all_output)), i,])

# Total number of cases per simulation:
print(summary(sum_i))
