n_samples <- 500
n_part <- 10

source("R/import_library.R")


## Generate model specifications
list_specs_run <- specs_run()

## Create the four different scenario
## - main scenario: cprd vaccine data, distance kernel parameter estimated, no baseline leak
## - sensitivity: baseline leak (constant with age)
## - sensitivity: cover vaccine data
## - sensitivity: fixed distance kernel
input_parameters <- 
  list(list(vax = "cprd", distance = "degree", sec = FALSE, vacc_70s = FALSE), 
       list(vax = "cprd", distance = "degree", sec = TRUE, vacc_70s = FALSE), 
       list(vax = "cover", distance = "degree", sec = FALSE, vacc_70s = FALSE),
       list(vax = "cprd", distance = "fixed", sec = FALSE, vacc_70s = FALSE),
       list(vax = "cprd", distance = "degree", sec = FALSE, vacc_70s = TRUE))

## Three waning scenarios
vec_waning <- c("no", "since_eli", "since_vax")


for(i in seq_along(input_parameters)){
  # Extract vax, distance and sec from the input_parameters list
  vax <- input_parameters[[i]]$vax
  distance <- input_parameters[[i]]$distance
  sec <- input_parameters[[i]]$sec
  vacc_70s <- input_parameters[[i]]$vacc_70s
  
  for(waning in vec_waning){
    ## Read object containing model and parameter fits
    pmcmc_run <- readRDS(paste0("Output/", vax, "_", distance, if(sec) "_sec", 
                                if(vacc_70s) "_vacc70s", "/", waning, ".RDS"))
    ## Generate stochastic simulations (n_part * n_samples simulations in total)
    all_output <- generate_outbreaks(pmcmc_run, list_specs_run, vax, n_part, n_samples, 
                                     waning, burnin = 1000)
    
    ## save 3d array containing the stochastic simulations per status and year
    saveRDS(all_output, paste0("Output/", vax, "_", distance, if(sec) "_sec", "/sim_",
                               waning, ".RDS"))
    if(waning == "since_eli"){
      ## Generate stochastic simulations (n_part * n_samples simulations in total) with waning removed
      all_output <- generate_outbreaks(pmcmc_run, list_specs_run, vax, n_part, n_samples, 
                                       waning, burnin = 1000, nowane = TRUE)
      ## save 3d array containing the stochastic simulations per status and year
      saveRDS(all_output, paste0("Output/", vax, "_", distance, if(sec) "_sec", "/sim_",
                                 waning, "_nowane.RDS"))
    }
  }
}

