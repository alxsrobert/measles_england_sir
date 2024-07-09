n_steps <- 20000

source("R/import_library.R")

## Generate model specifications
specs_model <- initialise_model(n_steps, anoun = TRUE)

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
    ## Run model
    set.seed(1)
    pmcmc_run <- run_model(
      list_model = specs_model, vax = vax, n_steps = n_steps, 
      distance = distance, waning = waning, sec = sec, vacc_70s = vacc_70s)
    ## Save output
    saveRDS(pmcmc_run, paste0("Output/", vax, "_", distance, if(sec) "_sec", 
                              if(vacc_70s) "_vacc70s", "/", waning, ".RDS"))
  }
}
