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
  list(list(vax = "cprd", distance = "degree", sec = FALSE), 
       list(vax = "cprd", distance = "degree", sec = TRUE), 
       list(vax = "cover", distance = "degree", sec = FALSE),
       list(vax = "cprd", distance = "fixed", sec = FALSE))

## Three waning scenarios
vec_waning <- c("no", "since_eli", "since_vax")

for(i in seq_along(input_parameters)){
  # Extract vax, distance and sec from the input_parameters list
  vax <- input_parameters[[i]]$vax
  distance <- input_parameters[[i]]$distance
  sec <- input_parameters[[i]]$sec
  
  for(waning in vec_waning){
    ## Run model
    set.seed(1)
    pmcmc_run <- run_model(
      list_model = specs_model, vax = vax, n_steps = n_steps, 
      distance = distance, waning = waning, sec = sec)
    ## Save output
    saveRDS(pmcmc_run, paste0("Output/", vax, "_", distance, if(sec) "_sec", "/",
                              waning, ".RDS"))
    if(vacc_yes == "v1" & distance == "degree" & waning == "no" & 
       scenario_import == "per_year" & vax == "cprd" & 
       sec == FALSE)
      saveRDS(pmcmc_run$pars, "Output/v1_cprd_10k_peryear_catchup_pars.RDS")
  }
}
