## Run odin.dust model
run_model <- function(list_model, vax, n_steps, waning, sec, distance, vacc_70s){
  # Import all specs
  list_specs_run <- specs_run()
  
  ## Import list containing all data
  all_data <- import_all_data(
    year_start = list_specs_run$year_start, N_year = list_specs_run$N_year,
    regions = list_specs_run$regions, age = list_specs_run$age, 
    scenario = list_specs_run$scenario, vax = vax)
  
  ## Initialise list of parameters
  mcmc_pars <- create_mcmc_pars(
    vacc_yes = list_specs_run$vacc_yes, year_start = list_specs_run$year_start, 
    N_time = list_specs_run$N_time, vax = vax, waning = waning, 
    distance = distance, sec = sec, vacc_70s = vacc_70s, list_data = all_data)
  
  ## Run model
  set.seed(1)
  pmcmc_run <- mcstate::pmcmc(
    mcmc_pars, list_model$filter, control = list_model$control)
  
  return(pmcmc_run)
}

## Create control and filter objects, which are the same in all scenarios
initialise_model <- function(n_steps, anoun = FALSE){
  list_specs_run <- specs_run()
  
  ## Import odin.dust model
  si_age <- odin.dust::odin_dust("R/model_odin_dust.R")
  
  ## Import data
  particle_data <- create_part_data(state_names = list_specs_run$state_names, 
                                    regions = list_specs_run$regions, 
                                    age = list_specs_run$age, 
                                    vacc_yes = list_specs_run$vacc_yes, 
                                    year_start = list_specs_run$year_start,
                                    N_year = list_specs_run$N_year, anoun = anoun)
  
  ## Initialise the model using the parameters previously defined
  filter <- mcstate::particle_deterministic$new(
    particle_data, si_age, case_compare, index = index
  )
  
  ## Create control object
  control <- mcstate::pmcmc_control(
    n_steps = n_steps, save_state = FALSE,  adaptive_proposal = TRUE, 
    save_trajectories = FALSE, progress = TRUE)
  
  return(list(control = control, filter = filter))
}

## Set filter function  
case_compare <- function(state, observed, pars = NULL) {
  incidence_modelled <- state[,, drop = TRUE]
  ## Remove the first four entries of observed (date_start, date_end, time_start, time_end)
  incidence_observed <- unlist(observed)[-seq_len(4)]
  lambda <- incidence_modelled + 1e-10
  if(length(incidence_modelled) > length(incidence_observed)){
    colSums(dpois(x = incidence_observed, lambda = lambda, log = TRUE))
  } else sum(dpois(x = incidence_observed, lambda = lambda, log = TRUE))
}

## Function to return only states IS, IV1, and IV2
index <- function(info) {
  list(run = c(new_IS = info$index$new_IS
               , new_IV1 = info$index$new_IV1,
               new_IV2 = info$index$new_IV2
  ), state = c())
}

## Set parameters fixed across all scenarios
specs_run <- function(){
  scenario <- "reference"
  vacc_yes <- "v1"
  catchup <- TRUE
  
  ## Duration of the run (in days)
  N_year <- 10 
  N_time <- t_tot <- 365 * N_year
  year_start <- 2010
  
  ## Define time step
  dt <- 1
  
  state_names <- c("new_IS", "new_IV1", "new_IV2")
  regions <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
               "West Midlands", "East", "London", "South East", "South West")
  age <- c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
           "[15,20)", "[20,30)", "[30,40)", "[40,100]"
  )

  return(list(state_names = state_names, regions = regions, age = age, N_year = N_year,
              vacc_yes = vacc_yes, year_start = year_start, N_time = N_time, 
              scenario = scenario, catchup = catchup))
}

## Create particle_filter_data object, which contains the data
create_part_data <- function(state_names, regions, age, vacc_yes, year_start, N_year, 
                             anoun = TRUE){
  ## Import case data
  data_anoun <- import_case_data(state_names, regions, age, vacc_yes, year_start, 
                                 N_year, anoun)
  
  ## Create unique row id
  data_anoun[, population := paste(vaccinated, region, age_groups, sep = "_")]
  
  ## Create levels of row id and set population as factor
  levels_pop <- c(paste(rep(paste(rep(state_names, each = length(regions)),
                                  rep(toupper(regions), length(state_names)), sep = "_"),
                            each = length(age)), age, sep = "_")
  )
  data_anoun[, population := factor(population, levels = levels_pop)]
  data_anoun <- data_anoun[order(population),]
  
  ## Rename column "N" into "cases
  data_anoun[, cases := N]
  data_anoun[, N := NULL]
  
  ## Set date as numeriw
  data_anoun[, date := as.numeric(date)]
  ## Select columns dates, cases, and population
  data_anoun <- data_anoun[, .(date, cases, population)]
  data_anoun <- pivot_wider(data_anoun, names_from = "population", values_from = "cases")
  
  ## Create odin data object
  particle_data <- mcstate::particle_filter_data(data_anoun, time = "date", rate = 1
                                                 , initial_time = 0
  )
  return(particle_data)
}
