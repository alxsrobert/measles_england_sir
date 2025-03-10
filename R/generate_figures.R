burnin <- 1000
thin <- 25

source("R/import_library.R")

## Generate model specifications
list_specs_run <- specs_run()

## Compute the total number of cases per region across simulations
data_anoun <- import_case_data(
  list_specs_run$state_names, list_specs_run$regions, list_specs_run$age, 
  list_specs_run$vacc_yes, list_specs_run$year_start, list_specs_run$N_year,
  anoun = TRUE)

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
for(i in seq_along(input_parameters)){
  # Extract vax, distance and sec from the input_parameters list
  vax <- input_parameters[[i]]$vax
  distance <- input_parameters[[i]]$distance
  sec <- input_parameters[[i]]$sec
  vacc_70s <- input_parameters[[i]]$vacc_70s
  
  ## Repository containing the simulations and parameter fits
  repos_i <- paste0("Output/", vax, "_", distance, if(sec) "_sec", if(vacc_70s) "_vacc70s")
  
  ## Repository where the figures will be saved
  repos_figures_i <- paste0(repos_i, "/figures/")
  
  ## If repos_figures_i does not exist, create it
  if (!file.exists(repos_figures_i)){
    dir.create(file.path(repos_figures_i))
  }
  
  ### Import files containing the simulations generated for this scenario
  ## Without waning
  all_output_no <- readRDS(paste0(repos_i, "/sim_no.RDS"))
  ## With waning since vaccination
  all_output_since_vax <- readRDS(paste0(repos_i, "/sim_since_vax.RDS"))
  ## With waning since elimination
  all_output_since_eli <- readRDS(paste0(repos_i, "/sim_since_eli.RDS"))
  ## With waning since elimination, removing waning
  all_output_since_eli_nowane <- readRDS(paste0(repos_i, "/sim_since_eli_nowane.RDS"))
  
  ## Aggregate no waning, waning since vax and waining since eli in a list
  list_output <- list("no waning" = all_output_no, 
    "with waning" = all_output_since_vax,
    "with waning starting in 2000" = all_output_since_eli
  )
  ## Aggregate waining since eli and waning since eli (removed waning) in a list
  list_output_nowane <- list(
    "waning starting in 2000" = all_output_since_eli,
    "waning starting in 2000, setting waning to 0" = all_output_since_eli_nowane
  )
  
  ## Import files containing the model fits generated for this scenario and 
  ## aggregate them in a list
  pmcmc_run_no <- readRDS(paste0(repos_i, "/no.RDS"))
  pmcmc_run_since_eli <- readRDS(paste0(repos_i, "/since_eli.RDS"))
  pmcmc_run_since_vax <- readRDS(paste0(repos_i, "/since_vax.RDS"))
  
  list_pmcmc_run <- list("no waning" = pmcmc_run_no, 
                         "with waning" = pmcmc_run_since_vax,
                         "with waning starting in 2000" = pmcmc_run_since_eli
  )
  
  ## If "sec" is true, "no waning" contains a constant risk of secondary vaccine failure
  if(sec){
    names(list_pmcmc_run)[1] <- "constant secondary vaccine failure"
    names(list_output)[1] <- "constant secondary vaccine failure"
  }
  
  ## Figure 3: age distribution of vaccinated cases per simulation set
  png(paste0(repos_figures_i, "age_vaccinated.png"), width = 600, height = 500)
  plot_figure_vax_distrib(list_output, data_anoun, list_specs_run$age, list_specs_run$regions)
  dev.off()
  
  ## Figure 4: time distribution of vaccinated cases per simulation set
  png(paste0(repos_figures_i, "year_vaccinated.png"), width = 500, height = 400)
  plot_figure_vax_year(list_output, data_anoun)
  dev.off()
  
  ## Figure 5: values of parameters in models including waning
  png(paste0(repos_figures_i, "parameter_waning_only.png"), width = 500, height = 500)
  plot_figure_parameters(list_pmcmc_run[c(2,3)], list_specs_run, burnin, thin, 
                         vax, sec, vacc_70s, 
                         distance, main = T)
  dev.off()  
  
  ### Supplement figures
  # Distribution of cases by year, region and age group
  png(paste0(repos_figures_i, "age_region_all.png"), width = 500, height = 500)
  plot_figure_region_age(list_output, data_anoun, list_specs_run$age, list_specs_run$regions)
  dev.off() 
  
  ## Parameter values
  png(paste0(repos_figures_i, "parameter_all_models.png"), width = 500, height = 600)
  plot_figure_parameters(list_pmcmc_run, list_specs_run, burnin, thin, vax, sec,
                         # vacc_70s, 
                         distance, main = F)
  dev.off()
  
  ## Impact of removing waning from the since_elimination scenario
  png(paste0(repos_figures_i, "waning_remove.png"), width = 500, height = 600)
  plot_figure_nowane(list_output_nowane, data_anoun)
  dev.off() 
  
  ## Seasonality
  png(paste0(repos_figures_i, "seasonality.png"), width = 500, height = 400)
  plot_figure_season(list_pmcmc_run)
  dev.off()
  
  ## Posterior distribution
  png(paste0(repos_figures_i, "posterior.png"), width = 500, height = 400)
  plot_posterior(list_pmcmc_run, burnin, thin)
  dev.off()
  
  ## Density of parameter estimates
  png(paste0(repos_figures_i, "parameter_density.png"), width = 900, height = 600)
  plot_dens_param(list_pmcmc_run, burnin, thin)
  dev.off()
}
