# Function to generate n_samples * n_part stochastic outbreaks for a given model output
generate_outbreaks <- function(pmcmc_run, list_specs_run, vax, n_part, n_samples, 
                               waning, burnin, nowane = FALSE, aggreg = "year"){
  samples <- pmcmc_run$pars[-c(1:burnin),]
  samples <- (samples[seq(1, nrow(samples), nrow(samples)/n_samples),] %>% unique)
  if(nowane) samples[, "v_leak"] <- 0
  
  ## Import odin.dust model
  si_age <- odin.dust::odin_dust("R/model_odin_dust.R")
  
  ## Import list containing all data
  all_data <- import_all_data(
    year_start = list_specs_run$year_start, N_year = list_specs_run$N_year,
    regions = list_specs_run$regions, age = list_specs_run$age, 
    scenario = list_specs_run$scenario, vax = vax)
  
  # Initialise empty 3-d array, which will contain the number of cases per 
  # vaccination status, age, region, simulation, and day
  all_output <- array(NA, dim = c(
    length(list_specs_run$state_names) * length(list_specs_run$age) * 
      length(list_specs_run$regions), nrow(samples) * n_part, 
    if(aggreg == "year") list_specs_run$N_year else list_specs_run$N_time))
  # The rownames of all_output is a combination of vaccination status, age, and region
  rownames(all_output) <- 
    c(paste0(rep(paste0(
      rep(list_specs_run$state_names, each = length(list_specs_run$regions)), 
      "_reg", rep(seq_along(list_specs_run$regions), length(list_specs_run$state_names))), 
      each = length(list_specs_run$age)), "_age", seq_along(list_specs_run$age))
    )
  # for each element of sample, generate n_part stochastic outbreaks
  for(k in seq_len(nrow(samples))){
    all_output[, seq((k-1) * n_part + 1, (k) * n_part),] <- 
      generate_outbreaks_1sample(
        sample = samples[k,], model = si_age, data = all_data, aggreg = aggreg,
        states = rownames(all_output), n_part = n_part, waning = waning, 
        year_start = list_specs_run$year_start, N_year = list_specs_run$N_year)
  }
  return(all_output)
}

# Generate n_part simulations from a given sample
# sample: vector containing the value of the different parameters
# model: dust model created with the function odin.dust::odin_dust()
# data: list containing all datasets imported with the function import_all_data()
# states: vector containing the states that will be returned from the model run
# waning: is there waning of immunity? "no", "since_eli", or "since_vax"
# year_start: starting year of the simulations
generate_outbreaks_1sample <- function(sample, model, data, states, 
                                       waning, year_start, 
                                       n_part = 1, N_year = 10, dt = 1, 
                                       aggreg = "no"){
  
  ## Define number of contacts
  beta <- sample["beta"]
  
  ## Define vaccine effectiveness against infection
  # Primary vaccine failure
  v_fail <- sample["v_fail"]
  # secondary vaccine failure (increasing with age)
  if(any(names(sample) == "v_leak")) v_leak <- sample["v_leak"]
  # Secondary vaccine failure (constant)
  if(any(names(sample) == "v_sec")) v_sec <- sample["v_sec"]
  # Against onwards infection
  vacc <- sample["vacc"]
  
  ## Define spatial kernel parameter
  b <- if(any(names(sample) == "b")) sample["b"] else 1
  c <- if(any(names(sample) == "c")) sample["c"] else 1
  theta <- if(any(names(sample) == "theta")) sample["theta"] else 1
  
  
  ## Define the seasonality parameters
  X <- sample["X"]
  Y <- sample["Y"]
  X_import <- sample["X_import"]
  Y_import <- sample["Y_import"]
  report_import <- sample["report_import"]
  delta <- sample["delta"]
  
  ## Duration of the run (in days)
  N_time <- t_tot <- 365 * N_year
  
  ## Extract parameters from the "sample" vector
  catchup <- if(any(names(sample) == "catchup")) sample["catchup"] else 0
  catchup2 <- if(any(names(sample) == "catchup2")) sample["catchup2"] else 0
  recov11to15 <- sample["recov11to15"]
  recov16to20 <- sample["recov16to20"]
  recov21to30 <- sample["recov21to30"]   
  recov31to40 <- sample["recov31to40"]   
  recov40plus <- sample["recov40plus"]
  
  # Extract proportion of recovered from the samples
  recov <- data$R * 0
  recov["age11to15", ] <- recov11to15
  recov["age16to20", ] <- recov16to20
  recov["age21to30", ] <- recov21to30
  recov["age31to40", ] <- recov31to40
  recov["age40plus", ] <- recov40plus
  
  # Compute the proportion of vaccinated in each region / age group
  V_tot <- data$V1 + data$V2
  # Adults in 20-30 who were vaccinated during the MMR2 catchup in 1996 are set as V2
  data$V1["age21to30",] <- round(V_tot["age21to30", ] * (1 - catchup))
  data$V2["age21to30",] <- round(V_tot["age21to30", ] * (catchup))
  
  # Adults in 6-10 who were vaccinated during the catchup campaigns in 2008 and 2013 
  # are set as V2
  data$S["age6to9",] <- round(data$S["age6to9",] * (1 - catchup2))
  data$V1["age6to9",] <- round(data$V1["age6to9",] * (1 - catchup2))
  data$V2["age6to9",] <- round(data$V2["age6to9",] + 
                                 data$S["age6to9",] * catchup2 +
                                 data$V1["age6to9",] * catchup2)
  S <- data$S
  R <- data$R
  
  
  # Compute the number of importations per region 
  import_per_reg <- data$mean_import_per_reg
  
  mean_import <- import_per_reg / report_import
  
  ## Create the age and region-stratified model
  seir_model <- model$new(pars = list(
    m = data$ref_m, d = data$ref_d, b = b, c = c, theta = theta,
    beta = beta, X = X, Y = Y, delta = delta, X_import = X_import, 
    Y_import = Y_import, v_fail = v_fail, vacc = vacc,
    v_leak = if(exists("v_leak")) v_leak else 0, 
    v_sec = if(exists("v_sec")) v_sec else 0, 
    mean_import = mean_import, 
    N_time = N_time, N_age = data$N_age, N_reg = data$N_reg, V1_init = data$V1, 
    V2_init = data$V2, 
    Es_init = S * 0, Ev1_init = S * 0, Ev2_init = S * 0,
    Is_init = S * 0, Iv1_init = S * 0, Iv2_init = S * 0, 
    S_init = S + R, 
    recov = recov, 
    R_init = R * 0, RV1_init = R * 0, RV2_init = R * 0,
    
    array_cov1 = data$array_cov1[-1,,], array_cov2 = data$array_cov2[-1,,],
    array_new = data$new_birth, 
    dt = dt, 
    waning = if(waning == "no") 0 else if(waning == "since_vax") 1 else
      if (waning == "since_eli") 2,
    year_start = year_start, year_per_age = data$year_per_age
  ),
  time = 1, n_particles = n_part, n_threads = 1L, seed = 1L)
  
  ## Define array output_sim, containing the number of individuals in each compartment per day
  ## in each simulation
  output_sim <- array(NA, dim = c(seir_model$info()$len, n_part, t_tot))
  
  ## Rename the rows of output_sim, as the name of the state + region + age group
  all_states <- c("S", "M", "V1", "V2", "V1p", "V2p", "Es", "Ev1", "Ev2", "Is", 
                  "Iv1", "Iv2", "R", "RV1", "RV2", "new_IS", "new_IV1", "new_IV2")
  ## The first row of output_sim contains the time
  rownames(output_sim) <- c(
    "Time", "iter", "new_IV1_tot", "new_IV2_tot", 
    paste0(rep(paste0(rep(all_states, each = nrow(data$ref_d)), "_reg", 
                      rep(seq_len(nrow(data$ref_d)), length(all_states))), 
               each = nrow(data$ref_m)), "_age", seq_len(nrow(data$ref_m)))
  )
  
  ## Run the model for each time step
  if(aggreg == "year"){
    output_year <- array(NA, dim = c(length(states), ncol(output_sim), dim(output_sim)[3]/365),
                         dimnames = list(states))
  }
  for (t in seq_len(t_tot)) {
    ## Generate stochastic simulations
    output_sim[ , , t] <- seir_model$run(t)
    if(t%%365.25 < 1 & aggreg == "year"){
      # aggregate by year
      iter <- t%/%365.25
      output_year[,,iter] <- 
        apply(output_sim[states, , seq_len(t_tot) %/% 365.25 == (iter - 1)], 
              c(1,2), sum)
    } else if(t == t_tot & aggreg == "year"){
      # aggregate last year of simulation
      iter <- iter + 1
      output_year[,,iter] <- 
        apply(output_sim[states, , seq_len(t_tot) %/% 365.25 == (iter - 1)], 
              c(1,2), sum)
    }
  }
  
  if(aggreg == "tot") return(apply(output_sim[states, , ], c(1,2), sum)) else 
    if(aggreg == "year") return(output_year) else
      return(output_sim[states, , ])
}

