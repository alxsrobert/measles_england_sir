## Function to create mcstate::pmcmc_parameters
create_mcmc_pars <- function(vacc_yes, vax, distance, waning, list_data, 
                             year_start, N_time, sec){
  ## Extract elements from list_data (generated with the function import_all_data)
  ref_d <- list_data[["ref_d"]]
  ref_m <- list_data[["ref_m"]]
  new_birth <- list_data[["new_birth"]]
  mean_import_per_reg <- list_data[["mean_import_per_reg"]]
  N_age <- list_data[["N_age"]]
  N_reg <- list_data[["N_reg"]]
  S <- list_data[["S"]]
  V1 <- list_data[["V1"]]
  V2 <- list_data[["V2"]]
  RV1 <- RV2 <- R <- list_data[["R"]]
  array_cov1 <- list_data[["array_cov1"]]
  array_cov2 <- list_data[["array_cov2"]]
  year_per_age <- list_data[["year_per_age"]]
  
  ## Create vector containing initial values of parameters 
  init <- create_init(vacc_yes, vax, distance, waning, sec)
  
  ## Extract elements from init as pcmc_parameter objects
  # Transmission parameter
  beta <- mcstate::pmcmc_parameter("beta", init["beta"],min = 0, max = 50)
  # Duration of maternal immunity
  delta <- mcstate::pmcmc_parameter("delta", init["delta"],min = 1/180, max = 1, prior = function(p)
    dnorm(1/p, mean = 90, sd = 5, log = TRUE))
  # Seasonality of transmission 
  X <- mcstate::pmcmc_parameter("X", init["X"],min = 0, max = 1)
  # Seasonality of transmission
  Y <- mcstate::pmcmc_parameter("Y", init["Y"],min = 0, max = 2 * pi)
  # Seasonality of importations
  X_import <- mcstate::pmcmc_parameter("X_import", init["X_import"],min = 0, max = 1)
  # Seasonality of importations
  Y_import <- mcstate::pmcmc_parameter("Y_import", init["Y_import"],min = 0, max = 2 * pi + 1)
  # Risk of primary vaccine failure
  v_fail <- mcstate::pmcmc_parameter("v_fail", init["v_fail"],min = 0, max = 1)
  # Risk of secondary vaccine failure increasing with age (if waning)
  if(waning != "no")
    v_leak <- mcstate::pmcmc_parameter("v_leak", init["v_leak"],min = 0, max = .01)
  # Risk of secondary vaccine failure constant with age (if sec)
  if(sec)
    v_sec <- mcstate::pmcmc_parameter("v_sec", init["v_sec"],min = 0, max = .01)
  
  # Proportion of unvaccinated cases vaccinated during the catchup campaigns
  catchup <- mcstate::pmcmc_parameter("catchup", init["catchup"],min = 0, max = 1)
  catchup2 <- mcstate::pmcmc_parameter("catchup2", init["catchup2"],min = 0, max = 1)
  
  # Proportion of onwards transmission caused by vaccinated cases, compared to unvaccinated cases 
  vacc <- mcstate::pmcmc_parameter("vacc", init["vacc"],min = 0, max = 1, prior = function(p)
    dbeta(p, shape1 = 10, shape2 = 10, log = TRUE))
  
  # Parameters to compute spatial kernel parameters
  if(distance != "fixed"){
    b <- mcstate::pmcmc_parameter("b", init["b"],min = 0, max = 5)
    c <- mcstate::pmcmc_parameter("c", init["c"],min = 0, max = 5)
    theta <- mcstate::pmcmc_parameter("theta", init["theta"],min = 0, max = 1)
  }
  
  # Proportion of importation reported
  report_import <- mcstate::pmcmc_parameter("report_import", init["report_import"],
                                            min = 0.2, max = 3)
  # Proportion of each age group recovered in 2010
  recov11to15 <- mcstate::pmcmc_parameter("recov11to15", init["recov11to15"],
                                          min = 0, max = 1)
  recov16to20 <- mcstate::pmcmc_parameter("recov16to20", init["recov16to20"],
                                          min = 0, max = 1)
  recov21to30 <- mcstate::pmcmc_parameter("recov21to30", init["recov21to30"],
                                          min = 0, max = 1)
  recov31to40 <- mcstate::pmcmc_parameter("recov31to40", init["recov31to40"],
                                          min = 0, max = 1)
  recov40plus <- mcstate::pmcmc_parameter("recov40plus", init["recov40plus"],
                                          min = 0, max = 1)
  
  ## Create list of parameters estimated during the fitting process
  pars <- list(
    beta, delta, X, Y, X_import, Y_import, v_fail, vacc, report_import
    , recov11to15, recov16to20, recov21to30, recov31to40, recov40plus
  )
  # If waning, add v_leak
  if(waning != "no") pars <- append(pars, list(v_leak))
  # If constant risk of leak from the vaccine, add v_sec
  if(sec) pars <- append(pars, list(v_sec))
  # If distance parameters are estimated, add theta, b, and c
  if(distance != "fixed") pars <- append(pars, list(theta, b, c))
  # Add catchup parameters
  pars <- append(pars, list(catchup, catchup2))
  
  # Create proposal matrix
  proposal_matrix <- 
    create_proposal(vacc_yes, vax, distance, waning, init, pars, sec)
  
  # Function for parameter transformation
  transform <- make_transform(
    pars,m = ref_m, d = ref_d, 
    import = mean_import_per_reg, N_time = N_time, N_age = N_age, N_reg = N_reg, 
    S_init = S, V1_init = V1, V2_init = V2, R_init = R, RV1_init = RV1, RV2_init = RV2, 
    # All E and I compartments start empty
    Es_init = S * 0, Ev1_init = S * 0, Ev2_init = S * 0, 
    Is_init = S * 0, Iv1_init = S * 0, Iv2_init = S * 0,
    
    array_cov1 = array_cov1[-1,,], array_cov2 = array_cov2[-1,,], array_new = new_birth, 
    
    waning = if(waning == "no") 0 else if(waning == "since_vax") 1 else
      if (waning == "since_eli") 2, 
    
    dt = 1, year_start = year_start, year_per_age = year_per_age,
    # If the following parameters are included in pars, this reference level will be ignored
    b = 1, c = 1, theta = 1, v_leak = 0, v_sec = 0
  )
  ## create pmcmc_parameters object
  mcmc_pars <- mcstate::pmcmc_parameters$new(
    parameters = pars, proposal = proposal_matrix, transform = transform
  )
  
  return(mcmc_pars)
}

## Transformation function to move between inferred parameters and parameters 
## needed to implement the model (see https://mrc-ide.github.io/mcstate/reference/pmcmc_parameters.html)
make_transform <- function(pars,m, d, import, N_time, N_age, N_reg, 
                           b, c, theta, v_leak, v_sec, 
                           S_init, V1_init, V2_init, Es_init, 
                           Ev1_init, Ev2_init, Is_init, Iv1_init, Iv2_init, 
                           R_init, RV1_init, RV2_init, array_cov1, array_cov2, 
                           array_new, dt, year_per_age, waning, 
                           year_start){
  function(pars){
    # Extract parameters from the vector pars
    beta <- if(length(pars) == 1) pars else pars[["beta"]]
    delta <- if(length(pars) == 1) pars else pars[["delta"]]
    X <- if(length(pars) == 1) pars else pars[["X"]]
    Y <- if(length(pars) == 1) pars else pars[["Y"]]
    X_import <- if(length(pars) == 1) pars else pars[["X_import"]]
    Y_import <- if(length(pars) == 1) pars else pars[["Y_import"]]
    v_fail <- if(length(pars) == 1) pars else pars[["v_fail"]]
    vacc <- if(length(pars) == 1) pars else pars[["vacc"]]
    catchup <- if(length(pars) == 1) pars else pars[["catchup"]]
    catchup2 <- if(length(pars) == 1) pars else pars[["catchup2"]] 
    report_import <- if(length(pars) == 1) pars else pars[["report_import"]]
    recov11to15 <- if(length(pars) == 1) pars else pars[["recov11to15"]]
    recov16to20 <- if(length(pars) == 1) pars else pars[["recov16to20"]]
    recov21to30 <- if(length(pars) == 1) pars else pars[["recov21to30"]]
    recov31to40 <- if(length(pars) == 1) pars else pars[["recov31to40"]]
    recov40plus <- if(length(pars) == 1) pars else pars[["recov40plus"]]
    
    # Extract optional parameters
    if(any(names(pars) == "v_leak"))
      v_leak <- if(length(pars) == 1) pars else pars[["v_leak"]]
    if(any(names(pars) == "v_sec"))
      v_sec <- if(length(pars) == 1) pars else pars[["v_sec"]]
    if(any(names(pars) == "b"))
      b <- if(length(pars) == 1) pars else pars[["b"]]
    if(any(names(pars) == "c"))
      c <- if(length(pars) == 1) pars else pars[["c"]]
    if(any(names(pars) == "theta"))
      theta <- if(length(pars) == 1) pars else pars[["theta"]]
    
    
    V_tot <- V1_init + V2_init
    # Adults in 20-30 who were vaccinated during the MMR2 catchup in 1996 are set as V2
    V1_init["age21to30",] <- round(V_tot["age21to30", ] * (1 - catchup))
    V2_init["age21to30",] <- round(V_tot["age21to30", ] * (catchup))
    
    # Adults in 6-10 who were vaccinated during the catchup campaigns in 2008 and 2013 
    # are set as V2
    S_init["age6to9",] <- round(S_init["age6to9",] * (1 - catchup2))
    V1_init["age6to9",] <- round(V1_init["age6to9",] * (1 - catchup2))
    V2_init["age6to9",] <- round(V2_init["age6to9",] + 
                                  S_init["age6to9",] * catchup2 +
                                  V1_init["age6to9",] * catchup2)
    
    # Set proportion of recovered in 2010 per age group
    recov <- R_init * 0
    recov["age11to15", ] <- recov11to15
    recov["age16to20", ] <- recov16to20
    recov["age21to30", ] <- recov21to30
    recov["age31to40", ] <- recov31to40
    recov["age40plus", ] <- recov40plus
    
    list(beta = beta, X = X, Y = Y, delta = delta, X_import = X_import, Y_import = Y_import,
         b = b, c = c, theta = theta, m = m, d = d, vacc = vacc, 
         mean_import = import / report_import, N_time = N_time, 
         N_age = N_age, N_reg = N_reg, v_fail = v_fail, v_leak = v_leak, v_sec = v_sec, 
         V1_init = V1_init, V2_init = V2_init, Es_init = Es_init, 
         Ev1_init = Ev1_init, Ev2_init = Ev2_init, Is_init = Is_init, 
         Iv1_init = Iv1_init, Iv2_init = Iv2_init, 
         
         recov = recov, R_init = R_init * 0, RV1_init = R_init * 0, RV2_init = R_init * 0,
         S_init = S_init + R_init,
         
         array_cov1 = array_cov1, array_cov2 = array_cov2, 
         array_new = array_new, dt = dt, year_per_age = year_per_age,
         waning = waning, year_start = year_start)
  }
}

# Create initial vector of parameters 
create_init <- function(vacc_yes, vax, distance, waning, sec){
  name_file <- paste0("Output/", vax, "_", distance, if(sec) "_sec", "/",
                      waning, ".RDS")
  # If the file already exists (i.e. the model has already been run), 
  # import the value from the saved file
  if(any(is.element(dir(recursive = T), name_file))) {
    init <- readRDS(name_file)$pars[10000,]
  } else {
    # Otherwise, create init 
    init <- c(beta = 1, delta = .1, X = .5, Y = 1, X_import = .5, Y_import = 1, 
              v_fail = .1, vacc = .5, report_import = .5, recov11to15 = 0, 
              recov16to20 = 0, recov21to30 = 0, recov31to40 = 0, recov40plus = 0, 
              catchup = .5, catchup2 = .5)
  }
  
  # If waning is included in the model, add v_leak
  if(waning != "no" & !any(names(init) == "v_leak")) init["v_leak"] <- 0
  if(waning == "no" & any(names(init) == "v_leak")) 
    init <- init[-which(names(init) == "v_leak")]
  
  # If constant leak rate is included in the model, add v_sec
  if(sec & !any(names(init) == "v_sec")) init["v_sec"] <- 0
  if(!sec & any(names(init) == "v_sec")) init <- init[-which(names(init) == "v_sec")]
  
  # If the spatial kernel is estimated in the model, add b, c and theta 
  if(distance == "fixed" & any(names(init) %in% c("b", "c", "theta"))) 
    init <- init[-which(names(init) %in% c("b", "c", "theta"))]
  
  
  return(init)
}

# Create covariance proposal matrix
create_proposal <- function(vacc_yes, vax, distance, waning, init, pars, sec){
  name_file <- paste0("Output/", vax, "_", distance, if(sec) "_sec", "/",
                      waning, ".RDS")
  
  # If the file already exists (i.e. the model has already been run), 
  # import the value from the saved file
  if(any(is.element(dir(recursive = T), name_file))) {
    pmcmc_run <- readRDS(name_file)
  } else{
    proposal_matrix <- matrix(0, nrow = length(init), ncol = length(init))
    colnames(proposal_matrix) <- rownames(proposal_matrix) <- names(init)
    diag(proposal_matrix) <- .01
    return(proposal_matrix)
  } 
  
  proposal_matrix <- cov(pmcmc_run$pars[-c(1:2000),])
  
  # If waning is included in the model, add v_leak
  if(waning != "no" & !any(colnames(proposal_matrix) == "v_leak")){
    proposal_matrix <- cbind(proposal_matrix, v_leak = 0)
    proposal_matrix <- rbind(proposal_matrix, v_leak = 0)
    proposal_matrix["v_leak", "v_leak"] <- 1e-5
  }
  if(waning == "no" & any(colnames(proposal_matrix) == "v_leak")){
    index_leak <- which(rownames(proposal_matrix) == "v_leak")
    proposal_matrix <- proposal_matrix[-index_leak, -index_leak]
  }
  # If constant leak rate is included in the model, add v_sec
  if(sec & !any(colnames(proposal_matrix) == "v_sec")){
    proposal_matrix <- cbind(proposal_matrix, v_sec = 0)
    proposal_matrix <- rbind(proposal_matrix, v_sec = 0)
    proposal_matrix["v_sec", "v_sec"] <- 1e-5
  }
  if(!sec & any(colnames(proposal_matrix) == "v_sec")){
    index_sec <- which(rownames(proposal_matrix) == "v_sec")
    proposal_matrix <- proposal_matrix[-index_sec, -index_sec]
  }
  
  # If the spatial kernel is estimated in the model, add b, c and theta 
  if(distance == "fixed" & any(names(init) %in% c("b", "c", "theta"))){
    index_space <- which(rownames(proposal_matrix) %in% c("b", "c", "theta"))
    proposal_matrix <- proposal_matrix[-index_space, -index_space]
  }
  
  proposal_matrix <- proposal_matrix[unlist(lapply(pars, function(X) return(X$name))), 
                                     unlist(lapply(pars, function(X) return(X$name)))]
  return(proposal_matrix)
}
