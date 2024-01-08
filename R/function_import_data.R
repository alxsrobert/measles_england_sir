## Import population data
import_pop_data <- function(year_start){
  ## Import population data
  dt_pop_all <- read.csv2("Data/regional_population.csv", row.names = 1)
  
  ## Keep only columns with overall number of inhabitants (not stratified m/f)
  dt_pop_select <- dt_pop_all[dt_pop_all$Area != "Country", 
                              which(colnames(dt_pop_all) != "Area" & 
                                      (colnames(dt_pop_all) == "Name" |
                                         !grepl("_", colnames(dt_pop_all))))]
  # Extract the number of regions / age
  N_reg <- length(unique(dt_pop_select$Name))
  N_age <- sum(grepl("age", colnames(dt_pop_select)))
  # Fix typo in column name
  colnames(dt_pop_select)[colnames(dt_pop_select) == "age16t20"] <- "age16to20"
  
  ## Create population vector 2006, 2013, 2019
  N_2006 <- t(dt_pop_select[dt_pop_select$Year == 2006, grep("age", colnames(dt_pop_select))])
  N_2013 <- t(dt_pop_select[dt_pop_select$Year == 2013, grep("age", colnames(dt_pop_select))])
  N_2019 <- t(dt_pop_select[dt_pop_select$Year == 2019, grep("age", colnames(dt_pop_select))])
  
  colnames(N_2006) <- colnames(N_2013) <- colnames(N_2019) <- unique(dt_pop_select$Name)
  if(year_start == 2006 || year_start == 2010) N <- N_2006 else stop("Define N")
  return(N)
}

## Import vaccine data => USE IF SCENARIO == "" TO ADD MORE SCENARIOS
import_ehr_vaccine <- function(vax, scenario){
  if(vax == "cover" & scenario == "reference") {
    cov_per_year <- read.csv2(#ADD VACCINE DATASET FROM COVER DATA
                              sep = ";") %>% 
      as.data.table()
  } else if(vax == "cprd" & scenario == "reference"){
    cov_per_year <- read.csv2(#ADD VACCINE DATASET FROM CPRD DATA
                              sep = ";") %>% 
      as.data.table()
  }
  
  ## Formatting changes
  cov_per_year[, region := toupper(region)]
  cov_per_year[, dose := as.numeric(n_dose)]
  cov_per_year[, n_dose := NULL]
  cov_per_year[, year := as.numeric(year)]
  
  cov_per_year[, cov1y := as.numeric(cov1y)]
  cov_per_year[, cov2y := as.numeric(cov2y)]
  cov_per_year[, cov3y := as.numeric(cov3y)]
  cov_per_year[, cov4y := as.numeric(cov4y)]
  cov_per_year[, cov5y := as.numeric(cov5y)]
  
  ## Switch to long format 
  cov_per_year_long <- pivot_longer(cov_per_year, cols = paste0("cov", 1:5, "y"),
                                    names_to = "age", values_to = "coverage") %>% 
    as.data.table
  
  ## Remove "covXy" from the column "age" => only keep age
  cov_per_year_long[, age := as.numeric(substr(age, 4, 4))]
  ## Compute year of birth
  cov_per_year_long[, yob := year - age]
  ## Set NA values to 0
  cov_per_year_long[is.infinite(coverage), coverage := 0]
  
  ## Set dt_vacc by selecting some of the columns
  dt_vacc <- cov_per_year_long[, .(year, region, vaccine, coverage, yob, dose, age)]
  
  ## Coverage at 1 is 75% of coverage at 2
  dt_vacc[age == 1 & dose == 1, coverage := dt_vacc[age == 2 & dose == 1, coverage] * .75]
  ## Second dose coverage
  dt_vacc[age == 3 & dose == 2, coverage := dt_vacc[age == 4 & dose == 2, coverage] * .5]
  dt_vacc[region == "EAST OF ENGLAND", region := "EAST"]
  dt_vacc[, id := paste(year, tolower(region), dose, age, sep = "_")]
  colnames(dt_vacc) <- c("years", "region", "vac_code", "coverage", "yob", "dose",
                         "age", "id")
  setkey(dt_vacc, id)
  
  return(dt_vacc)
}

# Import number of births
compute_n_birth <- function(year_start, N_year){
  ## Import population data
  dt_pop_all <- read.csv2("Data/regional_population.csv", row.names = 1)
  
  dt_pop_select <- dt_pop_all[dt_pop_all$Area != "Country", 
                              which(colnames(dt_pop_all) != "Area" & 
                                      (colnames(dt_pop_all) == "Name" |
                                         !grepl("_", colnames(dt_pop_all))))]
  colnames(dt_pop_select)[colnames(dt_pop_select) == "age16t20"] <- "age16to20"
  
  ## Compute number of births per year
  ## Initialise the empty matrix
  n_birth_per_year <- matrix(, nrow = max(dt_pop_select$Year) - min(dt_pop_select$Year) + 6,
                             ncol = length(unique(dt_pop_select$Name)))
  rownames(n_birth_per_year) <- seq(min(dt_pop_select$Year) - 5, max(dt_pop_select$Year))
  colnames(n_birth_per_year) <- unique(dt_pop_select$Name)
  
  ## At each year, compute the number of new births as the number of inhabitants on yob
  age_n_birth <- c(0, 1, 2, 3, 4, 5)
  for(i in seq_len(nrow(dt_pop_select))){
    n_birth_per_year[as.character(dt_pop_select$Year[i] - age_n_birth), 
                     dt_pop_select$Name[i]] <- 
      as.numeric(dt_pop_select[i, paste0("age", 0:5)])
  }
  ## For NA values, use the number of births at the previous year 
  n_birth_per_year[is.na(n_birth_per_year[,1]),] <- n_birth_per_year[which(is.na(n_birth_per_year[,1])) - 1,]
  ## Keep data from 2010 onwards
  n_birth_per_year <- n_birth_per_year[as.character(seq(year_start, year_start + N_year - 1)),]
  ## Compute the number of births per year
  new_birth <- t(n_birth_per_year[rep(seq_len(nrow(n_birth_per_year)), each = 365), ]) / 365
  return(new_birth)
}

# Compute the distribution of individuals at t = 0
compute_initial_state <- function(vax, scenario, N_age, regions, N){
  #### Define initial number of cases in compartment
  if(vax == "cprd") adj <- "Yes" else adj <- "No"
  N_reg <- length(regions)
  dt_vacc <- import_ehr_vaccine(vax, scenario)

  ## Define vaccine uptake and proportion of recovered at t = 0
  # In the first age group, everyone is susceptible
  # year_start is 2010, 11 age groups: 
  # - age0 and age1: born in 2010 everyone susceptible
  # - age2 to age4: born 2008-2006: 1 vaccine dose
  # - age5 to age 1620: born 2005-1990: 2 vaccine doses => Drop in coverage
  # - age2140: born 90-70: ?
  # - age40plot: born before 70: Full recovered
  
  # source: vaccination data and UKHSA's measles risk assessment 
  
  ## Initialise empty matrices
  recov <- matrix(0, ncol = N_reg, nrow = N_age, byrow = F)
  cov1 <- matrix(0, ncol = N_reg, nrow = N_age)
  cov2 <- matrix(0, ncol = N_reg, nrow = N_age)
  
  colnames(cov1) <- colnames(cov2) <- toupper(regions)
  
  mean_vacc1 <- numeric(N_reg)
  mean_vacc2 <- numeric(N_reg)
  nb_years <- 0
  
  ## For years of birth that are included in dt_vacc, use dt_vacc
  for(i in unique(dt_vacc[years <= 2010, yob])){
    last_vacc <- max(dt_vacc[yob == i & dose == 1 & years <= 2010, years])
    if(i > 2004){
      cov1[1 + (2010-i), unique(dt_vacc$region)] <- 
        dt_vacc[yob == i & years == last_vacc & dose == 1, coverage]
      cov2[1 + (2010-i), unique(dt_vacc$region)] <- 
        dt_vacc[yob == i & years == last_vacc & dose == 2, coverage]
    }
  }
  past_cov <- as.data.table(read.csv2("Data/risk_assessment_ukhsa.csv", sep = ","))
  past_cov[, Coverage := as.numeric(Coverage) / 100]
  # born in 04-00
  cov1[7, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(2000, 2004)) &
                               Dose == "MMR1" & Adjusted == adj, Coverage]) 
  cov2[7, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(2000, 2004)) &
                               Dose == "MMR2" & Adjusted == adj, Coverage]) 
  cov1[7, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(2000, 2004)) &
                                       Dose == "MMR1" & Adjusted == adj, Coverage])
  cov2[7, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(2000, 2004)) &
                                       Dose == "MMR2" & Adjusted == adj, Coverage])
  
  # Born in 99-96
  cov1[8, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1995, 1999)) & Dose == "MMR1" & Adjusted == adj, Coverage])# .95
  cov2[8, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1995, 1999)) & Dose == "MMR2" & Adjusted == adj, Coverage])# .87
  cov1[8, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1995, 1999)) & Dose == "MMR1" & Adjusted == adj, Coverage])# .91
  cov2[8, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1995, 1999)) & Dose == "MMR2" & Adjusted == adj, Coverage])# .79
  
  # Born in 95-91
  cov1[9, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1990, 1994)) & 
                               Dose == "MMR1" & Adjusted == adj, Coverage], na.rm = T)# .96
  cov2[9, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1990, 1994)) & 
                               Dose == "MMR2" & Adjusted == adj, Coverage], na.rm = T)# .87
  cov1[9, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1990, 1994)) & 
                                       Dose == "MMR1" & Adjusted == adj, Coverage],
                            na.rm = T)# .93
  cov2[9, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1990, 1994)) & 
                                       Dose == "MMR2" & Adjusted == adj, Coverage], 
                            na.rm = T)# .79
  
  # Born in 90-80
  cov1[10, ] <- mean(past_cov[Region == "England" & Year < 1990 & 
                                Dose == "MMR1" & Adjusted == adj, Coverage], 
                     na.rm = T)# .98
  cov1[10, "LONDON"] <- mean(past_cov[Region == "London" & Year < 1990 & 
                                        Dose == "MMR1" & Adjusted == adj, Coverage], 
                             na.rm = T)
  
  cov1 <- cov1 - cov2
  cov1[cov1 < 0] <- 0
  
  unvax <- 1 - cov1 - cov2
  if(any(unvax > 1 | unvax < 0)) stop("Proportion should be between 0 and 1")
  
  ## Number of inhabitants in each compartment
  V1 <- round(N * (1 - recov) * cov1, 0)
  V2 <- round(N * (1 - recov) * cov2, 0)
  S <- round(N * (1 - recov) * unvax, 0)
  R <- N * 0
  
  return(list(S = S, V1 = V1, V2 = V2, R = R))
}

## Compute vaccine coverage at each year
compute_vax_cov <- function(age_names, year_start, N_year, N_age,
                            regions, vax, scenario){
  # Compute the number of regions
  N_reg <- length(regions)
  # Import vaccine coverage
  dt_vacc <- import_ehr_vaccine(vax, scenario)
  # Create empty data table to compute the vaccine coverage per region / age / year
  vacc_per_age <- data.table(regions = rep(toupper(regions), N_age * N_year), 
                             age = rep(rep(age_names, each = N_reg), N_year),
                             year = rep(year_start + 0:(N_year - 1), each = N_age * N_reg))
  if(all(dt_vacc$age %in% c(2, 5))){
    ## v1 <2 is 0
    vacc_per_age[age %in% c("age0", "age1"), v1 := 0]
    ## v2 <5 is 0
    vacc_per_age[age %in% c("age0", "age1", "age2", "age3", "age4"), v2 := 0]
    
    ## Compute vaccine coverage at 2, 3, 4, and 5
    vacc_per_age[age == "age2", v1 := dt_vacc[
      paste(year, tolower(regions), 1, 2, sep = "_"), coverage]]
    vacc_per_age[age == "age5", v1 := dt_vacc[
      paste(year, tolower(regions), 1, 5, sep = "_"), coverage]]
    vacc_per_age[age == "age5", v2 := dt_vacc[
      paste(year, tolower(regions), 2, 5, sep = "_"), coverage]]
    
    vacc_per_age[age == "age3", v1 := dt_vacc[
      paste(year - 1, tolower(regions), 1, 2, sep = "_"), coverage]]
    vacc_per_age[age == "age4", v1 := dt_vacc[
      paste(year - 2, tolower(regions), 1, 2, sep = "_"), coverage]]
  } else {
    ## Use entries for all ages
    vacc_per_age[, agegroup := substr(age, 4, 5)]
    vacc_per_age[age == "age0", v1 := 0]
    vacc_per_age[age == "age0", v2 := 0]
    vacc_per_age[age %in% c("age1", "age2", "age3", "age4", "age5"),
                 v1 := dt_vacc[paste(year, tolower(regions), 1, 
                                     agegroup, sep = "_"), coverage]]
    vacc_per_age[age %in% c("age1", "age2", "age3", "age4", "age5"),
                 v2 := dt_vacc[paste(year, tolower(regions), 2, 
                                     agegroup, sep = "_"), coverage]]
    vacc_per_age[, agegroup := NULL]
  }
  ## Compute vaccine coverage in older ages from dt_vacc in previous years
  vacc_per_age[age == "age6to9", v1 := dt_vacc[paste(year - 2, tolower(regions), 1, 5, sep = "_"), coverage]]
  vacc_per_age[age == "age6to9", v2 := dt_vacc[paste(year - 2, tolower(regions), 2, 5, sep = "_"), coverage]]
  vacc_per_age[age == "age11to15", v1 := dt_vacc[paste(year - 7, tolower(regions), 1, 5, sep = "_"), coverage]]
  vacc_per_age[age == "age11to15", v2 := dt_vacc[paste(year - 7, tolower(regions), 2, 5, sep = "_"), coverage]]
  vacc_per_age[age == "age16to20", v1 := dt_vacc[paste(year - 12, tolower(regions), 1, 5, sep = "_"), coverage]]
  vacc_per_age[age == "age16to20", v2 := dt_vacc[paste(year - 12, tolower(regions), 2, 5, sep = "_"), coverage]]
  vacc_per_age[age == "age21to30", v1 := dt_vacc[paste(year - 17, tolower(regions), 1, 5, sep = "_"), coverage]]
  vacc_per_age[age == "age21to30", v2 := dt_vacc[paste(year - 17, tolower(regions), 2, 5, sep = "_"), coverage]]
  vacc_per_age[is.na(v1), v1 := 0]
  vacc_per_age[is.na(v2), v2 := 0]
  
  # Proportion of the population vaccinated once
  vacc_per_age$v1 <- vacc_per_age$v1 - vacc_per_age$v2
  vacc_per_age$v1[vacc_per_age$v1 < 0] <- 0
  
  vacc_per_age$regions <- factor(vacc_per_age$regions, levels = toupper(regions))
  
  vacc_per_age$age <- factor(vacc_per_age$age, levels = unique(vacc_per_age$age))
  
  # re-order rows
  vacc_per_age <- vacc_per_age[order(vacc_per_age$year, vacc_per_age$regions, 
                                     vacc_per_age$age), ]
  
  ## Create 3-d arrays containing the vaccine coverage (v1 then v2) per age / region / DAY
  array_cov1 <- array(data = vacc_per_age$v1, 
                      dim = c(N_age, N_reg, N_year),
                      dimnames = list(unique(vacc_per_age$age), 
                                      unique(vacc_per_age$regions))
  )[,, rep(seq_len(N_year), each  = 365)]
  
  array_cov2 <- array(data = vacc_per_age$v2, 
                      dim = c(N_age, N_reg, N_year),
                      dimnames = list(unique(vacc_per_age$age), 
                                      unique(vacc_per_age$regions))
  )[,, rep(seq_len(N_year), each  = 365)]
  
  return(list(array_cov1 = array_cov1, array_cov2 = array_cov2))
}

compute_contact_matrix <- function(year_per_age){
  ## Define contact matrix
  data(polymod)
  ## Get the contact matrix from socialmixr
  contact <- socialmixr::contact_matrix(
    survey = polymod,
    countries = "United Kingdom",
    age.limits = cumsum(year_per_age) - year_per_age,
    symmetric = TRUE)
  
  ## Transform the matrix to the (symetrical) transmission matrix
  ## rather than the contact matrix. This transmission matrix is
  ## weighted by the population in each age band (equal to the contact 
  ## rate per capita).
  ref_m <- 1e6 * contact$matrix /
    rep(contact$demography$population, each = ncol(contact$matrix))
  
  # # The number of contacts between groups is smaller than within groups
  rownames(ref_m) <- colnames(ref_m) <- 
    paste(cumsum(year_per_age) - year_per_age, cumsum(year_per_age), sep = "-")
  return(ref_m)
}

compute_importation <- function(regions, N_age, scenario_import = "per_year"){
  ## Define number of import per day
  if(scenario_import == "pop"){
    mean_import_per_reg <- sum(c(1.6, 3.2, 3.0, 2.0, 2.3, 3.2, 20.1, 7.6, 3.2)) / 365.25 /  N_age *
      colSums(N) / sum(N)
  } else if(scenario_import == "per_year"){ 
    # Cases classified as imported or possible import in the epi data. 
    mean_import_per_reg <- matrix(c(4, 2,  4,  1, 1,  8, 12, 10, 2,
                                    1, 6,  3,  3, 10, 4, 55, 30, 10,
                                    2, 10, 4,  2, 6,  3, 10, 7,  5,
                                    6, 5,  3,  2, 2,  6, 7,  3,  2,
                                    1, 4,  1,  2, 3,  3, 19, 4,  1,
                                    1, 2,  0,  1, 2,  1, 11, 1,  4,
                                    0, 1,  1,  0, 0,  3, 15, 5,  3,
                                    2, 6,  2,  1, 4,  4, 19, 3,  2,
                                    1, 3,  13, 4, 5,  1, 36, 10, 4,
                                    1, 6,  4,  6, 2,  8, 39, 14, 4),
                                  ncol = length(regions), byrow = T
    )/365.25/  N_age
    colnames(mean_import_per_reg) <- regions
  } else {
    mean_import_per_reg <- (c(1.6, 3.2, 3.0, 2.0, 2.3, 3.2, 20.1, 7.6, 3.2) / N_age) / 365.25# * 5
  }
  return(mean_import_per_reg)
}

import_all_data <- function(year_start, N_year, scenario, vax, regions, age){
  N <- import_pop_data(year_start)
  
  ## Time spent in each age group
  year_per_age <- c(1, 1, 1, 1, 1, 1, 4, 5, 5, 10, 10, 40)
  
  ## Create the neighbour matrix
  ref_d <- matrix(c(1, 2, 2, 3, 3, 4, 5, 4, 4,
                    2, 1, 2, 2, 2, 3, 4, 3, 3,
                    2, 2, 1, 2, 3, 3, 4, 3, 4,
                    3, 2, 2, 1, 2, 2, 3, 2, 3,
                    3, 2, 3, 2, 1, 3, 3, 2, 2,
                    4, 3, 3, 2, 3, 1, 2, 2, 3,
                    5, 4, 4, 3, 3, 2, 1, 2, 3,
                    4, 3, 3, 2, 2, 2, 2, 1, 2,
                    4, 3, 4, 3, 2, 3, 3, 2, 1), nrow = ncol(N))
  colnames(ref_d) <- rownames(ref_d) <- regions
  
  ## compute the number of births per year
  new_birth <- compute_n_birth(year_start, N_year)
  
  ## Compute initial state
  list_state <- compute_initial_state(
    scenario = scenario, vax = vax, N_age = length(age), regions = regions, 
    N = N)
  
  ## Compute vaccine coverage through time
  list_array_cov <- compute_vax_cov(
    age_names = rownames(N), year_start = year_start, 
    N_year = N_year, N_age = nrow(N), regions = regions, scenario = scenario, 
    vax = vax)
  
  ## Compute contact matrix between age groups
  ref_m <- compute_contact_matrix(year_per_age = year_per_age)
  
  ## Compute number of importations per region / year
  mean_import_per_reg <- compute_importation(regions, N_age = nrow(N))
  list_data <- 
    append(append(list_state, list_array_cov), list(
      ref_m = ref_m, ref_d = ref_d, new_birth = new_birth, 
      mean_import_per_reg = mean_import_per_reg, N_reg = ncol(N), N_age = nrow(N),
      year_per_age = year_per_age
    ))
  return(list_data)
}

