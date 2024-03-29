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
  # We use N_2006 as the population at t=0
  if(year_start == 2010) N <- N_2006 else stop("Define N")
  return(N)
}

## Import vaccine data
import_vaccine <- function(vax, scenario){
  if(vax == "cover" & scenario == "reference") {
    cov_per_year <- read.csv2(
      "Data/coverage_cover_extrapol.csv", sep = ";") %>% as.data.table()
  } else if(vax == "cprd" & scenario == "reference"){
    cov_per_year <- read.csv2("Data/coverage_cprd_extrapol.csv", sep = ";") %>% 
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
  if(scenario == "reference")
    dt_vacc[age == 3 & dose == 2, coverage := dt_vacc[age == 4 & dose == 2, coverage] * .5]
  dt_vacc[region == "EAST OF ENGLAND", region := "EAST"]
  # Add ID column (unique for each row) with the year, region, dose and age
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
  # if the model is using the cprd data, use the adjusted values of coverage from 
  # the risk assessment document, otherwise use unadjusted coverage (for data prior to 2004) 
  if(vax == "cprd") adj <- "Yes" else adj <- "No"
  N_reg <- length(regions)
  dt_vacc <- import_vaccine(vax, scenario)

  ## Define vaccine uptake and proportion of recovered at t = 0
  # In the first age group, everyone is susceptible
  # source: vaccination data and UKHSA's measles risk assessment 
  
  ## Initialise empty matrices for proportion of recovery and coverage
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
  # 2004 and before, use data from ukhsa risk assessment
  # Coverage available for London, for all other regions use the national estimates
  past_cov <- as.data.table(read.csv2("Data/risk_assessment_ukhsa.csv", sep = ","))
  past_cov[, Coverage := as.numeric(Coverage) / 100]
  
  # born in 00-04: use the average coverage across all years
  cov1[7, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(2000, 2004)) &
                               Dose == "MMR1" & Adjusted == adj, Coverage]) 
  cov2[7, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(2000, 2004)) &
                               Dose == "MMR2" & Adjusted == adj, Coverage]) 
  cov1[7, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(2000, 2004)) &
                                       Dose == "MMR1" & Adjusted == adj, Coverage])
  cov2[7, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(2000, 2004)) &
                                       Dose == "MMR2" & Adjusted == adj, Coverage])
  
  # Born in 95-99
  cov1[8, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1995, 1999)) & 
                               Dose == "MMR1" & Adjusted == adj, Coverage])
  cov2[8, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1995, 1999)) & 
                               Dose == "MMR2" & Adjusted == adj, Coverage])
  cov1[8, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1995, 1999)) & 
                                       Dose == "MMR1" & Adjusted == adj, Coverage])
  cov2[8, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1995, 1999)) & 
                                       Dose == "MMR2" & Adjusted == adj, Coverage])
  
  # Born in 91-95
  cov1[9, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1990, 1994)) & 
                               Dose == "MMR1" & Adjusted == adj, Coverage], na.rm = T)
  cov2[9, ] <- mean(past_cov[Region == "England" & is.element(Year, seq(1990, 1994)) & 
                               Dose == "MMR2" & Adjusted == adj, Coverage], na.rm = T)
  cov1[9, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1990, 1994)) & 
                                       Dose == "MMR1" & Adjusted == adj, Coverage],
                            na.rm = T)
  cov2[9, "LONDON"] <- mean(past_cov[Region == "London" & is.element(Year, seq(1990, 1994)) & 
                                       Dose == "MMR2" & Adjusted == adj, Coverage], 
                            na.rm = T)
  
  # Born in 80-90
  cov1[10, ] <- mean(past_cov[Region == "England" & Year < 1990 & 
                                Dose == "MMR1" & Adjusted == adj, Coverage], 
                     na.rm = T)
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
  dt_vacc <- import_vaccine(vax, scenario)
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

## Compute contact matrix between age groups
compute_contact_matrix <- function(year_per_age){
  ## Define contact matrix
  data(polymod)
  
  ## Get the transmission matrix weighted by the population in each age band from 
  ## socialmixr (equal to the contact rate per capita)
  ref_m <- 1e6 * socialmixr::contact_matrix(
    survey = polymod,
    countries = "United Kingdom",
    age.limits = cumsum(year_per_age) - year_per_age,
    symmetric = TRUE, per.capita = T)$matrix.per.capita
  
  rownames(ref_m) <- colnames(ref_m) <- 
    paste(cumsum(year_per_age) - year_per_age, cumsum(year_per_age), sep = "-")
  return(ref_m)
}

## Compute the number of reported importations per year (from UKHSA's epi data)
compute_importation <- function(regions, N_age, scenario_import = "per_year"){
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
  return(mean_import_per_reg)
}

## Import all data using all other functions, and return in a list
import_all_data <- function(year_start, N_year, scenario, vax, regions, age){
  ## Number of inhabitants per age group
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
  
  ## Return a list containing all data
  list_data <- 
    append(append(list_state, list_array_cov), list(
      ref_m = ref_m, ref_d = ref_d, new_birth = new_birth, 
      mean_import_per_reg = mean_import_per_reg, N_reg = ncol(N), N_age = nrow(N),
      year_per_age = year_per_age
    ))
  return(list_data)
}

## Import case data
import_case_data <- function(state_names, regions, age, vacc_yes, year_start, N_year, 
                             anoun = TRUE){
  ## Import case file
  if(anoun){
    data_anoun <- as.data.table(readRDS("Data/sim_data.RDS"))
  } else{
    stop("Set anoun as TRUE to use simulated case data.")
  }
  
  ## Remove cases outside 
  date_start <- as.Date(paste0(year_start, "-01-01"))
  date_end <- as.Date(paste0(year_start + N_year, "-01-01"))
  data_anoun <- data_anoun[date > date_start,]
  data_anoun <- data_anoun[date < date_end,]
  data_anoun[, date := as.numeric(as.Date(date) - date_start)]
  data_anoun[, date := factor(date, levels = seq(1, max(date)))]
  
  ## Rename categories
  data_anoun[region == "East of England", region := "East"]
  data_anoun[is.na(vaccinated), vaccinated := "new_IS"]
  data_anoun[vaccinated == "mmr x2", vaccinated := "new_IV2"]
  data_anoun[vaccinated == "mmr x1", vaccinated := "new_IV1"]
  data_anoun[vaccinated == "no", vaccinated := "new_IS"]
  data_anoun[, vaccinated := factor(vaccinated)]
  data_anoun[, age_groups := factor(age_groups, levels = age)]
  data_anoun[, region := factor(toupper(region), levels = toupper(regions))]
  
  ## Convert data_anoun to number of entries by date / vaccination status / region and age group
  data_anoun <- data_anoun[, data.table(table(date, vaccinated, region, age_groups))]
  
  data_anoun[, vaccinated := as.character(vaccinated)]
  
  ## Set individuals classified as "yes" as v1 or v2 (v1 by default)
  if(vacc_yes == "v1" & any(data_anoun$vaccinated == "yes")){
    data_anoun[vaccinated == "new_IV1", 
               N := data_anoun[vaccinated == "new_IV1", N] + 
                 data_anoun[vaccinated == "yes", N]]
  } else if(vacc_yes == "v2" & any(data_anoun$vaccinated == "yes")){
    data_anoun[vaccinated == "new_IV2", 
               N := data_anoun[vaccinated == "new_IV2", N] + 
                 data_anoun[vaccinated == "yes", N]]
  } else if(any(data_anoun$vaccinated == "yes")) 
    stop("vacc_yes should be v1 or v2")
  
  ## Remove entries with vaccinated == "yes"
  data_anoun <- data_anoun[vaccinated != "yes",]
  data_anoun[, vaccinated := factor(vaccinated)]
  
  return(data_anoun)
}
