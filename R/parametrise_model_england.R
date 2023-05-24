#### Import population data

dt_pop_all <- read.csv2("Data/regional_population.csv", row.names = 1)

dt_pop_select <- dt_pop_all[dt_pop_all$Area != "Country", 
                            which(colnames(dt_pop_all) != "Area" & 
                                    (colnames(dt_pop_all) == "Name" |
                                       !grepl("_", colnames(dt_pop_all))))]
N_reg <- length(unique(dt_pop_select$Name))
N_age <- sum(grepl("age", colnames(dt_pop_select)))

colnames(dt_pop_select)[colnames(dt_pop_select) == "age16t20"] <- "age16to20"

## Create population vector 2006, 2013, 2019
N_2006 <- t(dt_pop_select[dt_pop_select$Year == 2006, grep("age", colnames(dt_pop_select))])
N_2013 <- t(dt_pop_select[dt_pop_select$Year == 2013, grep("age", colnames(dt_pop_select))])
N_2019 <- t(dt_pop_select[dt_pop_select$Year == 2019, grep("age", colnames(dt_pop_select))])

colnames(N_2006) <- colnames(N_2013) <- colnames(N_2019) <- unique(dt_pop_select$Name)

## Time spent in each age group
year_per_age <- c(1, 1, 1, 1, 1, 1, 4, 5, 5, 20, 40)

## Create the neighbour matrix
ref_d <- matrix(c(1, 2, 2, 3, 3, 4, 5, 4, 4,
                  2, 1, 2, 2, 2, 3, 4, 3, 3,
                  2, 2, 1, 2, 3, 3, 4, 3, 4,
                  3, 2, 2, 1, 2, 2, 3, 2, 3,
                  3, 2, 3, 2, 1, 3, 3, 2, 2,
                  4, 3, 3, 2, 3, 1, 2, 2, 3,
                  5, 4, 4, 3, 3, 2, 1, 2, 3,
                  4, 3, 3, 2, 2, 2, 2, 1, 2,
                  4, 3, 4, 3, 2, 3, 3, 2, 1), nrow = N_reg)
colnames(ref_d) <- rownames(ref_d) <- unique(dt_pop_select$Name)
# Capitalise only the first letter of each word in rownames(ref_d)
names_reg <- gsub("(^|[[:space:]])([[:alpha:]])", "\\2\\L\\2",  
                  tolower(rownames(ref_d)), perl = TRUE)

## Compute number of births per year

n_birth_per_year <- matrix(, nrow = max(dt_pop_select$Year) - min(dt_pop_select$Year) + 6,
                           ncol = length(unique(dt_pop_select$Name)))
rownames(n_birth_per_year) <- seq(min(dt_pop_select$Year) - 5, max(dt_pop_select$Year))
colnames(n_birth_per_year) <- unique(dt_pop_select$Name)

age_n_birth <- c(0, 1, 2, 3, 4, 5)
for(i in seq_len(nrow(dt_pop_select))){
  n_birth_per_year[as.character(dt_pop_select$Year[i] - age_n_birth), dt_pop_select$Name[i]] <- 
    as.numeric(dt_pop_select[i, paste0("age", 0:5)])
}
n_birth_per_year[is.na(n_birth_per_year[,1]),] <- n_birth_per_year[which(is.na(n_birth_per_year[,1])) - 1,]

## define when the simulation is starting
year_start <- 2006

#### Define initial number of cases in compartment
if(year_start == 2006) N <- N_2006 else stop("Define N")
## Define vaccine uptake and proportion of recovered at t = 0
# In the first age group, everyone is susceptible
# year_start is 2006, 11 age groups: 
# - age0 and age1: born in 2006 everyone susceptible
# - age2 to age4: born 2004-2002: 1 vaccine dose
# - age5 to age 1620: born 2001-1985: 2 vaccine doses
# - age2140: born 85-66: 
# - age40plot: born before 66: Full recovered

# source: Measles in the United Kingdom 1990-2008 and the effectiveness of measles vaccines
#  "we estimate that, in 1994, 65% of children age 1–2 years had been vaccinated 
# with the MMR vaccine, 87% of # children age 3–4 years had been vaccinated, 77% 
# of children age 5–9 years had been vaccinated, and 28% of those aged 10–19 years"
# yob:       2006,05, 2004, 2003, 2002, 2001,00-96, 95-91,90-86,85-66, 1965)   
mean_recov <- c(0, 0, .006, .01,  .013, .02,  .2,    .4,     .999,  .999, .999)
mean_cov1 <-  c(0, 0, .876, .852, .828, .100, .1,     0,      0,    0,  0)
mean_cov2 <-  c(0, 0,  0,   0,    0,    .756, .8,    .9,     .9,  .99,  0) # Proportion of the susceptible vaccinated
recov <- matrix(rbeta(shape1 = mean_recov * 200, shape2 = 200 - mean_recov * 200, 
                      n = N_reg * N_age), 
                ncol = N_reg, byrow = F)
cov1 <- matrix(rbeta(shape1 = mean_cov1 * 200, shape2 = 200 - mean_cov1 * 200, 
                     n = N_reg * N_age), ncol = N_reg)

cov2 <- matrix(rbeta(shape1 = mean_cov2 * 200, shape2 = 200 - mean_cov2 * 200, 
                     n = N_reg * N_age), ncol = N_reg)
unvax <- 1 - cov1 - cov2
if(any(unvax > 1 | unvax < 0)) stop("Proportion should be between 0 and 1")

## Number of inhabitants in each compartment
S <- round(N * (1 - recov) * unvax, 0)
V1 <- round(N * (1 - recov) * cov1, 0)
V2 <- round(N * (1 - recov) * cov2, 0)
R <- round(N * recov, 0)
RV1 <- RV2 <- R * 0

n_birth_per_year <- n_birth_per_year[as.character(seq(year_start, year_start + N_year - 1)),]
new_birth <- t(n_birth_per_year[rep(seq_len(nrow(n_birth_per_year)), each = 365), ]) / 365

#### Vaccine coverage through time ####

vacc_per_age <- data.table(regions = rep(unique(dt_pop_select$Name), N_age * N_year), 
                           age = rep(rep(rownames(N), each = N_reg), N_year),
                           year = rep(year_start + 0:(N_year - 1), each = N_age * N_reg))
source("R/import_cover_vaccine_data.R")
dt_vacc[region == "EAST OF ENGLAND", region := "EAST"]
dt_vacc[, id := paste(year, tolower(region), dose, age, sep = "_")]
colnames(dt_vacc) <- c("years", "region", "vac_code", "coverage", "yob", "dose",
                       "age", "id")
setkey(dt_vacc, id)

vacc_per_age[age %in% c("age0", "age1"), v1 := 0]
vacc_per_age[age %in% c("age0", "age1", "age2", "age3", "age4"), v2 := 0]

vacc_per_age[age == "age2", v1 := dt_vacc[paste(year, tolower(regions), 1, 2, sep = "_"), coverage]]
vacc_per_age[age == "age5", v1 := dt_vacc[paste(year, tolower(regions), 1, 5, sep = "_"), coverage]]
vacc_per_age[age == "age5", v2 := dt_vacc[paste(year, tolower(regions), 2, 5, sep = "_"), coverage]]

vacc_per_age[age == "age3", v1 := dt_vacc[paste(year - 1, tolower(regions), 1, 2, sep = "_"), coverage]]
vacc_per_age[age == "age4", v1 := dt_vacc[paste(year - 2, tolower(regions), 1, 2, sep = "_"), coverage]]
vacc_per_age[age == "age6to9", v1 := dt_vacc[paste(year - 2, tolower(regions), 1, 5, sep = "_"), coverage]]
vacc_per_age[age == "age6to9", v2 := dt_vacc[paste(year - 2, tolower(regions), 2, 5, sep = "_"), coverage]]
vacc_per_age[age == "age11to15", v1 := dt_vacc[paste(year - 7, tolower(regions), 1, 5, sep = "_"), coverage]]
vacc_per_age[age == "age11to15", v2 := dt_vacc[paste(year - 7, tolower(regions), 2, 5, sep = "_"), coverage]]
vacc_per_age[age == "age16to20", v1 := dt_vacc[paste(year - 12, tolower(regions), 1, 5, sep = "_"), coverage]]
vacc_per_age[age == "age16to20", v2 := dt_vacc[paste(year - 12, tolower(regions), 2, 5, sep = "_"), coverage]]
vacc_per_age[age == "age21to40", v1 := dt_vacc[paste(year - 17, tolower(regions), 1, 5, sep = "_"), coverage]]
vacc_per_age[age == "age21to40", v2 := dt_vacc[paste(year - 17, tolower(regions), 2, 5, sep = "_"), coverage]]
vacc_per_age[is.na(v1), v1 := 0]
vacc_per_age[is.na(v2), v2 := 0]

# Proportion of the population vaccinated once
vacc_per_age$v1 <- vacc_per_age$v1 - vacc_per_age$v2
vacc_per_age$v1[vacc_per_age$v1 < 0] <- 0

vacc_per_age$regions <- factor(vacc_per_age$regions, levels = unique(dt_pop_select$Name))
vacc_per_age$age <- factor(vacc_per_age$age, levels = rownames(N))
# re-order rows
vacc_per_age <- vacc_per_age[order(vacc_per_age$year, vacc_per_age$regions, 
                                   vacc_per_age$age), ]
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

## 4 regions ; 3 age groups
## Define contact matrix
data(polymod)
ref_m <- socialmixr::contact_matrix(polymod, countries = "United Kingdom", 
                                    age.limits = cumsum(year_per_age) - year_per_age)$matrix
# # The number of contacts between groups is smaller than within groups
rownames(ref_m) <- colnames(ref_m) <- 
  paste(cumsum(year_per_age) - year_per_age, cumsum(year_per_age), sep = "-")

## Define number of import
mean_import_per_reg <- (c(1, 3, 3, 2, 2, 3, 15, 7, 2) / nrow(ref_m))# * 5
mean_import <- numeric()
seas <- exp(.05 * cos(2 * pi * seq_len(t_tot) / 365) + 
              .25 * sin(2 * pi * seq_len(t_tot) / 365))

import <- array(dim = c(nrow(ref_m), nrow(ref_d), t_tot))

for(i in seq_along(mean_import_per_reg)){
  # Mean number of import by day
  mean_import_i <- mean_import_per_reg[i] * seas / sum(seas[1:365])
  # Draw the number of importations (by age, region, and day)
  import[,i,] <- round(rpois(n = t_tot * nrow(ref_m),
                             rep(mean_import_i, each = nrow(ref_m))))
}

