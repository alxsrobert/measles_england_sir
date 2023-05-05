#### Import population data

dt_pop_all <- read.csv2("Data/regional_population.csv", row.names = 1)

dt_pop_select <- dt_pop_all[dt_pop_all$Area != "Country", 
                            which(colnames(dt_pop_all) != "Area" & 
                                    (colnames(dt_pop_all) == "Name" |
                                       !grepl("_", colnames(dt_pop_all))))]
N_reg <- length(unique(dt_pop_select$Name))
N_age <- sum(grepl("age", colnames(dt_pop_select)))

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
## Define vaccine uptake and proportion of recovered
# In the first age group, everyone is susceptible
# In the third age group, most people are vaccinated twice
# Low vaccine uptake in region 3 (1/3 of susceptibles)
mean_recov <- c(0, .003, .006, .01, .013, .02,  .05,  .2,   .7,  .9, .99)
mean_cov1 <-  c(0, .9,   .92,  .95, .1,   .02,  .01,  .01,  .01, 0,  0)
mean_cov2 <-  c(0, 0,    0,    0,   .75,  .83,  .82,  .7,   .2,  0,  0)
recov <- matrix(rbeta(shape1 = mean_recov * 100, shape2 = 100 - mean_recov * 100, 
                      n = N_reg * N_age), 
                ncol = N_reg, byrow = F)
cov1 <- matrix(rbeta(shape1 = mean_cov1 * 100, shape2 = 100 - mean_cov1 * 100, 
                     n = N_reg * N_age), ncol = N_reg)

cov2 <- matrix(rbeta(shape1 = mean_cov2 * 100, shape2 = 100 - mean_cov2 * 100, 
                     n = N_reg * N_age), ncol = N_reg)
unvax <- 1 - cov1 - cov2
if(any(unvax > 1 | unvax < 0)) stop("Proportion should be between 0 and 1")

## Number of inhabitants in each compartment
S <- round(N * (1 - recov) * unvax, 0)
V1 <- round(N * (1 - recov) * cov1, 0)
V2 <- round(N * (1 - recov) * cov2, 0)
R <- round(N * recov * unvax, 0)
RV1 <- round(N * recov * cov1, 0)
RV2 <- round(N * recov * cov2, 0)

n_birth_per_year <- n_birth_per_year[as.character(seq(year_start, year_start + N_year - 1)),]
new_birth <- t(n_birth_per_year[rep(seq_len(nrow(n_birth_per_year)), each = 365), ]) / 365

#### Vaccine coverage through time ####

vacc_per_age <- data.frame(regions = rep(unique(dt_pop_select$Name), N_age * N_year), 
                           age = rep(rep(rownames(N), each = N_reg), N_year),
                           year = rep(year_start + 0:(N_year - 1), each = N_age * N_reg))
mean_cov1 <- c(0, .7, .8, .9, .92, .95, .92, .93, .95, .6, 0)
mean_cov2 <- c(0, 0,  0,  0,  .7,  .75, .8,  .85, .85, .4, 0)
## Vaccine coverage: at least one dose
vacc_per_age$v1 <- rbeta(shape1 = rep(mean_cov1 * 100, each = N_reg),
                         shape2 = rep(100 - mean_cov1 * 100, each = N_reg),
                         n = N_reg * N_year * N_age)

## Vaccine coverage: Two doses
vacc_per_age$v2 <- rbeta(shape1 = rep(mean_cov2 * 100, each = N_reg),
                         shape2 = rep(100 - mean_cov2 * 100, each = N_reg),
                         n = N_reg * N_year * N_age)

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
mean_import_per_reg <- c(1, 3, 3, 2, 2, 3, 15, 7, 2) / nrow(ref_m)
mean_import <- numeric()
seas <- exp(.2 * cos(2 * pi * seq_len(t_tot) / 365) + 
  1.5 * sin(2 * pi * seq_len(t_tot) / 365))

import <- array(dim = c(nrow(ref_m), nrow(ref_d), t_tot))

for(i in seq_along(mean_import_per_reg)){
  # Mean number of import by day
  mean_import_i <- mean_import_per_reg[i] * seas / sum(seas[1:365])
  # Draw the number of importations (by age, region, and day)
  import[,i,] <- round(rpois(n = t_tot * nrow(ref_m),
                             rep(mean_import_i, each = nrow(ref_m))))
}

