## 4 regions ; 3 age groups
## Define contact matrix
ref_m <- matrix(, nrow = 4, ncol = 4)
# The number of contacts between groups is smaller than within groups
ref_m[] <- abs(rnorm(n = prod(dim(ref_m)), mean = .2, sd = .05))
diag(ref_m)[] <- rnorm(n = nrow(ref_m), mean = .8, sd = .05)

## Define distance matrix. Distance is defined as the degree of connectivity between
## regions (i.e. distance of 1 between neighbours, 2 between neighbours of neighbours etc...)
ref_d <- matrix(c(0, 1, 1, 2, 3,
                  1, 0, 2, 1, 1,
                  1, 2, 0, 2, 2,
                  2, 1, 2, 0, 1,
                  3, 1, 2, 1, 0), ncol = 5) + 1

# Dimensions
N_age <- nrow(ref_m)
N_reg <- nrow(ref_d)

## Total number of inhabitants per region / age groups
N <- matrix(c(2e4, 8e4, 1e4, 5e3, 4e4,
              1.5e4, 5e4, 2e4, 1e4, 5e4, 
              3e4, 1e5, 1.5e4, 7e3, 3e4,
              1e4, 9e4, 2e4, 7e3, 4e4
), ncol = 5, byrow = T)

## Define vaccine uptake and proportion of recovered
# In the first age group, everyone is susceptible
# In the third age group, most people are vaccinated twice
# Low vaccine uptake in region 3 (1/3 of susceptibles)
recov <- matrix(c(0, 0, 0, 0, 0,
                  1/20, 1/40, 1/20, 1/80, 1/40,
                  1/10, 1/20, 1/15, 1/50, 1/30,
                  1/8, 1/15, 1/12, 1/30, 1/20), ncol = 5, byrow = T)

cov1 <- matrix(c(  0, 0, 0, 0, 0,
                   .3, .5, .3, .7, .2,
                   .2, .5, .6, .3, .4,
                   .1, .15, .05, .05, .1
), ncol = 5, byrow = T)

cov2 <- matrix(c(  0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0,
                   .5, .4, .3, .6, .2,
                   .7, .8, .9, .9, .6), ncol = 5, byrow = T)

unvax <- 1 - cov1 - cov2
if(any(unvax > 1 | unvax < 0)) stop("Proportion should be between 0 and 1")

## Number of inhabitants in each compartment
S <- round(N * (1 - recov) * unvax, 0)
V1 <- round(N * (1 - recov) * cov1, 0)
V2 <- round(N * (1 - recov) * cov2, 0)
R <- round(N * recov * unvax, 0)
RV1 <- round(N * recov * cov1, 0)
RV2 <- round(N * recov * cov2, 0)

# Draw number of new births
new_birth <- matrix(c(2e4, 8e4, 1e4, 5e3, 4e4) / 365, ncol = N_time, nrow = N_reg)

year_per_age <- rep(1, N_age)

#### Vaccine coverage through time ####


vacc_per_age <- data.frame(regions = rep(c("A", "B", "C", "D", "E"), 4 * 4), 
                           age = rep(rep(c(1, 2, 3, 4), each = 5), 4),
                           year = rep(c(2011, 2012, 2013, 2014), each = 4 * 5))

## Vaccine coverage: at least one dose
vacc_per_age$v1 <- c(# Year 1
  0, 0, 0, 0, 0,
  .3, .5, .3, .7, .2,
  .7, .9, .9, .9, .6,
  .8, .95, .95, .95, .7,
  # Year 2
  0, 0, 0, 0, 0,
  .35, .6, .3, .75, .3,
  .6, .7, .9, .9, .7,
  .9, .9, .95, .95, .8,
  # Year 3
  0, 0, 0, 0, 0,
  .3, .6, .3, .6, .5,
  .7, .7, .8, .9, .75,
  .75, .95, .95, .9, .95,
  # Year 4
  0, 0, 0, 0, 0,
  .2, .5, .3, .7, .5,
  .5, .9, .7, .8, .8,
  .8, .9, .9, .95, .8
)

## Vaccine coverage: Two doses
vacc_per_age$v2 <- c(# Year 1
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  .2, .4, .3, .6, .2,
  .7, .8, .9, .9, .6,
  # Year 2
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  .3, .5, .2, .7, .2,
  .5, .6, .85, .8, .6,
  # Year 3
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  .2, .4, .1, .5, .3,
  .6, .6, .75, .8, .6,
  # Year 4
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  .1, .4, .2, .5, .3,
  .4, .7, .5, .75, .6
)

# Proportion of the population vaccinated once
vacc_per_age$v1 <- vacc_per_age$v1 - vacc_per_age$v2 

# re-order rows
vacc_per_age <- vacc_per_age[order(vacc_per_age$year, vacc_per_age$regions, 
                                   vacc_per_age$age), ]
array_cov1 <- array(data = vacc_per_age$v1, 
                    dim = c(N_age, N_reg, N_time / 365),
                    dimnames = list(unique(vacc_per_age$age), 
                                    unique(vacc_per_age$regions))
)[,, rep(seq_len(N_time / 365), each  = 365)]

array_cov2 <- array(data = vacc_per_age$v2, 
                    dim = c(N_age, N_reg, N_time / 365),
                    dimnames = list(unique(vacc_per_age$age), 
                                    unique(vacc_per_age$regions))
)[,, rep(seq_len(N_time / 365), each  = 365)]

