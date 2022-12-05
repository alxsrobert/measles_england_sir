#### First go at the age and region-structured model
#### Include transmission between regions (using a spatial kernel) and age groups
#### Include outside importations

#### Todo:
#### - Add number of deaths?
#### - Draw the number of importations in this script (currently defined by the user)
#### - Add maternal protection for the first age group?

#### Structure of the script:
#### 1- Core equations
#### 2- Draw number of individuals changing compartments
#### 3- Compute the probability of movement between compartments
#### 4- compute the force of infection
#### 5- Compute the number of importations
#### 6- Compute ageing
#### 7- Define initial status / dimension of the compartment matrices

#### 1- Core equations for transitions between compartments: ####

### All compartments are stratified by age (ROWS) and regions (COLUMNS)
## Susceptibles compartments: 
# New value of S, V1, and V2 is: 
#   (old value) - (nb of susceptible individuals who were exposed)
# First age groups: Add births
update(S[1, ]) <- S[1, j] + new_birth[j] - n_S[1, j] - n_SEs[1, j]
update(V1[1,]) <- V1[1, j] - n_v1E[1, j] - n_V1[1, j]
update(V2[1,]) <- V2[1, j] - n_v2E[1, j] - n_V2V2[1, j]

# Following age groups
update(S[2 : len_ageing,]) <- S[i, j] - n_SEs[i, j] + 
  n_SS[i - 1, j] - n_S[i, j]
update(V1[2 : len_ageing,]) <- V1[i, j] - n_v1E[i, j] + 
  n_V1V1[i - 1, j] + n_SV1[i - 1, j] - n_V1[i, j]
update(V2[2 : len_ageing,]) <- V2[i, j] - n_v2E[i, j] + 
  n_V2V2[i - 1, j] + n_V1V2[i - 1, j] - n_V2V2[i, j]

# Last age groups: Add ageing population, remove deaths? (or could put everyone in R?)
update(S[N_age,]) <- S[N_age, j] - n_SEs[N_age, j] + 
  n_SS[len_ageing, j]
update(V1[N_age,]) <- V1[N_age, j] - n_v1E[N_age, j] + 
  n_V1V1[len_ageing, j] + n_SV1[len_ageing, j]
update(V2[N_age,]) <- V2[N_age, j] - n_v2E[N_age, j] + 
  n_V2V2[len_ageing, j] + n_V1V2[len_ageing, j]


## Exposed compartments: No Aging
# New value of ES, EV1, and EV2 is: 
#   (old value) + (new_exposed) - (nb of exposed individuals moving to infectious)
update(Es[,]) <-  Es[i, j] + n_SEs[i, j] - n_EsIs[i, j]
update(Ev1[,]) <- Ev1[i, j] + n_v1E[i, j] - n_Ev1Iv1[i, j]
update(Ev2[,]) <- Ev2[i, j] + n_v2E[i, j] - n_Ev12v2[i, j]

## Infected compartments: No Aging
# New value of IS is:
#   (old value) + (new_exposed) + (new_imports) - (nb of infectious moving to recovered)
# import_t corresponds to the number of new imports in this region / age group
# Assumption: All imports are unvaccinated
update(Is[,]) <- Is[i, j] + n_EsIs[i, j] - n_IsR[i, j] + import_t[i,j]
update(Iv1[,]) <- Iv1[i, j] + n_Ev1Iv1[i, j] - n_Iv1R[i, j]
update(Iv2[,]) <- Iv2[i, j] + n_Ev12v2[i, j] - n_Iv2R[i, j]

## Recovered compartments: similar to Susceptible compartments
# First age group:
update(R[1, ]) <- R[1, j] + n_IsR[1, j] - 
  n_R[1, j]
update(RV1[1, ]) <- RV1[1, j] + n_Iv1R[1, j] - 
  n_RV1[1, j]
update(RV2[1, ]) <- RV2[1, j] + n_Iv2R[1, j] - 
  n_RV2RV2[1, j]

# Following age groups:
update(R[2 : len_ageing,]) <- R[i, j] + n_IsR[i, j] + 
  n_RR[i - 1, j] - n_R[i, j]
update(RV1[2 : len_ageing,]) <- RV1[i, j] + n_Iv1R[i, j] + 
  n_RV1RV1[i - 1, j] + n_RRV1[i - 1, j] - n_RV1[i, j]
update(RV2[2 : len_ageing,]) <- RV2[i, j] + n_Iv2R[i, j] + 
  n_RV2RV2[i - 1, j] + n_RV1RV2[i - 1, j] - n_RV2RV2[i, j]

# Last age group:
update(R[N_age,]) <- R[N_age, j] + n_IsR[N_age, j] +
  n_RR[len_ageing, j]
update(RV1[N_age,]) <- RV1[N_age, j] + n_Iv1R[N_age, j] +
  n_RV1RV1[len_ageing, j] + n_RRV1[len_ageing, j]
update(RV2[N_age,]) <- RV2[N_age, j] + n_Iv2R[N_age, j] +
  n_RV2RV2[len_ageing, j] + n_RV1RV2[len_ageing, j]

## Track number of new infections
update(new_IS[, ]) <- n_EsIs[i, j]
update(new_IV1[, ]) <- n_Ev1Iv1[i, j]
update(new_IV2[, ]) <- n_Ev12v2[i, j]

#### 2- Compute "n_" variables: the number of individuals changing between compartments ####
## Draw from binomial distributions: 
# Depends on the number of individuals in the compartment and the probability of moving
n_SEs[,] <- rbinom(S[i, j], p_SE[i, j])
n_v1E[,] <- rbinom(V1[i, j], p_SEv1[i, j])
n_v2E[,] <- rbinom(V2[i, j], p_SEv2[i, j])

n_EsIs[,] <- rbinom(Es[i, j], p_EI)
n_Ev1Iv1[,] <- rbinom(Ev1[i, j], p_EI)
n_Ev12v2[,] <- rbinom(Ev2[i, j], p_EI)

n_IsR[,] <- rbinom(Is[i, j], p_IR)
n_Iv1R[,] <- rbinom(Iv1[i, j], p_IR)
n_Iv2R[,] <- rbinom(Iv2[i, j], p_IR)



#### 3- Compute the probabilities of transition between each compartment: ####
## From S to E depends on lambda_t, the force of infection (per age / region / date)
p_SE[,] <- 1 - exp(-lambda_t[i, j] * dt) # S to E
## From v1 and v2 to E also depends on lambda, and v1/v2, the protection from infection 
## brought by the vaccine 
p_SEv1[,] <- 1 - exp(-lambda_t[i, j] * v1 * dt) # V1 to Ev1
p_SEv2[,] <- 1 - exp(-lambda_t[i, j] * v2 * dt) # V2 to Ev2
## From E to I depends on the duration of the latent period
p_EI <- 1 - exp(-alpha * dt) # E to I
## From I to R depends on the duration of the infectious period
p_IR <- 1 - exp(-gamma * dt) # I to R

## Define the duration of  the latent / infectious periods
alpha <- user(0.1)
gamma <- user(0.1)

## Define the default protection from infection brought by the vaccine
v1 <- user(.5)
v2 <- user(.1)



#### 4- Compute lambda_t, the force of infection including the impact of seasonnality ####

lambda_t[,] <- lambda[i,j] * 
  exp(X * cos(2 * 3.14 * time / 365) + Y * sin(2 * 3.14 * time / 365))
# Default values of X and Y (seasonnality parameters)
X <- user(1)
Y <- user(1)

### Compute the force of infection from beta (the average nb of contacts), 
### N (the total number of inhabitants) and case_ij (the potential of infection
### stemming from the currently infectious cases, depending on the age / region)
lambda[,] <- beta * cases_ij[i, j] / N

## Default values of beta (average number of contacts)
beta <- user(1.8)
## Total population size
N <- sum(S[,]) + sum(V1[,]) + sum(V2[,]) + sum(Es[,]) + sum(Ev1[,]) + sum(Ev2[,]) + 
  sum(Is[,]) + sum(Iv1[,]) + sum(Iv2[,]) + sum(R[,])

## Compute the potential of infection given the current number of infectious cases
## in each age group / region. Transmission from cases A (age i, region j) to 
## B (age k, region l) is m[i, k] * d[j, l]**-a
## with m the age contact matrix, d the distance matrix, and a the spatial kernel parameter
## Therefore, we compute cases_ijkl (the number of connection from [i,j] to [k,l])
cases_ijkl[, , , ] <- m[i, k] * d_a[j, l] *
  (Is[k, l] + vacc1 * Iv1[k, l] + vacc2 * Iv2[k, l])
## with vacc1/ vacc2: protection from onwards transmission brought by vaccination 
## and d_a the distance matrix after applying the spatial kernel
d_a[,] <- (d[i, j]^(a))

## We sum over k and l to get the total potential for transmission towards individuals 
## in (i,j)
cases_ijk[, , ] <- sum(cases_ijkl[i, j, k, ])
cases_ij[, ] <- sum(cases_ijk[i, j, ])

# Dimension of the transmission matrices
dim(cases_ijkl) <- c(N_age, N_reg, N_age, N_reg)
dim(cases_ijk) <- c(N_age, N_reg, N_age)
dim(cases_ij) <- c(N_age, N_reg)
# Dimension of the force of infection matrices
dim(lambda) <- c(N_age, N_reg)
dim(lambda_t) <- c(N_age, N_reg)
# Dimension of the distance matrices and age contact matrix
dim(d_a) <- c(N_reg, N_reg)
dim(m) <- c(N_age, N_age)
dim(d) <- c(N_reg, N_reg)

# Default value of the matrices and parameters
m[, ] <- user() # No default value, has to be defined by the user
d[, ] <- user() # No default value, has to be defined by the user
a <- user(2)
N_age <- user(2)
N_reg <- user(2)
vacc1 <- user(.5)
vacc2 <- user(.1)



#### 5- Compute the number of importations ####

### Extract the number of importation per age / region at time (step + 1)
import_t[,] <- import[i, j, step + 1]

## Define time step
dt <- user(1)
initial(time) <- 0
update(time) <- (step + 1) * dt

## Initialise the number of importations / dimensions of the matrices
import[,,] <- user()
dim(import) <- c(N_age, N_reg, N_time)
dim(import_t) <- c(N_age, N_reg)
N_time <- user(2)


#### 6- Compute ageing ####

## Draw number of new susceptibles
new_birth[] <- rpois(array_new[i, step])
array_new[,] <- user()
dim(array_new) <- c(N_reg, N_time)
dim(new_birth) <- c(N_reg)

mean_S[,] <- if(step %% 365 == 1) S[i, j] / 365 else mean_S[i,j]
mean_V1[,] <- if(step %% 365 == 1) V1[i, j] / 365 else mean_V1[i,j]
mean_V2[,] <- if(step %% 365 == 1) V2[i, j] / 365 else mean_V2[i,j]
mean_R[,] <- if(step %% 365 == 1) R[i, j] / 365 else mean_R[i,j]
mean_RV1[,] <- if(step %% 365 == 1) RV1[i, j] / 365 else mean_RV1[i,j]
mean_RV2[,] <- if(step %% 365 == 1) RV2[i, j] / 365 else mean_RV2[i,j]


## Compute overall number of movements between compartments
n_S[,] <- rpois(mean_S[i, j])
n_V1[,] <- rpois(mean_V1[i, j])
n_V2V2[,] <- rpois(mean_V2[i, j])
n_R[,] <- rpois(mean_R[i, j])
n_RV1[,] <- rpois(mean_RV1[i, j])
n_RV2RV2[,] <- rpois(mean_RV2[i, j])
len_ageing <- N_age - 1

## Number of ageing movements between specific compartments
# In susceptible compartments
n_SV1[1 : len_ageing,] <- rbinom(n_S[i, j], array_cov1[i, j, step])
n_SS[1 : len_ageing,] <-  n_S[i, j] - n_SV1[i, j]

# n_V1V2[1 : len_ageing,] <- rbinom(n_V1[i, j], array_cov2[i, j, step])
prop_v1v2[1:len_ageing,] <- if(step == 1 || i == 1) 0 else
  (array_cov2[i, j, step] - array_cov2[i - 1, j, step - 1]) / array_cov1[i - 1, j, step - 1]
dim(prop_v1v2) <- c(len_ageing, N_reg)
n_V1V2[1 : len_ageing,] <- rbinom(n_V1[i, j], prop_v1v2[i, j])
n_V1V1[1 : len_ageing,] <- n_V1[i, j] - n_V1V2[i, j]

array_cov1[,,] <- user()
dim(array_cov1) <- c(len_ageing, N_reg, N_time)
array_cov2[,,] <- user()
dim(array_cov2) <- c(len_ageing, N_reg, N_time)


# In Recovered compartments
n_RRV1[,] <- rbinom(n_R[i, j], array_cov1[i, j, step])
n_RR[,] <- n_R[i, j] - n_RRV1[i, j]
n_RV1RV2[,] <- rbinom(n_RV1[i, j], array_cov2[i, j, step])
n_RV1RV1[,] <- n_RV1[i, j] - n_RV1RV2[i, j]

#### 7- Initial conditions ####

initial(S[,]) <- S_ini[i, j]
initial(V1[,]) <- V1_ini[i, j]
initial(V2[,]) <- V2_ini[i, j]
initial(Es[,]) <- Es_ini[i, j]
initial(Ev1[,]) <- Ev1_init[i, j]
initial(Ev2[,]) <- Ev2_init[i, j]
initial(Is[,]) <- Is_init[i, j]
initial(Iv1[,]) <- Iv1_init[i, j]
initial(Iv2[,]) <- Iv2_init[i, j]
initial(R[,]) <- R_init[i, j]
initial(RV1[,]) <- RV1_init[i, j]
initial(RV2[,]) <- RV2_init[i, j]
initial(new_IS[, ]) <- 0
initial(new_IV1[, ]) <- 0
initial(new_IV2[, ]) <- 0

## Default values
S_ini[,] <- user(100)
V1_ini[,] <- user(100)
V2_ini[,] <- user(800)
Es_ini[,] <- user(0)
Ev1_init[,] <- user(0)
Ev2_init[,] <- user(0)
Is_init[,] <- user(1)
Iv1_init[,] <- user(0)
Iv2_init[,] <- user(0)
R_init[,] <- user(0)
RV1_init[,] <- user(0)
RV2_init[,] <- user(0)

### Dimensions of each compartment
dim(S) <- c(N_age, N_reg)
dim(V1) <- c(N_age, N_reg)
dim(V2) <- c(N_age, N_reg)
dim(Es) <- c(N_age, N_reg)
dim(Ev1) <- c(N_age, N_reg)
dim(Ev2) <- c(N_age, N_reg)
dim(Is) <- c(N_age, N_reg)
dim(Iv1) <- c(N_age, N_reg)
dim(Iv2) <- c(N_age, N_reg)
dim(R) <- c(N_age, N_reg)
dim(RV1) <- c(N_age, N_reg)
dim(RV2) <- c(N_age, N_reg)
dim(new_IS) <- c(N_age, N_reg)
dim(new_IV1) <- c(N_age, N_reg)
dim(new_IV2) <- c(N_age, N_reg)

### Initialise the dimensions of the compartments' initial value
dim(S_ini) <- c(N_age, N_reg)
dim(V1_ini) <- c(N_age, N_reg)
dim(V2_ini) <- c(N_age, N_reg)
dim(Es_ini) <- c(N_age, N_reg)
dim(Ev1_init) <- c(N_age, N_reg)
dim(Ev2_init) <- c(N_age, N_reg)
dim(Is_init) <- c(N_age, N_reg)
dim(Iv1_init) <- c(N_age, N_reg)
dim(Iv2_init) <- c(N_age, N_reg)
dim(R_init) <- c(N_age, N_reg)
dim(RV1_init) <- c(N_age, N_reg)
dim(RV2_init) <- c(N_age, N_reg)


### Initialise the dimensions of the number of individual moving between the compartments
dim(n_SEs) <- c(N_age, N_reg)
dim(n_v1E) <- c(N_age, N_reg)
dim(n_v2E) <- c(N_age, N_reg)
dim(n_EsIs) <- c(N_age, N_reg)
dim(n_Ev1Iv1) <- c(N_age, N_reg)
dim(n_Ev12v2) <- c(N_age, N_reg)
dim(n_IsR) <- c(N_age, N_reg)
dim(n_Iv1R) <- c(N_age, N_reg)
dim(n_Iv2R) <- c(N_age, N_reg)
dim(p_SE) <- c(N_age, N_reg)
dim(p_SEv1) <- c(N_age, N_reg)
dim(p_SEv2) <- c(N_age, N_reg)

dim(n_SS) <- c(len_ageing, N_reg)
dim(n_SV1) <- c(len_ageing, N_reg)
dim(n_V1V1) <- c(len_ageing, N_reg)
dim(n_V1V2) <- c(len_ageing, N_reg)
dim(n_V2V2) <- c(len_ageing, N_reg)
dim(n_RR) <- c(len_ageing, N_reg)
dim(n_RRV1) <- c(len_ageing, N_reg)
dim(n_RV1RV2) <- c(len_ageing, N_reg)
dim(n_RV1RV1) <- c(len_ageing, N_reg)
dim(n_RV2RV2) <- c(len_ageing, N_reg)
dim(n_S) <- c(len_ageing, N_reg)
dim(n_V1) <- c(len_ageing, N_reg)
dim(n_R) <- c(len_ageing, N_reg)
dim(n_RV1) <- c(len_ageing, N_reg)
dim(mean_S) <- c(len_ageing, N_reg)
dim(mean_V1) <- c(len_ageing, N_reg)
dim(mean_V2) <- c(len_ageing, N_reg)
dim(mean_R) <- c(len_ageing, N_reg)
dim(mean_RV1) <- c(len_ageing, N_reg)
dim(mean_RV2) <- c(len_ageing, N_reg)
