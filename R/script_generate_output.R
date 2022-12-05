## Script:
## Import odin.dust model, define model variables, run model, plot outputs

## 4 regions ; 3 age groups

#### Import libraries ####

library(dplyr)
library(odin.dust)
library(mcstate)
library(tictoc)


#### Define model and variables ####

## Import odin.dust model
si_age <- odin.dust::odin_dust("R/model_odin_dust.R")


## Define number of contacts
beta <- 1

## Define vaccine efficacy
# Against infection
v1 <- .5
v2 <- .01
# Against onwards infection
vacc1 <- .7
vacc2 <- .5

## Define spatial kernel parameter
a <- 2.5

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

## Number of inhabitants in each compartment
S <- round(N * (1 - recov) * unvax, 0)
V1 <- round(N * (1 - recov) * cov1, 0)
V2 <- round(N * (1 - recov) * cov2, 0)
R <- round(N * recov * unvax, 0)
RV1 <- round(N * recov * cov1, 0)
RV2 <- round(N * recov * cov2, 0)
if(any(R < 0)) stop()

## Define the seasonality parameters
X <- .2
Y <- .4

## Define time step
dt <- 1
## Define the number of particules (i.e. the number of stochastic runs)
n_part <- 500

## Duration of the run (in days)
t_tot <- 365 * 4

## Mean number of import by day
mean_import <- exp(.2 * cos(2 * 3.14 * seq_len(t_tot) / 365) + 
                     1.5 * sin(2 * 3.14 * seq_len(t_tot) / 365) - 4)
# Draw the number of importations (by age, region, and day)
import <- array(round(rpois(n = t_tot * nrow(ref_m) * nrow(ref_d), 
                            rep(mean_import, each = nrow(ref_m) * nrow(ref_d)))),
                dim = c(nrow(ref_m), nrow(ref_d), t_tot))

#### Vaccine coverage through time ####

# Dimensions
N_age <- nrow(ref_m)
N_reg <- nrow(ref_d)
N_time <- dim(import)[3]

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


new_birth <- matrix(c(2e4, 8e4, 1e4, 5e3, 4e4) / 365, ncol = N_time, nrow = N_reg)


#### Model runs ####

## Initialise the model using the parameters previously defined
seir_model <- si_age$new(pars = list(m = ref_m, d = 1/ref_d, a = a, 
                                     beta = beta, X = X, Y = Y,
                                     
                                     v1 = v1, v2 = v2, vacc1 = vacc1, vacc2 = vacc2, 
                                     
                                     import = import, 
                                     
                                     N_time = N_time, N_age = N_age, N_reg = N_reg, 
                                     
                                     S_ini = S, V1_ini = V1, V2_ini = V2, 
                                     Es_ini = S * 0, Ev1_init = S * 0, Ev2_init = S * 0,
                                     Is_init = S * 0, Iv1_init = S * 0, Iv2_init = S * 0,
                                     R_init = R, RV1_init = RV1, RV2_init = RV2,
                                     
                                     array_cov1 = array_cov1[-1,,], array_cov2 = array_cov2[-1,,],
                                     array_new = new_birth, dt = 1),
                        step = 1, n_particles = n_part, n_threads = 1L, seed = 1L)


## Define array output_sim, containing the number of individuals in each compartment per day
## in each simulation
output_sim <- array(NA, dim = c(seir_model$info()$len, n_part, t_tot))

## Rename the rows of output_sim, as the name of the state + region + age group
states <- c("S", "V1", "V2", "Es", "Ev1", "Ev2", "Is", "Iv1", "Iv2", "R", "RV1", 
            "RV2", "new_IS", "new_IV1", "new_IV2")
## The first row of output_sim contains the time
rownames(output_sim) <- c("Time",
  paste0(rep(paste0(rep(states, each = nrow(ref_d)), 
                    "_reg", rep(seq_len(nrow(ref_d)), 10)), each = nrow(ref_m)), 
         "_age", seq_len(nrow(ref_m)))
)

# For loop to run the model iteratively and store the results in output_sim
for (t in seq_len(t_tot)) {
  output_sim[ , , t] <- seir_model$run(t)
}
output_sim[grep("_reg1_", rownames(output_sim)), 1, 1:10]

#### Analyse simulations / generate plots ####

## Extract time and number of individuals per compartmemt
time <- output_sim[1, 1, ]
output <- output_sim[-1, , ]

## Extract the distribution of the population at the last time step
final_res <- output_sim[, , dim(output)[3]]

## Compute the total number of cases per region across simulations
print(summary(apply(output_sim[grep("new_I", rownames(final_res)), ,], 2, sum)))
## Compute the total number of cases per region across simulations
print(summary(apply(output_sim[grep("new_IS_", rownames(final_res)), ,], 2, sum)))
## Compute the total number of single vaccinated cases per region across simulations
print(summary(apply(output_sim[grep("new_IV1_", rownames(final_res)), ,], 2, sum)))
## Compute the total number of double vaccinated cases per region across simulations
print(summary(apply(output_sim[grep("new_IV2_", rownames(final_res)), ,], 2, sum)))


### Generate plot of the number of Susceptibles, Infected and Recovered individuals through time
### per region / age group
png(filename = "Output/fig_sir_import_age.png", width = 600, height = 800)
par(mfrow = c(nrow(ref_d),nrow(ref_m)), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l")

## Define colour scheme
cols <- c(S = "#8c8cd9", I = "#cc0044", R = "#999966")

for(i in seq_len(nrow(ref_d))){
  for(j in seq_len(nrow(ref_m))){
    ## Extract entries from region i and age j
    lab_i_j <- paste0("_reg", i, "_age", j)
    ## Compute total number of susceptibles + vaccinated
    susceptibles <- colSums(output[c(paste0("S", lab_i_j), paste0("V1", lab_i_j), paste0("V2", lab_i_j)),, ])
    ## Compute total number of infectious
    infected <- colSums(output[c(paste0("Is", lab_i_j), paste0("Iv1", lab_i_j), paste0("Iv2", lab_i_j)),, ])
    ## Compute total number of recovered
    recovered <- colSums(output[c(paste0("R", lab_i_j), paste0("RV1", lab_i_j), paste0("RV2", lab_i_j)),, ])

    ## Plot the number of susceptible individuals through time
    matplot(time, t(susceptibles), type = "l", xlab = "", ylab = "", yaxt="none", 
            main = paste0("Age ", j, "Region", i), col = cols[["S"]], lty = 1, 
            ylim = range(c(susceptibles, infected, recovered)))
    
    ## Add the number of infected
    matlines(time, t(infected), col = cols[["I"]], lty = 1)
    
    ## Add the number of recovered
    matlines(time, t(recovered), col = cols[["R"]], lty = 1)
    
    ## Add the legend in the topleft panel
    if(i == 1 & j == 1)
      legend("right", lwd = 1, col = cols, legend = names(cols), bty = "n")
    axis(2, las =2)
  }
}

# Add axis names
title(xlab = "Time (days)", outer = T, line = 0, cex.lab = 2)
title(ylab = "Number of individuals", outer = T, line = 0, cex.lab = 2)

dev.off()

### Generate plot of the number of Infected per region / age group, stratified by vaccination status
png(filename = "Output/fig_new_cases_import_age.png", width = 600, height = 800)
par(mfrow = c(nrow(ref_d),nrow(ref_m)), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l")

## Define colour scheme
cols <- c(Is = "#8c8cd9", Iv1 = "#cc0044", Iv2 = "#999966")

for(i in seq_len(nrow(ref_d))){
  for(j in seq_len(nrow(ref_m))){
    # Extract the rownames of all rows of interest
    lab_i_j <- c(paste0("new_IS_reg", i, "_age", j), paste0("new_IV1_reg", i, "_age", j),
                 paste0("new_IV2_reg", i, "_age", j))
    
    ## Plot the number of non-vaccinated infectious individuals at each date
    matplot(time, t(output[lab_i_j[1],, ]), type = "l", 
            xlab = "", ylab = "", yaxt="none", main = paste0("Age ", j, "Region", i),
            col = cols[["Is"]], lty = 1, ylim=range(output[lab_i_j,,]))
    
    ## Add the number of single-vaccinated infectious indviduals
    matlines(time, t(output[lab_i_j[2], , ]), col = cols[["Iv1"]], lty = 1)
    
    ## Add the number of double-vaccinated infectious indviduals
    matlines(time, t(output[lab_i_j[3], , ]), col = cols[["Iv2"]], lty = 1)
    
    ## Add a legend to the topleft panel
    if(i == 1 & j == 1)
      legend("right", lwd = 1, col = cols, legend = names(cols), bty = "n")
    
    axis(2, las =2)
    
  }
}
# Add axis names
title(xlab = "Time (days)", outer = T, line = 0, cex.lab = 2)
title(ylab = "Number of individuals", outer = T, line = 0, cex.lab = 2)

dev.off()

## Plot beta_t, the number of contacts (accounting for seasonality) 
par(mfrow = c(2, 1), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l")
plot(beta * exp(X * cos(2 * 3.14 * (1:365) / 365) + Y * sin(2 * 3.14 * (1:365) / 365)),
     ylab = "Nb of infectious contacts", xlab = "Time (days)")
## Plot the number of importations per day across the simulation period
plot(apply(import, 3, sum), type = "l", 
     ylab = "Nb of importations", xlab = "Time (days)")

### Generate plot of the vaccine distribution per age group / region
### per region / age group
png(filename = "Output/fig_vaccine_import_age.png", width = 600, height = 800)
par(mfrow = c(nrow(ref_d),nrow(ref_m)), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l")

## Define colour scheme
cols <- c(Unvaccinated = "#8c8cd9", V1 = "#cc0044", V2 = "#999966")

for(i in seq_len(nrow(ref_d))){
  for(j in seq_len(nrow(ref_m))){
    ## Extract entries from region i and age j
    lab_i_j <- paste0("_reg", i, "_age", j)
    ## Compute total number of non-vaccinated
    unvax <- colSums(output[c(paste0("S", lab_i_j), paste0("Es", lab_i_j), paste0("Is", lab_i_j), 
                              paste0("R", lab_i_j)),, ])
    ## Compute total number of vaccinated once
    v1 <- colSums(output[c(paste0("V1", lab_i_j), paste0("Ev1", lab_i_j), paste0("Iv1", lab_i_j), 
                           paste0("RV1", lab_i_j)),, ])
    ## Compute total number of vaccinated twice
    v2 <- colSums(output[c(paste0("V2", lab_i_j), paste0("Ev2", lab_i_j), paste0("Iv2", lab_i_j), 
                           paste0("RV2", lab_i_j)),, ])
    
    N <- unvax + v1 + v2 
    
    ## Plot the number of susceptible individuals through time
    matplot(time, t(unvax / N), type = "l", xlab = "", ylab = "", yaxt = "none", 
            main = paste0("Age ", j, "Region", i), col = cols[["Unvaccinated"]], lty = 1, 
            ylim = c(0, 1))
    
    ## Add the number of infected
    matlines(time, t(v1 / N), col = cols[["V1"]], lty = 1)
    
    ## Add the number of recovered
    matlines(time, t(v2 / N), col = cols[["V2"]], lty = 1)
    
    ## Add the legend in the topleft panel
    if(i == 1 & j == 1)
      legend("right", lwd = 1, col = cols, legend = names(cols), bty = "n")
    axis(2, las =2)
  }
}

dev.off()
