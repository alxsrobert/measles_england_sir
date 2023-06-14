set.seed(1)
## Script:
## Import odin.dust model, define model variables, run model, plot outputs


#### Import libraries ####

library(dplyr)
library(socialmixr)
library(odin.dust)
library(mcstate)
library(tictoc)
library(tidyr)
library(ggplot2)
library(data.table)

#### Define model and variables ####

## Import odin.dust model
si_age <- odin.dust::odin_dust("R/model_odin_dust.R")


## Define number of contacts
beta <- 1.75

## Define vaccine efficacy
# Against infection
v1 <- .5
v2 <- .01
# Against onwards infection
vacc1 <- .7
vacc2 <- .5

## Define spatial kernel parameter
a <- 3

## Define the seasonality parameters
X <- .3
Y <- .02

## Define time step
dt <- 1
## Define the number of particles (i.e. the number of stochastic runs)
n_part <- 10

## Duration of the run (in days)
N_year <- 13
N_time <- t_tot <- 365 * N_year

# source("R/parametrise_model_sim.R")
source("R/parametrise_model_england.R")

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
                                     array_new = new_birth, dt = 1, 
                                     year_per_age = year_per_age
                                     ),
                        time = 1, n_particles = n_part, n_threads = 1L, seed = 1L)


## Define array output_sim, containing the number of individuals in each compartment per day
## in each simulation
output_sim <- array(NA, dim = c(seir_model$info()$len, n_part, t_tot))

## Rename the rows of output_sim, as the name of the state + region + age group
states <- c("S", "V1", "V2", "Es", "Ev1", "Ev2", "Is", "Iv1", "Iv2", "R", "RV1", 
            "RV2", "new_IS", "new_IV1", "new_IV2")
## The first row of output_sim contains the time
rownames(output_sim) <- c("Time", "iter", "new_IV1_tot", "new_IV2_tot",
  paste0(rep(paste0(rep(states, each = nrow(ref_d)), 
                    "_reg", rep(seq_len(nrow(ref_d)), 10)), each = nrow(ref_m)), 
         "_age", seq_len(nrow(ref_m)))
)

# For loop to run the model iteratively and store the results in output_sim
for (t in seq_len(t_tot)) {
  output_sim[ , , t] <- seir_model$run(t)
}
output_sim[grep("_reg1_", rownames(output_sim)), 1, 1:10]

if(exists("year_start")){
  output_sim["Time",,] <- as.Date(paste0(year_start, "-01-01")) + output_sim["Time",,] - 1
}

#### Number of cases per region ####

source("R/function_figure.R")

## Extract time and number of individuals per compartmemt
## Extract the distribution of the population at the last time step
final_res <- output_sim[, , dim(output_sim)[3]]

## Compute the total number of cases per region across simulations
print(summary(apply(output_sim[grep("new_I", rownames(final_res)), ,], 2, sum)))

par(mfrow = c(1, 1), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l") 
proportion_outbreak(dt_output = output_sim, names_reg = rownames(ref_d), which_reg = NA)


if(nrow(ref_d) > 3 | nrow(ref_m) > 3){
  n_row <- n_col <- 3
  lab_plot <- "%02d" 
} else {
  n_row <- nrow(ref_m)
  n_col <- nrow(ref_d)
  lab_plot <- ""
}

par(mfrow = c(n_row, n_col), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l") 
proportion_outbreak(dt_output = output_sim, names_reg = rownames(ref_d),
                    which_reg = seq_len(9))

par(mfrow = c(n_row, n_col), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l") 
## Define colour scheme
cols <- c(Is = "#8c8cd9", Iv1 = "#cc0044", Iv2 = "#999966")
## Three categories: New unvaccinated infected - new infected vaccinated 1x - new infected vaccinated 2x
categories <- list("new_IS", "new_IV1", "new_IV2")
stratified_plot(by_age = F, by_reg = T, N_reg = N_reg, N_age = N_age, legend = T,  
                dt_output = output_sim[,,], cats = categories, colour = cols, prop = F,  
                main_lab = NA, outer_y = T, y_lab = "Number of individuals", 
                names_reg = rownames(ref_d), names_age = rownames(ref_m), aggreg = "week")

stratified_plot(by_age = T, by_reg = F, N_reg = N_reg, N_age = N_age, legend = T,  
                dt_output = output_sim[,,], cats = categories, colour = cols, prop = F,  
                main_lab = NA, outer_y = T, y_lab = "Number of individuals", 
                names_reg = rownames(ref_d), names_age = rownames(ref_m), aggreg = "week")
#### Generate plots stratified by age / region ####


### Generate plot of the number of Susceptibles, Infected and Recovered individuals through time
### per region / age group
message("Figures distribution of the compartments")
png(filename = paste0("Output/sir/fig_sir_import_age", lab_plot, ".png"), 
    width = 600, height = 800)
par(mfrow = c(n_row, n_col), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l") 

## Define colour scheme
cols <- c(S = "#8c8cd9", I = "#cc0044", R = "#999966")
## Three categories: Susceptibles - Infected - Recovered
categories <- list(c("S", "V1", "V2"), c("IS", "Iv1", "Iv2"), c("R", "RV1", "RV2"))
stratified_plot(by_age = T, by_reg = T, N_reg = N_reg, N_age = N_age, legend = T,
                dt_output = output_sim, cats = categories, colour = cols, prop = F,
                main_lab = NA, outer_y = T, y_lab = "Number of individuals", 
                names_reg = rownames(ref_d), names_age = rownames(ref_m), aggreg = "week")

dev.off()

### Generate plot of the number of Infected per region / age group, stratified by vaccination status
message("Figures new cases")
png(filename = paste0("Output/new_cases/fig_new_cases_import_age", lab_plot, ".png"), 
    width = 600, height = 800)
par(mfrow = c(n_row, n_col), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l") 
## Define colour scheme
cols <- c(Is = "#8c8cd9", Iv1 = "#cc0044", Iv2 = "#999966")
## Three categories: New unvaccinated infected - new infected vaccinated 1x - new infected vaccinated 2x
categories <- list("new_IS", "new_IV1", "new_IV2")
stratified_plot(by_age = T, by_reg = T, N_reg = N_reg, N_age = N_age, legend = T,  
                dt_output = output_sim, cats = categories, colour = cols, prop = F,  
                main_lab = NA, outer_y = T, y_lab = "Number of individuals", 
                names_reg = rownames(ref_d), names_age = rownames(ref_m), aggreg = "week")

dev.off()

### Generate plot of the vaccine distribution per age group / region
### per region / age group
message("Figures distribution vaccines")
png(filename = paste0("Output/vaccine/fig_vaccine_import_age", lab_plot, ".png"), 
    width = 600, height = 800)
par(mfrow = c(n_row, n_col), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l") 
cols <- c(Unvaccinated = "#8c8cd9", V1 = "#cc0044", V2 = "#999966")
## Three categories: Unvaccinated - Vaccinated 1x - Vaccinated 2x
categories <- list(c("S", "Es", "Is", "R"), c("V1", "Ev1", "Iv1", "RV1"), 
                   c("V2", "Ev2", "Iv2", "RV2"))
stratified_plot(by_age = T, by_reg = T, N_reg = N_reg, N_age = N_age, legend = T, 
                dt_output = output_sim, cats = categories, colour = cols, prop = T,
                main_lab = NA, outer_y = T, y_lab = "Proportion of individuals", 
                names_reg = rownames(ref_d), names_age = rownames(ref_m), aggreg = "week")

dev.off()

## Plot beta_t, the number of contacts (accounting for seasonality) 
par(mfrow = c(2, 1), mar = c(3, 4, 2, 0.5), mar = c(3, 4, 1, 0), 
    oma = c(2, 2, 0, 2), las = 1, bty = "l")
plot(beta * exp(X * cos(2 * 3.14 * (1:365) / 365) + Y * sin(2 * 3.14 * (1:365) / 365)),
     ylab = "Nb of infectious contacts", xlab = "Time (days)")
## Plot the number of importations per day across the simulation period
plot(apply(import, 3, sum), type = "l", 
     ylab = "Nb of importations", xlab = "Time (days)")

#### Generate country-level plots ####


png(filename = "Output/fig_all.png", width = 600, height = 800)
par(mfrow = c(3, 1), mar = c(3, 6, 1, 2), oma = c(2, 0, 1, 0), las = 1, bty = "l") 

### Generate plot of the number of Susceptibles, Infected and Recovered individuals through time
### per region / age group
categories <- list(c("S", "V1", "V2"), c("Is", "Iv1", "Iv2"), 
                   c("R", "RV1", "RV2"))

## Define colour scheme
cols <- c(S = "#8c8cd9", I = "#cc0044", R = "#999966")

stratified_plot(by_age = F, by_reg = F, N_reg = N_reg, N_age = N_age, 
                dt_output = output_sim, cats = categories, colour = cols, 
                main_lab = "Distribution of compartments", outer_y = T, 
                y_lab = "Number of individuals", legend = T, prop = F)

### Generate plot of the number of Infected per region / age group, stratified by vaccination status
categories <- list(c("new_IS"), c("new_IV1"), c("new_IV2"))

## Define colour scheme
cols <- c(Is = "#8c8cd9", Iv1 = "#cc0044", Iv2 = "#999966")
stratified_plot(by_age = F, by_reg = F, N_reg = N_reg, N_age = N_age, 
                dt_output = output_sim, cats = categories, colour = cols, 
                main_lab = "New cases by vaccine status", outer_y = T, 
                y_lab = "Number of individuals", legend = T, prop = F)

## Generate plot with the distribution of vaccine status
categories <- list(c("S", "Es", "Is", "R"), c("V1", "Ev1", "Iv1", "RV1"), 
                   c("V2", "Ev2", "Iv2", "RV2"))
## Define colour scheme
cols <- c(Unvaccinated = "#8c8cd9", V1 = "#cc0044", V2 = "#999966")
stratified_plot(by_age = F, by_reg = F, N_reg = N_reg, N_age = N_age, 
                dt_output = output_sim, cats = categories, colour = cols, 
                main_lab = "Distribution of vaccine coverage", outer_y = T, 
                y_lab = "Proportion of individuals", legend = T, prop = T)
dev.off()
