source("R/import_library.R")

## Generate model specifications
list_specs_run <- specs_run()

n_part <- 10
n_samples <- 5

# Extract vax, distance and sec from the input_parameters list
vax <- "cprd"
distance <- "degree"
sec <- FALSE

waning <- "since_eli"
## Read object containing model and parameter fits
pmcmc_run <- readRDS(paste0("Output/", vax, "_", distance, if(sec) "_sec", "/",
                            waning, ".RDS"))
set.seed(1)
## Generate stochastic simulations (n_part * n_samples simulations in total)
all_output <- generate_outbreaks(pmcmc_run, list_specs_run, vax, n_part, n_samples, 
                                 waning, aggreg = "no", burnin = 1000)

## Select the simulation with closest overall number of cases to actual data
print(which.min(abs(all_output %>% apply(c(2), sum) - 7504)))

## Add ID column and convert to data frame
sim_data <- cbind.data.frame(id = rownames(all_output), all_output[,40,])
## Pivot longer
long_data <- pivot_longer(as.data.frame(sim_data), cols = !id, names_to = "date",
                          values_to = "cases") %>% as.data.table
## Remove "new" from id
long_data[, id := gsub("new_", "", id)]

## Extract vaccinated, age and region from id
long_data[, vaccinated := gsub("_.*", "", id)]
long_data[, region := as.numeric(gsub("_.*", "", gsub(".*reg", "", id)))]
long_data[, age_groups := gsub(".*_", "", id)]

## Convert values of "vaccinated"
long_data[vaccinated == "IS", vaccinated := "no"]
long_data[vaccinated == "IV1", vaccinated := "mmr x1"]
long_data[vaccinated == "IV2", vaccinated := "mmr x2"]

## Convert age to actual age groups
long_data[, age_groups := as.numeric(gsub("age", "", age_groups))]
long_data[, age_groups := list_specs_run$age[age_groups]]

## Convert region to actual region names
long_data[, region := list_specs_run$regions[region]]

## Move date to as.Date object
long_data[, date := as.numeric(date)]
long_data[, date := as.Date("2010-01-01") + date - 1]

long_data[, id := NULL]
long_data <- long_data[cases > 0, ]
long_data <- long_data[order(date),]

## Create linelist
long_data <- long_data[,cbind(.SD,dup=1:cases),by= .(date, vaccinated, age_groups, region)]
long_data[, cases := NULL]
long_data[, dup := NULL]

# Save linelist
saveRDS(long_data, "Data/sim_data.RDS")
