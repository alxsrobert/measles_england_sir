set.seed(1)
## Script:
## Import odin.dust model, define model variables, run model, plot outputs

# vax <- "cprd" # "cprd" "cover"
# 
# scenario_import <- "per_year" # "pop" "epi" "vax" "per_year"
# 
# distance <- "degree" # "degree" "stouffer"
# 
# waning <- "since_eli" # "no" "since_vax" "since_eli" "only_2000s"
# 
scenario <- "reference"
vacc_yes <- "v1"
n_steps <- 10
# 
# n_steps <- 50

#### Import libraries ####

library(dplyr)
library(socialmixr)
library(odin.dust)
library(mcstate)
library(tictoc)
library(tidyr)
library(ggplot2)
library(data.table)
library(excel.link)
library(patchwork)

#### Define model and variables ####

source("R/function_import_data.R")
## Import odin.dust model

si_age <- odin.dust::odin_dust("R/model_odin_dust.R")

## Define time step
dt <- 1

## Duration of the run (in days)
N_year <- 10 
N_time <- t_tot <- 365 * N_year
year_start <- 2010

state_names <- c("new_IS", "new_IV1", "new_IV2")
regions <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
             "West Midlands", "East", "London", "South East", "South West")
age <- c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
         "[15,20)", "[20,30)", "[30,40)", "[40,100]"
)

particle_data <- import_case_data(state_names = state_names, regions = regions, 
                                  age = age, vacc_yes = vacc_yes)


wide_data <- particle_data[,grepl("new", colnames(particle_data))]

dates <- as.Date("2010-01-01") + seq_len(nrow(wide_data)) - 1

wide_data$time <- dates
long_data <- pivot_longer(wide_data, !time, names_to = "id", values_to = "cases")
long_data$vacc <- unlist(lapply(strsplit(long_data$id, "_"), function(X) return(X[[2]])))
long_data$region <- unlist(lapply(strsplit(long_data$id, "_"), function(X) return(X[[3]])))
long_data$age <- unlist(lapply(strsplit(long_data$id, "_"), function(X) return(X[[4]])))

long_data[long_data$vacc == "IS",]$vacc <- "Unvaccinated"
long_data[long_data$vacc == "IV1",]$vacc <- "One dose"
long_data[long_data$vacc == "IV2",]$vacc <- "Two doses"
long_data$vacc <- factor(long_data$vacc, c("Unvaccinated", "One dose", "Two doses"))

if(!exists("map")){
  temp <- tempfile()
  download.file("https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_rgn_2011.zip",
                temp)
  
  ## Extract the shapefile into a temporary folder
  unzip(temp, exdir = tempdir())
  ## Read the shapefile and create a map
  map <- sf::st_read(paste0(tempdir(), "/infuse_rgn_2011.shp"), 
                     quiet = T)
  
  map[map$geo_label == "East of England",]$geo_label <- "East"
  colnames(map)[which(colnames(map) == "geo_label")] <- "region"
}

#### Generate figure ####

p1 <- 
  (long_data %>% group_by(time, vacc) %>% summarise(sum_cases = sum(cases)) %>% 
  ggplot(aes(x = time, y = sum_cases, fill = vacc))) + geom_col() + theme_bw() +
  xlab("Time (days)") + ylab("Number of cases") + labs(title = "A") +
  theme(legend.position="none",
        axis.text=element_text(size = 10),
        title=element_text(size = 14))

p2 <- 
  long_data %>% group_by(year = year(time)) %>% mutate(sum_cases = sum(cases)) %>% 
  group_by(year = year(time), vacc = (vacc != "Two doses")) %>% 
  summarise(prop_cases = sum(cases)/unique(sum_cases)) %>% 
  mutate(vacc = ifelse(vacc == FALSE, "Two doses", "Other")) %>% 
  filter(vacc == "Two doses") %>% 
  ggplot(aes(x = as.factor(year), y = prop_cases)) + geom_point() + theme_bw() + 
  scale_y_continuous(limits = c(0, .1))+
  xlab("Time (years)") + ylab("Proportion of double-vaccinated cases") + 
  labs(title = "C") + 
  theme(axis.text=element_text(size = 10),
        title=element_text(size = 14))


p3 <- long_data %>% group_by(age,vacc) %>% summarise(sum_cases = sum(cases)) %>% 
  mutate(age = factor(age, c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", 
                             "[5,6)", "[6,10)", "[10,15)", "[15,20)", "[20,30)",
                             "[30,40)", "[40,100]"))) %>% 
  ggplot(aes(x = age, y = sum_cases, fill = vacc)) + geom_col() + theme_bw() +
  xlab("Age group") + ylab("Number of cases") + labs(fill = "Vaccination status") +
  theme(axis.text=element_text(size = 10), title=element_text(size = 14)) +
  labs(title = "B")



p4 <- long_data %>% group_by(region) %>% summarise(sum_cases = sum(cases)) %>% 
  left_join(map, ., by = "region") %>% 
  ggplot() + geom_sf(aes(fill = sum_cases)) + 
  scale_fill_gradient(low = "white", high = "brown") + 
  labs(fill = "Number of cases") + theme_void() + labs(title = "D") + 
  theme(title=element_text(size = 14))


p_tot <- (p1|p3)/(p2|p4)
print(p_tot)



