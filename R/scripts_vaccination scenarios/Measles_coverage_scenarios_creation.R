#set up 
library(tidyverse)
library(rlang)
options(scipen = 999)

df <- read_parquet(paste0(datafiles, "outcomes.parquet"))
pop <- read_parquet(paste0(datafiles, "study_population.parquet"))


#NHS coverage data
nhs <- read_parquet(paste0(datafiles, "nhs_vacc.parquet"))
nhs[, region:= tolower(region)]
nhs[region == "yorkshire & the humber", region := "yorkshire and the humber"]



####calculating the vaccine coverage by age and region
pop[, region:= tolower(region)]
df[, region:= tolower(region)]

df$region <- factor(df$region, levels = 1:9, labels = c("north east", "north west", "yorkshire and the humber",
                                                        "east midlands", "west midlands", "east of england",
                                                        "london", "south east", "south west" ))
pop$region <- factor(pop$region, levels = 1:9, labels =  c("north east", "north west", "yorkshire and the humber",
                                                           "east midlands", "west midlands", "east of england",
                                                           "london", "south east", "south west" ))


all_regions <- levels(df$region)
all_years <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 
               2015, 2016, 2017, 2018, 2019)


####function to calculate a generic vaccine uptake at different ages
#establishing the different bdays
df <- df[, bday1 := yob+1]
df <- df[, bday2 := yob+2]
df <- df[, bday3 := yob+3]
df <- df[, bday4 := yob+4]
df <- df[, bday5 := yob+5]

pop <- pop[, bday1 := yob+1]
pop <- pop[, bday2 := yob+2]
pop <- pop[, bday3 := yob+3]
pop <- pop[, bday4 := yob+4]
pop <- pop[, bday5 := yob+5]

###############################################################################
#function creating data frame with all coverages
res <- data.table()
  
for(j in 1:length(all_regions)){
  
  df1 <- df[region == all_regions[j]]
  pop1 <- pop[region == all_regions[j]]  
  
  
  for(i in 1:length(all_years)){
    
    #age1 
    df_temp <- df1[bday1== all_years[i]]  
    pop_temp <- pop1[bday1== all_years[i]]  
    
    
    df1[vaccine == "MMR" & n_dose == 1 & bday1== all_years[i],
        cov1y := 0]
    df1[vaccine == "MMR" & n_dose == 2 & bday1== all_years[i],
        cov1y := 0]
    
    #age2
    
    df_temp <- df1[bday2== all_years[i]]  
    pop_temp <- pop1[bday2== all_years[i]]   
    
    df1[vaccine == "MMR" & n_dose == 1 & bday2== all_years[i], 
        cov2y := coverage_2y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday2== all_years[i], 
        cov2y := coverage_2y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
    
    #age 3
    df_temp <- df1[bday3== all_years[i]]  
    pop_temp <- pop1[bday3== all_years[i]]   

    df1[vaccine == "MMR" & n_dose == 1 & bday3== all_years[i], 
        cov3y := coverage_3y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday3== all_years[i], 
        cov3y := coverage_3y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
    
    #age 4
    df_temp <- df1[bday4== all_years[i]]  
    pop_temp <- pop1[bday4== all_years[i]]  
    
    df1[vaccine == "MMR" & n_dose == 1 & bday4== all_years[i], 
        cov4y := coverage_4y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday4== all_years[i], 
        cov4y := coverage_4y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
    
    
    #age 5
    df_temp <- df1[bday5== all_years[i]]  
    pop_temp <- pop1[bday5== all_years[i]]  
    
    df1[vaccine == "MMR" & n_dose == 1 & bday5== all_years[i], 
        cov5y := coverage_5y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday5== all_years[i], 
        cov5y := coverage_5y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
  }
  
  if(j ==1){
    
    tmp <- unique(df1[, list(yob, region, vaccine, n_dose,
                             cov1y, cov2y, cov3y, cov4y, cov5y,
                             bday1, bday2, bday3, bday4, bday5
                             )])
    tmp <- tmp[!is.na(n_dose)]
    res <- tmp
  }
  else{
    tmp <- unique(df1[, list(yob, region, vaccine, n_dose,
                             cov1y, cov2y, cov3y, cov4y, cov5y, 
                             bday1, bday2, bday3, bday4, bday5
                             )])
    tmp <- tmp[!is.na(n_dose)]
    res <-rbind(res, tmp)
    
  }
}

res <- res[vaccine == "MMR"]
cov1 <- res[, list(region, vaccine, n_dose, bday1, cov1y)]
cov1[, year := bday1]
cov1[, bday1 := NULL]
cov2 <- res[, list(region, vaccine, n_dose, bday2, cov2y)]
cov2[, year := bday2]
cov2[, bday2 := NULL]
cov3 <- res[, list(region, vaccine, n_dose, bday3, cov3y)]
cov3[, year := bday3]
cov3[, bday3 := NULL]
cov4 <- res[, list(region, vaccine, n_dose, bday4, cov4y)]
cov4[, year := bday4]
cov4[, bday4 := NULL]
cov5 <- res[, list(region, vaccine, n_dose, bday5, cov5y)]
cov5[, year := bday5]
cov5[, bday5 := NULL]

final_reg <- merge(cov1, cov2, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)
final_reg <- merge(final_reg, cov3, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)
final_reg <- merge(final_reg, cov4, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)
final_reg <- merge(final_reg, cov5, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)


write.table(final_reg, file =  paste0("Data/",
                                  "Coverage_reg_year_orig.csv"),
            sep = ";", dec = ".", row.names = FALSE)


###############################################################################
#reshaping the NHS data
nhs[vac_code == "MMR1_5y" | vac_code == "MMR2_5y" , cov5y_nhs := coverage]
nhs[vac_code == "MMR_24m" , cov2y_nhs := coverage]
nhs[, n_dose := dose]
nhs[, coverage := NULL]
nhs[, dose := NULL]
nhs[, age := NULL]
nhs[, vac_code := NULL]

nhs[, cov5y_nhs := max_NA(cov5y_nhs), by = c("region", "n_dose", "yob")]
nhs[, cov2y_nhs := max_NA(cov2y_nhs), by = c("region", "n_dose", "yob")]
df <- merge(final_reg, nhs, by = c("region" , "n_dose", "year"), all = T)


###adding the corrected NHS estimates
df[, cov2y_nhs_c := cov2y_nhs+(0.5*(1-cov2y_nhs))]
df[, cov5y_nhs_c := cov5y_nhs+(0.5*(1-cov5y_nhs))]
df[n_dose == 2, cov2y_nhs_c := NA]
df[yob <= 1997,cov2y_nhs_c := NA]
df[yob >= 2018, cov5y_nhs_c := NA]


################################################################################
#extrapolating the missing data
cov_new <- melt(final_reg, id = c("year", "region", "vaccine", "n_dose"))

cov_new[variable == "cov1y", birth_year := year-1]
cov_new[variable == "cov2y", birth_year := year-2]
cov_new[variable == "cov3y", birth_year := year-3]
cov_new[variable == "cov4y", birth_year := year-4]
cov_new[variable == "cov5y", birth_year := year-5]
cov_new[variable == "cov1y", age := 1]
cov_new[variable == "cov2y", age := 2]
cov_new[variable == "cov3y", age := 3]
cov_new[variable == "cov4y", age := 4]
cov_new[variable == "cov5y", age := 5]
cov_new[, coverage := value]

#reshaping
cov_new[variable == "cov1y", cov1y := coverage]
cov_new[variable == "cov2y", cov2y := coverage]
cov_new[variable == "cov3y", cov3y := coverage]
cov_new[variable == "cov4y", cov4y := coverage]
cov_new[variable == "cov5y", cov5y := coverage]


cov_new[,cov1y := max_NA(cov1y), by= c("region", "n_dose", "birth_year")]
cov_new[,cov2y := max_NA(cov2y), by= c("region", "n_dose", "birth_year")]
cov_new[,cov3y := max_NA(cov3y), by= c("region", "n_dose", "birth_year")]
cov_new[,cov4y := max_NA(cov4y), by= c("region", "n_dose", "birth_year")]
cov_new[,cov5y := max_NA(cov5y), by= c("region", "n_dose", "birth_year")]

cov_new <- cov_new[, list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]
cov_new <- unique(cov_new, by = c("region", "vaccine", "n_dose", "birth_year", 
                                  "cov1y", "cov2y", "cov3y", "cov4y", "cov5y"))

cov_new[, test5 := cov5y>=cov4y]
cov_new[, test4 := cov4y>=cov3y]
cov_new[, test3 := cov3y>=cov2y]
cov_new[, test2 := cov2y>=cov1y]

any(cov_new$test2 == F)
any(cov_new$test3 == F)
any(cov_new$test4 == F) #yorkshire born 2011, cov3<cov2
any(cov_new$test5 == F)

#correcting that error
cov_new[test3==F, cov3y := cov2y]

#extrapolating for other birth years
cov_new[, cov1y := 0.75*cov2y]

cov_new[, test2:=NULL]
cov_new[, test3:=NULL]
cov_new[, test4:=NULL]
cov_new[, test5:=NULL]

#round all the values
cov_new[, cov1y:= round(cov1y, digits = 4)]
cov_new[, cov2y:= round(cov2y, digits = 4)]
cov_new[, cov3y:= round(cov3y, digits = 4)]
cov_new[, cov4y:= round(cov4y, digits = 4)]
cov_new[, cov5y:= round(cov5y, digits = 4)]


cov_new <- cov_new[
  birth_year <2015, list(region, vaccine, n_dose,birth_year, cov1y, cov2y, cov3y,  cov4y, cov5y)]

#adding missing values from extrapolated NHS data
nhs_2y <- df[yob >=2000, list(yob, region, n_dose,  cov2y_nhs_c)]
nhs_5y <- df[yob >=2000, list(yob, region, n_dose, cov5y_nhs_c)]
nhs_2y[, birth_year := yob]
nhs_5y[, birth_year := yob]
nhs_2y[, yob := NULL]
nhs_5y[, yob := NULL]
nhs_2y[is.na(cov2y_nhs_c) & n_dose == 2, cov2y_nhs_c := 0]


cov_new <- merge(cov_new, nhs_2y, by = c("region", "n_dose", "birth_year"),
                 all.y = T)
cov_new <- merge(cov_new, nhs_5y, by = c("region", "n_dose", "birth_year"),
                 all.y = T)
cov_new <- unique(cov_new, by = c("region", "n_dose", "birth_year",
                                  "cov1y", "cov2y", "cov3y", "cov4y", "cov5y",
                                  "cov2y_nhs_c", "cov5y_nhs_c"))

cov_new[birth_year <2006, cov1y := NA]
cov_new[birth_year <2006, cov2y := NA]
cov_new[birth_year <2006, cov3y := NA]
cov_new[birth_year <2006, cov4y := NA]
cov_new[birth_year <2006, cov5y := NA]
cov_new[is.na(cov2y), cov2y := cov2y_nhs_c]
cov_new[is.na(cov5y), cov5y := cov5y_nhs_c]


#estimating the factor between in last available year
cov_new[birth_year == 2006, diff52_cprd := cov5y-cov2y]
cov_new[birth_year == 2017, cov5_ex:= cov5y]
cov_new[, diff52_cprd := max_NA(diff52_cprd), by = c("region", "n_dose")]
cov_new[, cov5_ex := max_NA(cov5_ex), by = c("region", "n_dose")]
cov_new[birth_year >= 2018, cov5y:= cov5_ex]

cov_new[, diff52_cov := cov5y-cov2y]
cov_new[birth_year == 2006 & n_dose == 1, f5to4_d1 := round((cov5y-cov4y)/diff52_cprd, digits =4)]
cov_new[birth_year == 2006 & n_dose == 1, f4to3_d1 := round((cov4y-cov3y)/diff52_cprd, digits = 4)]
cov_new[birth_year == 2006 & n_dose == 2, f5to4_d2 := round((cov5y-cov4y)/diff52_cprd, digits = 4)]
cov_new[birth_year == 2006 & n_dose == 2, f4to3_d2 := round((cov4y-cov3y)/diff52_cprd, digits = 4)]
cov_new[, f5to4_d1m := max_NA(f5to4_d1), by = c("region", "n_dose")]
cov_new[, f4to3_d1m := max_NA(f4to3_d1), by = c("region", "n_dose")]
cov_new[, f5to4_d2m := max_NA(f5to4_d2), by = c("region", "n_dose")]
cov_new[, f4to3_d2m := max_NA(f4to3_d2), by = c("region", "n_dose")]

#estimating the missing values born before 2006
cov_new[birth_year < 2006 & n_dose == 1, cov4y := cov5y-(f5to4_d1m*diff52_cov)]
cov_new[birth_year < 2006 & n_dose == 2, cov4y := cov5y-(f5to4_d2m*diff52_cov)]
cov_new[birth_year < 2006 & n_dose == 1, cov3y := cov4y-(f4to3_d1m*diff52_cov)]
cov_new[birth_year < 2006 & n_dose == 2, cov3y := cov4y-(f4to3_d2m*diff52_cov)]


#adding missing values for 2018-2020
cov_new[, diff52_cprd := NULL]
cov_new[, f5to4_d1 := NULL]
cov_new[, f4to3_d1 := NULL]
cov_new[, f5to4_d2 := NULL]
cov_new[, f4to3_d2 := NULL]
cov_new[birth_year == 2014, diff52_cprd := cov5y-cov2y]
cov_new[, diff52_cprd := max_NA(diff52_cprd), by = c("region", "n_dose")]
cov_new[birth_year == 2014 & n_dose == 1, f5to4_d1 := round((cov5y-cov4y)/diff52_cprd, digits =4)]
cov_new[birth_year == 2014 & n_dose == 1, f4to3_d1 := round((cov4y-cov3y)/diff52_cprd, digits = 4)]
cov_new[birth_year == 2014 & n_dose == 2, f5to4_d2 := round((cov5y-cov4y)/diff52_cprd, digits = 4)]
cov_new[birth_year == 2014 & n_dose == 2, f4to3_d2 := round((cov4y-cov3y)/diff52_cprd, digits = 4)]
cov_new[, f5to4_d1m := max_NA(f5to4_d1), by = c("region", "n_dose")]
cov_new[, f4to3_d1m := max_NA(f4to3_d1), by = c("region", "n_dose")]
cov_new[, f5to4_d2m := max_NA(f5to4_d2), by = c("region", "n_dose")]
cov_new[, f4to3_d2m := max_NA(f4to3_d2), by = c("region", "n_dose")]


#estimating the missing values after 2014
cov_new[birth_year > 2014 & n_dose == 1, cov4y := cov5y-(f5to4_d1m*diff52_cov)]
cov_new[birth_year > 2014 & n_dose == 2, cov4y := cov5y-(f5to4_d2m*diff52_cov)]
cov_new[birth_year > 2014 & n_dose == 1, cov3y := cov4y-(f4to3_d1m*diff52_cov)]
cov_new[birth_year > 2014 & n_dose == 2, cov3y := cov4y-(f4to3_d2m*diff52_cov)]

cov_new[n_dose == 1, cov1y := 0.75*cov2y]
cov_new[n_dose == 2, cov3y := 0.5*cov4y]
cov_new[, vaccine := "MMR"]


#####reshaping the data set
cov1 <- cov_new[, list(region, vaccine, n_dose, birth_year, cov1y)]
cov2 <- cov_new[, list(region, vaccine, n_dose, birth_year, cov2y)]
cov3 <- cov_new[, list(region, vaccine, n_dose, birth_year, cov3y)]
cov4 <- cov_new[, list(region, vaccine, n_dose, birth_year, cov4y)]
cov5 <- cov_new[, list(region, vaccine, n_dose, birth_year, cov5y)]

cov1[, year := birth_year +1]
cov2[, year := birth_year +2]
cov3[, year := birth_year +3]
cov4[, year := birth_year +4]
cov5[, year := birth_year +5]
cov1[, birth_year := NULL]
cov2[, birth_year := NULL]
cov3[, birth_year := NULL]
cov4[, birth_year := NULL]
cov5[, birth_year := NULL]


cov_extrapol <- merge(cov1, cov2, by = c("region", "vaccine", "n_dose", "year"), all = T)
cov_extrapol <- merge(cov_extrapol, cov3, by = c("region", "vaccine", "n_dose", "year"), all = T)
cov_extrapol <- merge(cov_extrapol, cov4, by = c("region", "vaccine", "n_dose", "year"), all = T)
cov_extrapol <- merge(cov_extrapol, cov5, by = c("region", "vaccine", "n_dose", "year"), all = T)

cov_extrapol <- cov_extrapol[year >= 2005 & year <=2019]
cov_extrapol[n_dose == 2 & year == 2019, cov1y := 0.75*cov2y]
cov_extrapol[n_dose == 2 & is.na(cov1y), cov1y := 0]

write.table(cov_extrapol, file =  paste0("Data/",
                                      "Coverage_reg_year_orig_extrapol.csv"),
            sep = ";", dec = ".", row.names = FALSE)



################################################################################
#extrapolating the NHS data
nhs_extra <- data.table::copy(nhs)
nhs_extra[n_dose == 2, cov2y_nhs := NA]
nhs_extra[yob <= 1997,cov2y_nhs:= NA]
nhs_extra[yob >= 2018, cov5y_nhs := NA]
nhs_extra[, birth_year := yob]

nhs_extra <- unique(nhs_extra, by = c("region", "n_dose", "birth_year"))
covnew <- unique(cov_new[, list(region, n_dose, birth_year, cov1y, cov2y,
                         cov3y, cov4y, cov5y)], by = c("region", "n_dose", "birth_year"))
#merging with the extrapolated data
nhs_extra <- merge(nhs_extra, covnew, by = c("birth_year", "n_dose", "region"), all.x = T)

nhs_extra[n_dose == 2,cov2y_nhs := 0]
nhs_extra[yob == 2017, cov5_ex:= cov5y_nhs]
nhs_extra[, cov5_ex := max_NA(cov5_ex), by = c("region", "n_dose")]
nhs_extra[yob >= 2018, cov5y_nhs:= cov5_ex]

nhs_extra[, diff52_cov := cov5y_nhs-cov2y_nhs]
nhs_extra[, diff52_cprd := cov5y-cov2y]
nhs_extra[, f5to4 := round((cov5y-cov4y)/diff52_cprd, digits =4)]
nhs_extra[, f4to3 := round((cov4y-cov3y)/diff52_cprd, digits = 4)]
nhs_extra[, cov4y_nhs := cov5y_nhs-(f5to4*diff52_cov)]
nhs_extra[, cov3y_nhs := cov4y_nhs-(f4to3*diff52_cov)]

nhs_extra[, test := cov2y_nhs <= cov3y_nhs &  cov3y_nhs<=  cov4y_nhs  &cov4y_nhs <=  cov5y_nhs ]
nhs_extra[n_dose == 1, cov1y_nhs := 0.75*cov2y]
nhs_extra[n_dose == 2 & is.na(cov1y_nhs) , cov1y_nhs := 0]
nhs_extra[n_dose == 2, cov3y_nhs := 0.5*cov4y_nhs]

#renaming
nhs_extra[, vaccine := "MMR"]
nhs_extra[, cov1y :=cov1y_nhs ]
nhs_extra[, cov2y :=cov2y_nhs ]
nhs_extra[, cov3y :=cov3y_nhs ]
nhs_extra[, cov4y :=cov4y_nhs ]
nhs_extra[, cov5y :=cov5y_nhs ]
#####reshaping the data set
cov1 <- nhs_extra[, list(region, vaccine, n_dose, birth_year, cov1y)]
cov2 <- nhs_extra[, list(region, vaccine, n_dose, birth_year, cov2y)]
cov3 <- nhs_extra[, list(region, vaccine, n_dose, birth_year, cov3y)]
cov4 <- nhs_extra[, list(region, vaccine, n_dose, birth_year, cov4y)]
cov5 <- nhs_extra[, list(region, vaccine, n_dose, birth_year, cov5y)]

cov1[, year := birth_year +1]
cov2[, year := birth_year +2]
cov3[, year := birth_year +3]
cov4[, year := birth_year +4]
cov5[, year := birth_year +5]
cov1[, birth_year := NULL]
cov2[, birth_year := NULL]
cov3[, birth_year := NULL]
cov4[, birth_year := NULL]
cov5[, birth_year := NULL]


nhs_extrapol <- merge(cov1, cov2, by = c("region", "vaccine", "n_dose", "year"), all = T)
nhs_extrapol <- merge(nhs_extrapol, cov3, by = c("region", "vaccine", "n_dose", "year"), all = T)
nhs_extrapol <- merge(nhs_extrapol, cov4, by = c("region", "vaccine", "n_dose", "year"), all = T)
nhs_extrapol <- merge(nhs_extrapol, cov5, by = c("region", "vaccine", "n_dose", "year"), all = T)

nhs_extrapol <- nhs_extrapol[year >= 2005 & year <=2019]
nhs_extrapol[n_dose == 2 & year == 2019, cov1y := 0.75*cov2y]
write.table(nhs_extrapol, file =  paste0("Data/",
                                      "Coverage_reg_year_nhs_extrapol.csv"),
            sep = ";", dec = ".", row.names = FALSE)



# ################################################################################
# #DEVELOPING THE SCENIARIOS
#scenario 1: 1215-730 = 485
#first dose will only plataeu at age 2

df <- read.csv2(paste0("Data/",
                       "Coverage_reg_year_orig_extrapol.csv"),
                            sep = ";", dec = ".")
df <- df[vaccine == "MMR"]
df <- df[n_dose == 2, age := age-485]


res <- data.table()

for(j in 1:length(all_regions)){
  
  df1 <- df[region == all_regions[j]]
  pop1 <- pop[region == all_regions[j]]  
  
  
  for(i in 1:length(all_years)){
    
    #age1 
    df_temp <- df1[bday1== all_years[i]]  
    pop_temp <- pop1[bday1== all_years[i]]  
    
    #age2
    
    df_temp <- df1[bday2== all_years[i]]  
    pop_temp <- pop1[bday2== all_years[i]]   
    
    df1[vaccine == "MMR" & n_dose == 1 & bday1== all_years[i],
        cov1y := 0]
    df1[vaccine == "MMR" & n_dose == 2 & bday1== all_years[i],
        cov1y := 0]
    
    df1[vaccine == "MMR" & n_dose == 1 & bday2== all_years[i], 
        cov2y := coverage_2y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday2== all_years[i], 
        cov2y := coverage_2y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
    
    #age 3
    df_temp <- df1[bday3== all_years[i]]  
    pop_temp <- pop1[bday3== all_years[i]]   
    df1[vaccine == "MMR" & n_dose == 1 & bday3== all_years[i], 
        cov3y := coverage_3y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday3== all_years[i], 
        cov3y := coverage_3y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
    
    #age 4
    df_temp <- df1[bday4== all_years[i]]  
    pop_temp <- pop1[bday4== all_years[i]]  
    df1[vaccine == "MMR" & n_dose == 1 & bday4== all_years[i], 
        cov4y := coverage_4y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday4== all_years[i], 
        cov4y := coverage_4y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
    
    
    #age 5
    df_temp <- df1[bday5== all_years[i]]  
    pop_temp <- pop1[bday5== all_years[i]]  
    
    df1[vaccine == "MMR" & n_dose == 1 & bday5== all_years[i], 
        cov5y := coverage_5y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
    df1[vaccine == "MMR" & n_dose == 2 & bday5== all_years[i], 
        cov5y := coverage_5y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
    
  }
  
  if(j ==1){
    
    tmp <- unique(df1[, list(yob, region, vaccine, n_dose,
                             cov1y, cov2y, cov3y, cov4y, cov5y,
                             bday1, bday2, bday3, bday4, bday5)])
    tmp <- tmp[!is.na(n_dose)]
    res <- tmp
  }
  else{
    tmp <- unique(df1[, list(yob, region, vaccine, n_dose,
                             cov1y, cov2y, cov3y, cov4y, cov5y,
                             bday1, bday2, bday3, bday4, bday5)])
    tmp <- tmp[!is.na(n_dose)]
    res <-rbind(res, tmp)
    
  }
}


cov1 <- res[, list(region, vaccine, n_dose, bday1, cov1y)]
cov1[, year := bday1]
cov1[, bday1 := NULL]
cov2 <- res[, list(region, vaccine, n_dose, bday2, cov2y)]
cov2[, year := bday2]
cov2[, bday2 := NULL]
cov3 <- res[, list(region, vaccine, n_dose, bday3, cov3y)]
cov3[, year := bday3]
cov3[, bday3 := NULL]
cov4 <- res[, list(region, vaccine, n_dose, bday4, cov4y)]
cov4[, year := bday4]
cov4[, bday4 := NULL]
cov5 <- res[, list(region, vaccine, n_dose, bday5, cov5y)]
cov5[, year := bday5]
cov5[, bday5 := NULL]

final_reg <- merge(cov1, cov2, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)
final_reg <- merge(final_reg, cov3, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)
final_reg <- merge(final_reg, cov4, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)
final_reg <- merge(final_reg, cov5, by = c("region", "vaccine", "n_dose", "year"), 
                   all = T)

write.table(final_reg, file =  paste0("Data/",
                                   "Coverage_slowearlysecond_raw.csv"),
            sep = ";", dec = ".", row.names = FALSE)



#extrapolating the missing data
cov_new2 <- melt(final_reg, id = c("year", "region", "vaccine", "n_dose"))

cov_new2[variable == "cov1y", birth_year := year-1]
cov_new2[variable == "cov2y", birth_year := year-2]
cov_new2[variable == "cov3y", birth_year := year-3]
cov_new2[variable == "cov4y", birth_year := year-4]
cov_new2[variable == "cov5y", birth_year := year-5]
cov_new2[variable == "cov1y", age := 1]
cov_new2[variable == "cov2y", age := 2]
cov_new2[variable == "cov3y", age := 3]
cov_new2[variable == "cov4y", age := 4]
cov_new2[variable == "cov5y", age := 5]
cov_new2[, coverage := value]

#reshaping
cov_new2[variable == "cov1y", cov1y := coverage]
cov_new2[variable == "cov2y", cov2y := coverage]
cov_new2[variable == "cov3y", cov3y := coverage]
cov_new2[variable == "cov4y", cov4y := coverage]
cov_new2[variable == "cov5y", cov5y := coverage]

cov_new2[,cov1y := max_NA(cov1y), by= c("region", "n_dose", "birth_year")]
cov_new2[,cov2y := max_NA(cov2y), by= c("region", "n_dose", "birth_year")]
cov_new2[,cov3y := max_NA(cov3y), by= c("region", "n_dose", "birth_year")]
cov_new2[,cov4y := max_NA(cov4y), by= c("region", "n_dose", "birth_year")]
cov_new2[,cov5y := max_NA(cov5y), by= c("region", "n_dose", "birth_year")]

cov_new2 <- cov_new2[, list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]
cov_new2 <- unique(cov_new2, by = c("region", "vaccine", "n_dose", "birth_year", 
                                  "cov1y", "cov2y", "cov3y", "cov4y", "cov5y"))

cov_new2 <- cov_new2[birth_year >=2006 & birth_year <= 2014]

colnames(cov_new2)
#add the first dose coverage before 2005
tmp1 <- cov_new[(n_dose == 1 & birth_year < 2006) | (n_dose == 1 & birth_year > 2014), list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]

#adding additional second dose coverage
tmp2 <- cov_new[(n_dose == 2 & birth_year <= 2006) | (n_dose == 2 & birth_year >= 2014), list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]
tmp2 <- merge(tmp2, cov_new2[(birth_year == 2006 | birth_year == 2014)& n_dose == 2], by = c("region", "birth_year"), all.x = T)
tmp2[birth_year== 2006, cor_2006_5 := cov5y.y/cov5y.x]
tmp2[birth_year== 2006, cor_2006_4 := cov4y.y/cov4y.x]
tmp2[birth_year== 2006, cor_2006_3 := cov3y.y/cov3y.x]
tmp2[birth_year== 2006, cor_2006_2 := cov2y.y/cov2y.x]
tmp2[birth_year== 2006, cov2_ex_2006:= cov2y.y]
tmp2[birth_year== 2014, cor_2014_5 := cov5y.y/cov5y.x]
tmp2[birth_year== 2014, cor_2014_4 := cov4y.y/cov4y.x]
tmp2[birth_year== 2014, cor_2014_3 := cov3y.y/cov3y.x]
tmp2[birth_year== 2014, cor_2014_2 := cov2y.y/cov2y.x]
tmp2[birth_year== 2014, cov2_ex_2014:= cov2y.y]
tmp2[,cor_2006_5 := max_NA(cor_2006_5), by= c("region")]
tmp2[,cor_2006_4 := max_NA(cor_2006_4), by= c("region")]
tmp2[,cor_2006_3 := max_NA(cor_2006_3), by= c("region")]
tmp2[,cor_2006_2 := max_NA(cor_2006_2), by= c("region")]
tmp2[,cor_2014_5 := max_NA(cor_2014_5), by= c("region")]
tmp2[,cor_2014_4 := max_NA(cor_2014_4), by= c("region")]
tmp2[,cor_2014_3 := max_NA(cor_2014_3), by= c("region")]
tmp2[,cor_2014_2 := max_NA(cor_2014_2), by= c("region")]
tmp2[,cov2_ex_2006 := max_NA(cov2_ex_2006), by= c("region")]
tmp2[,cov2_ex_2014 := max_NA(cov2_ex_2014), by= c("region")]

tmp2[birth_year <2006, cov5y := cov5y.x*cor_2006_5]
tmp2[birth_year <2006, cov4y := cov4y.x*cor_2006_4]
tmp2[birth_year <2006, cov3y := cov3y.x*cor_2006_3]
tmp2[birth_year <2006, cov2y := cov2_ex_2006]
tmp2[birth_year >2014, cov5y := cov5y.x*cor_2014_5]
tmp2[birth_year >2014, cov4y := cov4y.x*cor_2014_4]
tmp2[birth_year >2014, cov3y := cov3y.x*cor_2014_3]
tmp2[birth_year >2014, cov2y := cov2_ex_2014]

#sense check
any(tmp2$cov2y > tmp2$cov3)
tmp2[, n_dose := n_dose.x]
tmp2[, vaccine := vaccine.x]
tmp2[, cov1y := 0]
tmp2 <- tmp2[birth_year < 2006 | birth_year >2014, list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]

#join all the data sets together
cov_all <- rbind(cov_new2, tmp1, tmp2)
#adjusting for vaccines given at 12 months and 18 months
cov_all[n_dose == 2, cov1y := 0]
cov_all[n_dose == 2, cov2y := 0.75*cov3y]
cov_all[n_dose == 1, cov1y := 0.75*cov2y]

 
 #####reshaping the data set
 cov1 <- cov_all[, list(region,  n_dose, birth_year, cov1y)]
 cov2 <- cov_all[, list(region,  n_dose, birth_year, cov2y)]
 cov3 <- cov_all[, list(region, n_dose, birth_year, cov3y)]
 cov4 <- cov_all[, list(region,  n_dose, birth_year, cov4y)]
 cov5 <- cov_all[, list(region,  n_dose, birth_year, cov5y)]
 
 cov1[, year := birth_year +1]
 cov2[, year := birth_year +2]
 cov3[, year := birth_year +3]
 cov4[, year := birth_year +4]
 cov5[, year := birth_year +5]
 cov1[, birth_year := NULL]
 cov2[, birth_year := NULL]
 cov3[, birth_year := NULL]
 cov4[, birth_year := NULL]
 cov5[, birth_year := NULL]
 
 
 cov_extrapol2 <- merge(cov1, cov2, by = c("region", "n_dose", "year"), all = T)
 cov_extrapol2 <- merge(cov_extrapol2, cov3, by = c("region","n_dose", "year"), all = T)
 cov_extrapol2 <- merge(cov_extrapol2, cov4, by = c("region", "n_dose", "year"), all = T)
 cov_extrapol2 <- merge(cov_extrapol2, cov5, by = c("region", "n_dose", "year"), all = T)
 
 cov_extrapol2 <- cov_extrapol2[year >= 2005 & year <=2019]
 cov_extrapol2[n_dose == 2 & year == 2019, cov1y := 0]
 cov_extrapol2[, vaccine := "MMR"]
 
 write.table(cov_extrapol2, file =  paste0("Data/",
                                       "Coverage_earlysecond.csv"),
             sep = ";", dec = ".", row.names = FALSE)

 
################################################################################
#second dose at the age of two but with uptake speed of old second dose
 
 df <- read.csv2(paste0("Data/",
                        "Coverage_reg_year_orig_extrapol.csv"),
                 sep = ";", dec = ".")

 df <- df[vaccine == "MMR"]
 df <- df[n_dose == 2, age := age-977]
 
 
 res <- data.table()
 
 for(j in 1:length(all_regions)){
   
   df1 <- df[region == all_regions[j]]
   pop1 <- pop[region == all_regions[j]]  
   
   
   for(i in 1:length(all_years)){
     
     #age1 
     df_temp <- df1[bday1== all_years[i]]  
     pop_temp <- pop1[bday1== all_years[i]]  
     
     #age2
     
     df_temp <- df1[bday2== all_years[i]]  
     pop_temp <- pop1[bday2== all_years[i]]   
     
     df1[vaccine == "MMR" & n_dose == 1 & bday1== all_years[i],
         cov1y := 0]
     df1[vaccine == "MMR" & n_dose == 2 & bday1== all_years[i],
         cov1y := 0]
     
     df1[vaccine == "MMR" & n_dose == 1 & bday2== all_years[i], 
         cov2y := coverage_2y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
     df1[vaccine == "MMR" & n_dose == 2 & bday2== all_years[i], 
         cov2y := coverage_2y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
     
     #age 3
     df_temp <- df1[bday3== all_years[i]]  
     pop_temp <- pop1[bday3== all_years[i]]   
     df1[vaccine == "MMR" & n_dose == 1 & bday3== all_years[i], 
         cov3y := coverage_3y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
     df1[vaccine == "MMR" & n_dose == 2 & bday3== all_years[i], 
         cov3y := coverage_3y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
     
     #age 4
     df_temp <- df1[bday4== all_years[i]]  
     pop_temp <- pop1[bday4== all_years[i]]  
     df1[vaccine == "MMR" & n_dose == 1 & bday4== all_years[i], 
         cov4y := coverage_4y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
     df1[vaccine == "MMR" & n_dose == 2 & bday4== all_years[i], 
         cov4y := coverage_4y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
     
     
     #age 5
     df_temp <- df1[bday5== all_years[i]]  
     pop_temp <- pop1[bday5== all_years[i]]  
     
     df1[vaccine == "MMR" & n_dose == 1 & bday5== all_years[i], 
         cov5y := coverage_5y(vacc = "MMR", dose = 1, df = df_temp, df_p = pop_temp)]
     df1[vaccine == "MMR" & n_dose == 2 & bday5== all_years[i], 
         cov5y := coverage_5y(vacc = "MMR", dose = 2, df = df_temp, df_p = pop_temp)]
     
   }
   
   if(j ==1){
     
     tmp <- unique(df1[, list(yob, region, vaccine, n_dose,
                              cov1y, cov2y, cov3y, cov4y, cov5y,
                              bday1, bday2, bday3, bday4, bday5)])
     tmp <- tmp[!is.na(n_dose)]
     res <- tmp
   }
   else{
     tmp <- unique(df1[, list(yob, region, vaccine, n_dose,
                              cov1y, cov2y, cov3y, cov4y, cov5y,
                              bday1, bday2, bday3, bday4, bday5)])
     tmp <- tmp[!is.na(n_dose)]
     res <-rbind(res, tmp)
     
   }
 }
 
 
 cov1 <- res[, list(region, vaccine, n_dose, bday1, cov1y)]
 cov1[, year := bday1]
 cov1[, bday1 := NULL]
 cov2 <- res[, list(region, vaccine, n_dose, bday2, cov2y)]
 cov2[, year := bday2]
 cov2[, bday2 := NULL]
 cov3 <- res[, list(region, vaccine, n_dose, bday3, cov3y)]
 cov3[, year := bday3]
 cov3[, bday3 := NULL]
 cov4 <- res[, list(region, vaccine, n_dose, bday4, cov4y)]
 cov4[, year := bday4]
 cov4[, bday4 := NULL]
 cov5 <- res[, list(region, vaccine, n_dose, bday5, cov5y)]
 cov5[, year := bday5]
 cov5[, bday5 := NULL]
 
 final_reg <- merge(cov1, cov2, by = c("region", "vaccine", "n_dose", "year"), 
                    all = T)
 final_reg <- merge(final_reg, cov3, by = c("region", "vaccine", "n_dose", "year"), 
                    all = T)
 final_reg <- merge(final_reg, cov4, by = c("region", "vaccine", "n_dose", "year"), 
                    all = T)
 final_reg <- merge(final_reg, cov5, by = c("region", "vaccine", "n_dose", "year"), 
                    all = T)
 
 write.table(final_reg, file =  paste0("Data/",
                                       "Coverage_slowearlysecond_raw.csv"),
             sep = ";", dec = ".", row.names = FALSE)
 
 
 
 #extrapolating the missing data
 cov_new2 <- melt(final_reg, id = c("year", "region", "vaccine", "n_dose"))
 
 cov_new2[variable == "cov1y", birth_year := year-1]
 cov_new2[variable == "cov2y", birth_year := year-2]
 cov_new2[variable == "cov3y", birth_year := year-3]
 cov_new2[variable == "cov4y", birth_year := year-4]
 cov_new2[variable == "cov5y", birth_year := year-5]
 cov_new2[variable == "cov1y", age := 1]
 cov_new2[variable == "cov2y", age := 2]
 cov_new2[variable == "cov3y", age := 3]
 cov_new2[variable == "cov4y", age := 4]
 cov_new2[variable == "cov5y", age := 5]
 cov_new2[, coverage := value]
 
 #reshaping
 cov_new2[variable == "cov1y", cov1y := coverage]
 cov_new2[variable == "cov2y", cov2y := coverage]
 cov_new2[variable == "cov3y", cov3y := coverage]
 cov_new2[variable == "cov4y", cov4y := coverage]
 cov_new2[variable == "cov5y", cov5y := coverage]
 
 cov_new2[,cov1y := max_NA(cov1y), by= c("region", "n_dose", "birth_year")]
 cov_new2[,cov2y := max_NA(cov2y), by= c("region", "n_dose", "birth_year")]
 cov_new2[,cov3y := max_NA(cov3y), by= c("region", "n_dose", "birth_year")]
 cov_new2[,cov4y := max_NA(cov4y), by= c("region", "n_dose", "birth_year")]
 cov_new2[,cov5y := max_NA(cov5y), by= c("region", "n_dose", "birth_year")]
 
 cov_new2 <- cov_new2[, list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]
 cov_new2 <- unique(cov_new2, by = c("region", "vaccine", "n_dose", "birth_year", 
                                     "cov1y", "cov2y", "cov3y", "cov4y", "cov5y"))
 
 cov_new2 <- cov_new2[birth_year >=2006 & birth_year <= 2014]
 
 colnames(cov_new2)
 #add the first dose coverage before 2005
 tmp1 <- cov_new[(n_dose == 1 & birth_year < 2006) | (n_dose == 1 & birth_year > 2014), list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]
 
 #adding additional second dose coverage
 tmp2 <- cov_new[(n_dose == 2 & birth_year <= 2006) | (n_dose == 2 & birth_year >= 2014), list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]
 tmp2 <- merge(tmp2, cov_new2[(birth_year == 2006 | birth_year == 2014)& n_dose == 2], by = c("region", "birth_year"), all.x = T)
 tmp2[birth_year== 2006, cor_2006_5 := cov5y.y/cov5y.x]
 tmp2[birth_year== 2006, cor_2006_4 := cov4y.y/cov4y.x]
 tmp2[birth_year== 2006, cor_2006_3 := cov3y.y/cov3y.x]
 tmp2[birth_year== 2006, cor_2006_2 := cov2y.y/cov2y.x]
 tmp2[birth_year== 2006, cov2_ex_2006:= cov2y.y]
 tmp2[birth_year== 2014, cor_2014_5 := cov5y.y/cov5y.x]
 tmp2[birth_year== 2014, cor_2014_4 := cov4y.y/cov4y.x]
 tmp2[birth_year== 2014, cor_2014_3 := cov3y.y/cov3y.x]
 tmp2[birth_year== 2014, cor_2014_2 := cov2y.y/cov2y.x]
 tmp2[birth_year== 2014, cov2_ex_2014:= cov2y.y]
 tmp2[,cor_2006_5 := max_NA(cor_2006_5), by= c("region")]
 tmp2[,cor_2006_4 := max_NA(cor_2006_4), by= c("region")]
 tmp2[,cor_2006_3 := max_NA(cor_2006_3), by= c("region")]
 tmp2[,cor_2006_2 := max_NA(cor_2006_2), by= c("region")]
 tmp2[,cor_2014_5 := max_NA(cor_2014_5), by= c("region")]
 tmp2[,cor_2014_4 := max_NA(cor_2014_4), by= c("region")]
 tmp2[,cor_2014_3 := max_NA(cor_2014_3), by= c("region")]
 tmp2[,cor_2014_2 := max_NA(cor_2014_2), by= c("region")]
 tmp2[,cov2_ex_2006 := max_NA(cov2_ex_2006), by= c("region")]
 tmp2[,cov2_ex_2014 := max_NA(cov2_ex_2014), by= c("region")]
 
 tmp2[birth_year <2006, cov5y := cov5y.x*cor_2006_5]
 tmp2[birth_year <2006, cov4y := cov4y.x*cor_2006_4]
 tmp2[birth_year <2006, cov3y := cov3y.x*cor_2006_3]
 tmp2[birth_year <2006, cov2y := cov2_ex_2006]
 tmp2[birth_year >2014, cov5y := cov5y.x*cor_2014_5]
 tmp2[birth_year >2014, cov4y := cov4y.x*cor_2014_4]
 tmp2[birth_year >2014, cov3y := cov3y.x*cor_2014_3]
 tmp2[birth_year >2014, cov2y := cov2_ex_2014]
 
 #sense check
 any(tmp2$cov2y > tmp2$cov3)
 tmp2[, n_dose := n_dose.x]
 tmp2[, vaccine := vaccine.x]
 tmp2[, cov1y := 0]
 tmp2 <- tmp2[birth_year < 2006 | birth_year >2014, list(region, vaccine, n_dose, birth_year, cov1y, cov2y, cov3y, cov4y, cov5y)]
 
 #join all the data sets together
 cov_all <- rbind(cov_new2, tmp1, tmp2)
 #adjusting for vaccines given at 12 months and 18 months
 cov_all[n_dose == 2, cov1y := 0]
 cov_all[n_dose == 2, cov2y := 0.75*cov3y]
 cov_all[n_dose == 1, cov1y := 0.75*cov2y]
 
 
 #####reshaping the data set
 cov1 <- cov_all[, list(region,  n_dose, birth_year, cov1y)]
 cov2 <- cov_all[, list(region,  n_dose, birth_year, cov2y)]
 cov3 <- cov_all[, list(region, n_dose, birth_year, cov3y)]
 cov4 <- cov_all[, list(region,  n_dose, birth_year, cov4y)]
 cov5 <- cov_all[, list(region,  n_dose, birth_year, cov5y)]
 
 cov1[, year := birth_year +1]
 cov2[, year := birth_year +2]
 cov3[, year := birth_year +3]
 cov4[, year := birth_year +4]
 cov5[, year := birth_year +5]
 cov1[, birth_year := NULL]
 cov2[, birth_year := NULL]
 cov3[, birth_year := NULL]
 cov4[, birth_year := NULL]
 cov5[, birth_year := NULL]
 
 
 cov_extrapol2 <- merge(cov1, cov2, by = c("region", "n_dose", "year"), all = T)
 cov_extrapol2 <- merge(cov_extrapol2, cov3, by = c("region","n_dose", "year"), all = T)
 cov_extrapol2 <- merge(cov_extrapol2, cov4, by = c("region", "n_dose", "year"), all = T)
 cov_extrapol2 <- merge(cov_extrapol2, cov5, by = c("region", "n_dose", "year"), all = T)
 
 cov_extrapol2 <- cov_extrapol2[year >= 2005 & year <=2019]
 cov_extrapol2[n_dose == 2 & year == 2019, cov1y := 0]
 cov_extrapol2[, vaccine := "MMR"]
 
 write.table(cov_extrapol2, file =  paste0("Data/",
                                           "Coverage_earlysecond.csv"),
             sep = ";", dec = ".", row.names = FALSE)
 
 
 
 


##########################################################################
#adding scenario that coverage is increased by 0.5/1/2% per dose
tmp  <- data.table(read.csv2(file =  paste0("Data/",
                                                           "Coverage_reg_year_orig_extrapol.csv"),
                              sep = ";", dec = "."))

 max(tmp[n_dose ==2, cov5y])

#modifying just the coverge
improve_d1 <- function(df, add){
  
  data <- data.table::copy(df)
  data <- data[n_dose == 1, cov2y := cov2y+add]
  data <- data[n_dose == 1, cov3y := cov3y+add]
  data <- data[n_dose == 1, cov4y := cov4y+add]
  data <- data[n_dose == 1, cov5y := cov5y+add]
  return(data)
}



improve_d2 <- function(df, add){
  
  data <- data.table::copy(df)
  data <- data[n_dose == 2, cov3y := cov3y+add]
  data <- data[n_dose == 2, cov4y := cov4y+add]
  data <- data[n_dose == 2, cov5y := cov5y+add]
  return(data)
}


d1_025 <- improve_d1(tmp, add = 0.0025)
d1_05<- improve_d1(tmp, add = 0.005)
d1_1 <- improve_d1(tmp, add = 0.01)

d2_025 <- improve_d2(tmp, add = 0.0025)
d2_05<- improve_d2(tmp, add = 0.005)
d2_1 <- improve_d2(tmp, add = 0.01)
d2_3 <- improve_d2(tmp, add = 0.03)


write.table(d1_025, file =  "Data/d1_025.csv", sep = ";", dec = ".", row.names = FALSE) 
write.table(d1_05, file =  "Data/d1_05.csv", sep = ";", dec = ".", row.names = FALSE) 
write.table(d1_1, file =  "Data/d1_1.csv", sep = ";", dec = ".", row.names = FALSE) 
write.table(d2_025, file =  "Data/d2_025.csv", sep = ";", dec = ".", row.names = FALSE) 
write.table(d2_05, file =  "Data/d2_05.csv", sep = ";", dec = ".", row.names = FALSE) 
write.table(d2_1, file =  "Data/d2_1.csv", sep = ";", dec = ".", row.names = FALSE) 
write.table(d2_3, file =  "Data/d2_3.csv", sep = ";", dec = ".", row.names = FALSE) 

###############################################################################
#early second dose with improved uptake

tmp <- data.table(read.csv2(paste0("Data/",
                                "Coverage_earlysecond.csv"),
                   sep = ";", dec = "."))
max(tmp$cov5y)
max(tmp$cov4y)
max(tmp$cov3y)

improve_d2_early <- function(df, add){
  
  data <- data.table::copy(df)
  data <- data[n_dose == 2, cov2y := cov2y+add]
  data <- data[n_dose == 2, cov3y := cov3y+add]
  data <- data[n_dose == 2, cov4y := cov4y+add]
  data <- data[n_dose == 2, cov5y := cov5y+add]
  return(data)
}

early_plus025 <- improve_d2_early(tmp, 0.0025)
early_plus05 <- improve_d2_early(tmp, 0.005)
early_plus1 <- improve_d2_early(tmp, 0.01)

max(early_plus1$cov5y)
max(early_plus1$cov4y)
max(early_plus1$cov3y)

write.table(early_plus025, file =  paste0("Data/",
                                          "Coverage_earlyplus025.csv"),
            sep = ";", dec = ".", row.names = FALSE) 
write.table(early_plus05, file =  paste0("Data/",
                                          "Coverage_earlyplus05.csv"),
            sep = ";", dec = ".", row.names = FALSE)  
write.table(early_plus1, file =  paste0("Data/",
                                          "Coverage_earlyplus1.csv"),
            sep = ";", dec = ".", row.names = FALSE) 

#############################################################################
#early MMR2 uptake as MMR1
tmp <- data.table(read.csv2(paste0("Data/",
                                   "Coverage_earlysecond.csv"),
                            sep = ";", dec = "."))
tmp1 <- tmp[n_dose == 1]
tmp2 <- tmp[n_dose == 1]
tmp2[, cov5y:= cov4y]
tmp2[, cov4y:= cov3y]
tmp2[, cov3y:= cov2y]
tmp2[, cov2y:= cov1y]
tmp2[, cov1y:= 0]
tmp2[, n_dose := 2]

df <- rbind(tmp1, tmp2)
write.table(df, file =  paste0("Data/",
                                        "Coverage_MMR2likeMMR1.csv"),
            sep = ";", dec = ".", row.names = FALSE) 


#############################################################################
#MMR2 is given at school entry around age of 5
#assuming some delay coverage is 0 for ages 1-4, and 0.75% of full 5 year coverage
#at age 5 and then normal coverage at later ages

tmp  <- data.table(read.csv2(file =  paste0("Data/",
                                            "Coverage_reg_year_orig_extrapol.csv"),
                             sep = ";", dec = "."))

tmp[n_dose ==2, cov1y := 0]
tmp[n_dose ==2, cov2y := 0]
tmp[n_dose ==2, cov3y := 0]
tmp[n_dose ==2, cov4y := 0]
tmp[n_dose ==2, cov5y := 0.75*cov5y]

write.table(tmp, file =  paste0("Data/",
                               "MMR2_at_5.csv"),
            sep = ";", dec = ".", row.names = FALSE) 


##########################################################################
#Early second dose but with worse uptake

tmp  <- data.table(read.csv2(file =  paste0("Data/",
                                            "Coverage_earlysecond.csv")),
                             sep = ";", dec = ".")

tmp[, cov1y:= as.numeric(cov1y)]
tmp[, cov2y:= as.numeric(cov2y)]
tmp[, cov3y:= as.numeric(cov3y)]
tmp[, cov4y:= as.numeric(cov4y)]
tmp[, cov5y:= as.numeric(cov5y)]

worse_d2_early <- function(df, add){
  
  data <- data.table::copy(df)
  data <- data[n_dose == 2, cov2y := cov3y-add]
  data <- data[n_dose == 2, cov3y := cov3y-add]
  data <- data[n_dose == 2, cov4y := cov4y-add]
  data <- data[n_dose == 2, cov5y := cov5y-add]
  return(data)
}


eary_minus_0.5 <- worse_d2_early(tmp, 0.005)
eary_minus_1 <- worse_d2_early(tmp, 0.01)
eary_minus_3 <- worse_d2_early(tmp, 0.03)
eary_minus_5 <- worse_d2_early(tmp, 0.05)

write.table(eary_minus_0.5, file =  paste0("Data/",
                                          "Coverage_earlyminus05.csv"),
            sep = ";", dec = ".", row.names = FALSE) 
write.table(eary_minus_1, file =  paste0("Data/",
                                         "Coverage_earlyminus1.csv"),
            sep = ";", dec = ".", row.names = FALSE)  
write.table(eary_minus_3, file =  paste0("Data/",
                                        "Coverage_earlyminus3.csv"),
            sep = ";", dec = ".", row.names = FALSE) 
write.table(eary_minus_5, file =  paste0("Data/",
                                         "Coverage_earlyminus5.csv"),
            sep = ";", dec = ".", row.names = FALSE) 

##############################################################################
#add extreme experimental scenarios
tmp  <- data.table(read.csv2(file =  paste0("Data/",
                                            "Coverage_earlysecond.csv")),
                   sep = ";", dec = ".")

tmp[, cov1y:= as.numeric(cov1y)]
tmp[, cov2y:= as.numeric(cov2y)]
tmp[, cov3y:= as.numeric(cov3y)]
tmp[, cov4y:= as.numeric(cov4y)]
tmp[, cov5y:= as.numeric(cov5y)]

worse_d2_early <- function(df, add){
  
  data <- data.table::copy(df)
  data <- data[n_dose == 2, cov2y := cov3y-add]
  data <- data[n_dose == 2, cov3y := cov3y-add]
  data <- data[n_dose == 2, cov4y := cov4y-add]
  data <- data[n_dose == 2, cov5y := cov5y-add]
  return(data)
}


minus_10 <- worse_d2_early(tmp, 0.1)
minus_50 <- worse_d2_early(tmp, 0.5)
minus_all <- data.table::copy(tmp)
minus_all[n_dose == 2, cov1y:= 0]
minus_all[n_dose == 2, cov2y:= 0]
minus_all[n_dose == 2, cov3y:= 0]
minus_all[n_dose == 2, cov4y:= 0]
minus_all[n_dose == 2, cov5y:= 0]

write.table(minus_10, file =  paste0("Data/",
                                           "Cov2minus10.csv"),
            sep = ";", dec = ".", row.names = FALSE) 
write.table(minus_50, file =  paste0("Data/",
                                         "Cov2minus50.csv"),
            sep = ";", dec = ".", row.names = FALSE)  
write.table(minus_all, file =  paste0("Data/",
                                         "Cove2zero.csv"),
            sep = ";", dec = ".", row.names = FALSE) 