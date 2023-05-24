# MMR introduced in 1988, second dose introduced in 1996
# (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/383184/1516_No10_Measles_Mumps_and_Rubella__MMR__Immunisation_Programme_FINAL.pdf)

#### Vaccine c all doses at 2 and 5yo from 2011 ####

dir <- tempdir()
temp <- tempfile()
url <- "https://files.digital.nhs.uk/A5/2F555C/Child_Vacc_2021-22_CSV_Data.zip"
download.file(url,temp)
unzip(temp, exdir = dir)
dt_vacc_2011_2021 <- as.data.table(
  read.csv2(paste0(dir, "/ChildVaccStat_2021-22.csv"), sep = ","))
dt_vacc_2011_2021 <- dt_vacc_2011_2021[
  OrgType == "Region" & VacCode %in% c("MMR_24m", "MMR2_5y", "MMR1_5y"),]
dt_vacc_2011_2021[, Value := as.numeric(Value)]

dt_vacc_2011_2021 <- dt_vacc_2011_2021[, .(Year, OrgName, VacCode, Value)]
colnames(dt_vacc_2011_2021) <- c("year", "region", "vac_code", "coverage")
unlink(temp)
unlink(dir)

#### Vaccine coverage 1st dose at 2yo before 2010 ####

dir <- tempdir()
temp <- tempfile()
url <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20210107183608mp_/https://files.digital.nhs.uk/publicationimport/pub09xxx/pub09125/nhs-immu-stat-eng-2011-12-tab-csv.zip"
download.file(url,temp)
unzip(temp, exdir = dir)
dt_vacc_bef_2y <- as.data.table(read.csv2(paste0(dir, "/Table 4.csv"), sep = ","))
names_reg <- c("year", "", "England", "", "North East", "North West", 
               "Yorkshire and the Humber", "East Midlands", "West Midlands", 
               "East of England", "London", "South East Coast", "South Central",
               "South West")
dt_vacc_bef_2y <- as.data.frame(dt_vacc_bef_2y[seq(35, 46),])
dt_vacc_bef_2y[, 1] <- c("2001-02", "2002-03", "2003-04", "2004-05", "2005-06", " ",
                              "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", 
                              "2011-12")
colnames(dt_vacc_bef_2y) <- names_reg
dt_vacc_bef_2y <- dt_vacc_bef_2y[-6, -c(2,4)]
dt_vacc_bef_2y <- pivot_longer(dt_vacc_bef_2y, cols = colnames(dt_vacc_bef_2y)[-1],
                               names_to = "region", values_to = "coverage")
dt_vacc_bef_2y <- as.data.table(dt_vacc_bef_2y)
dt_vacc_bef_2y[, coverage := as.numeric(coverage)]

dt_vacc_bef_2y <- dt_vacc_bef_2y[region != "England",]

regions <- c(unique(dt_vacc_bef_2y$region), "South East", "East")

# Bits of math to find how to merge South East Coast South Central:
sep_cov <- rbind(dt_vacc_bef_2y[region %in% c("South East Coast", "South Central") & 
                                  year == "2010-11", coverage],
                 dt_vacc_bef_2y[region %in% c("South East Coast", "South Central") & 
                                  year == "2011-12", coverage])
south_east_cov <- dt_vacc_2011_2021[year %in% c("2010-11", "2011-12") & region == "South East" &
                                      vac_code == "MMR_24m", coverage]
coefs <- solve(sep_cov, south_east_cov)
cov_south_central <- dt_vacc_bef_2y[region == "South Central", coverage]
dt_vacc_bef_2y[region == "South East Coast", coverage := coverage * coefs[1] + cov_south_central * coefs[2]]
dt_vacc_bef_2y[region == "South East Coast", region := "South East"]
dt_vacc_bef_2y <- dt_vacc_bef_2y[region != "South Central",]
dt_vacc_bef_2y[, vac_code := "MMR_24m"]

dt_vacc_bef_2y <- dt_vacc_bef_2y[!year %in% c("2009-10", "2010-11", "2011-12"), .(year, region, vac_code, coverage)]

unlink(temp)
unlink(dir)

#### Vaccine coverage at 5yo, 2004 to 2010 ####

# Import year by year
years <- c("2004-05", "2005-06", "2006-07", "2007-08", "2008-09")
url <- c("https://files.digital.nhs.uk/publicationimport/pub00xxx/pub00176/nhs-immu-stat-eng-2004-2005-tabs.xls",
         "https://files.digital.nhs.uk/publicationimport/pub00xxx/pub00184/nhs-immu-stat-eng-2005-2006-tabs.xls",
         "https://files.digital.nhs.uk/publicationimport/pub00xxx/pub00197/nhs-immu-stat-2006-2007-tabs.xls",
         "https://files.digital.nhs.uk/publicationimport/pub00xxx/pub00204/nhs-immu-stat-eng-2007-2008-tabs.xls",
         "https://files.digital.nhs.uk/publicationimport/pub00xxx/pub00220/nhs-immu-stat-eng-2008-2009-tabs.xlsm")
region <- unique(dt_vacc_bef_2y)

for(i in seq_along(years)){
  if(i == 5) temp <- paste0(tempfile(), ".xlsm") else temp <- paste0(tempfile(), ".xls")
  url_i <- url[i]
  download.file(url_i, temp, mode = "wb")
  if(i == 1) sheet_nb <- 24 else if(i == 5) sheet_nb <- 9 else sheet_nb <- 8
  
  if(i == 5) test <- readxl::read_xlsx(temp, sheet = sheet_nb) else
    test <- readxl::read_xls(temp, sheet = sheet_nb)
  
  col_mmr <- which(colSums(test == "MMR", na.rm = T) > 0) + seq(0, 2, 1)
  col_mmr <- col_mmr[col_mmr <= ncol(test)]
  col_reg <- which(colSums((test == "North East") + (test == "NORTH EAST"), na.rm = T) > 0)
  test <- test[!is.na(test[, col_reg]), c(col_reg, col_mmr)]
  test <- as.data.frame(test)
  test <- test[, colSums(!is.na(test)) != 0]
  colnames(test) <- c("region", "MMR1_5y", "MMR2_5y")
  test$MMR1_5y <- as.numeric(test$MMR1_5y)
  test$MMR2_5y <- as.numeric(test$MMR2_5y)
  test$year <- years[i]
  
  test$region <- gsub(pattern = " \\([[:digit:]]\\)", replacement = "", x = test$region)
  test$region <- gsub(pattern = "\\([[:digit:]]\\)", replacement = "", x = test$region)
  
  test <- test[toupper(test$region) %in% toupper(regions),]
  
  long_i <- pivot_longer(test, cols = c("MMR1_5y", "MMR2_5y"), 
                         names_to = "vac_code", values_to = "coverage")
  if(i == 1) {
    dt_vacc_bef_5y <- long_i
  } else dt_vacc_bef_5y <- rbind.data.frame(dt_vacc_bef_5y, long_i)
  unlink(temp)
}

dt_vacc_bef_5y <- as.data.table(dt_vacc_bef_5y)
dt_vacc_bef_5y[region == "EAST", region := "East of England"]
dt_vacc_bef_5y$region %>% toupper %>% table

cov_south_central_5y <- dt_vacc_bef_5y[region == "South Central", coverage]
dt_vacc_bef_5y[region == "South East Coast", 
               coverage := coverage * coefs[1] + cov_south_central_5y * coefs[2]]
dt_vacc_bef_5y[region == "South East Coast", region := "South East"]
dt_vacc_bef_5y <- dt_vacc_bef_5y[region != "South Central",]

#### Merge dts ####

dt_vacc <- as.data.table(rbind.data.frame(dt_vacc_2011_2021, dt_vacc_bef_2y, dt_vacc_bef_5y))
dt_vacc[, region := toupper(region)]
dt_vacc[region == "YORKSHIRE & THE HUMBER", region := "YORKSHIRE AND THE HUMBER"]
regions <- unique(dt_vacc$region)
vac_code <- unique(dt_vacc$vac_code)

#### National coverage 2yo, 1999 to 2001 ####

# From https://files.digital.nhs.uk/publicationimport/pub00xxx/pub00176/nhs-immu-stat-eng-2004-2005-tabs.xls
country_cov <- as.data.table(expand.grid(year = c("1999-00", "2000-01", "2001-02",
                                                  "2002-03", "2003-04"), 
                                         region = regions, vac_code = vac_code))
country_cov[vac_code == "MMR_24m" & year == "1999-00", coverage := 87.6]
country_cov[vac_code == "MMR_24m" & year == "2000-01", coverage := 87.4]


#### National coverage 5yo, 1999 to 2004 ####

# From https://files.digital.nhs.uk/publicationimport/pub00xxx/pub00176/nhs-immu-stat-eng-2004-2005-tabs.xls
country_cov[vac_code == "MMR1_5y" & year == "1999-00", coverage := 93]
country_cov[vac_code == "MMR1_5y" & year == "2000-01", coverage := 92]
country_cov[vac_code == "MMR1_5y" & year == "2001-02", coverage := 91]
country_cov[vac_code == "MMR1_5y" & year == "2002-03", coverage := 90]
country_cov[vac_code == "MMR1_5y" & year == "2003-04", coverage := 90]
country_cov[vac_code == "MMR2_5y" & year == "1999-00", coverage := 76]
country_cov[vac_code == "MMR2_5y" & year == "2000-01", coverage := 75]
country_cov[vac_code == "MMR2_5y" & year == "2001-02", coverage := 74]
country_cov[vac_code == "MMR2_5y" & year == "2002-03", coverage := 75]
country_cov[vac_code == "MMR2_5y" & year == "2003-04", coverage := 75]
country_cov <- country_cov[!is.na(coverage), ]

dt_vacc <- as.data.table(rbind.data.frame(dt_vacc, country_cov))
dt_vacc <- dt_vacc[order(year),]

dt_vacc[vac_code == "MMR_24m", yob := as.numeric(substr(year, 1, 4)) - 1]
dt_vacc[is.na(yob), yob := as.numeric(substr(year, 1, 4)) - 4]
dt_vacc[vac_code == "MMR2_5y", dose := 2]
dt_vacc[is.na(dose), dose := 1]
dt_vacc[vac_code == "MMR_24m", age := 2]
dt_vacc[is.na(age), age := 5]
dt_vacc[, year := as.numeric(substr(year, 1, 4)) + 1]

dt_vacc$coverage <- dt_vacc$coverage / 100
