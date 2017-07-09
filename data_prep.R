rm(list = ls())
library(data.table)
library(bit64)
library(lubridate)
library(plyr)
library(stringr)


## Creating a function to find standard deviations of weighted variables
weighted.sd <- function(vals, weight, na.rm = T){
    
    stopifnot(length(vals) == length(weight),
              is.numeric(vals) | is.list(vals),
              is.numeric(weight) | is.list(weight))
    
    if(na.rm){
        table <- data.frame(vals, weight)
        table <- na.omit(table)
        vals <- table$vals
        weight <- table$weight
    }
    
    sqrt(
        (1/sum(weight))*
        sum(
            weight*((vals - weighted.mean(vals, weight, na.rm))**2))
        
    )
}


cols.to.keep <- c(
    "SERIALNO", #Observation Serial Number
    "PUMA00", #2000 public use microdata area
    "PUMA10", #2010 public use microdata area
    "ADJINC", #Adjustment factor for income and earnings dollar amount
    "PWGTP", #Person's Weight
    "AGEP", #Age
    "CIT", #US citizenship status
    "COW", #Class of Worker
    "DDRS", #self-care difficulty
    "DEAR", #Hearing Difficulty 1 = yes, 0 = No
    "DEYE", #Vision Difficulty
    "DOUT", #Independent living difficulty
    "DPHY", #Ambulatory difficulty
    "DRATX", #veteran service connected disability
    "DREM", #cognitive difficulty
    "ENG", #Ability to speak english (1 = well, 4 = not able)
    "FER", #Gave birth to child in the last 12 months
    "INTP", #interest devidends and net rental income past 12 months
    "JWMNP", #travel time to work
    "LANX", #language other than English spoken at home
    "MAR", #Marital Status
    "MIG", #Mobility status
    "MIGSP05", #Migration recode for data prior to 2012
    "MIGSP12", #Migration recode for data after 2012
    "MIL", #military service
    "NWAB", #temporary absence from work
    "NWAV", #available or work
    "NWLA", #on layoff from work
    "NWLK", #looking for work
    "NWRE", #informed of recall
    "OIP", #All other income past 12 months
    "PAP", #public assistance income past 12 months
    "RETP", #retirement income past 12 months
    "SCH", #school enrollment
    "SCHL", #educational attainment
    "SEMP", #self-employment income past 12 months
    "SEX", #gender
    "SSP", #Social Security Income past 12 months
    "SSIP", #supplementary security income past 12 months
    "WAGP", #wages or salary past 12 months
    "WKHP", #usual hours worked per week past 12 months
    "WRK", #worked last week
    "DECADE", #Decade of entry to the USA
    "DIS", #disabilty recode
    "ESP", #employment status of parents
    "ESR", #employemnt status recode
    "FOD1P", #recoded field of degree
    "FOD2P", #recoded field of degree second entry
    "HICOV", #health insurance coverage recode
    "INDP", #industry recode based on 2012 IND codes
    "LANP05", #language spoken at home data collected prior to 2012
    "LANP12", #langauge spoken at home data collected in 2012 or later
    "MIGPUMA00", #migration PUMA based on census 2000 definition pre 2012
    "MIGPUMA10", #migration PUMA based on census 2010
    "NAICSP", #NAICS industry code based on 2012 codes
    "NATIVITY", #1 = Native, 2 = foreign born
    "NOP", #Nativity of parent
    "OCCP10", #occupation recode for 2010 and 2011 data based on 2010 OCC codes
    "OCCP12", #recode for 2012 and later data based on 2012 OCC codes
    "PAOC", #presence and age of own children
    "PERNP", #total person's earnings
    "PINCP", #total person's income
    "PRIVCOV", #private health insurance coverage recode
    "PUBCOV", #public heath coverage recode
    "RAC1P", #recoded detailed race code
    "SCIENGP", #field of degree science and engineering flag
    "SOCP10", #SOC occupation code for 2010 and 2011
    "SOCP12", #SOC occupation code for 2012 or later 
    "HISP" #Recoded detailed Hispanic origin
)

filter.data <- fread("C:/Users/Owner/Documents/R/PUMS/ss14pca.csv", 
                  verbose = F, select = cols.to.keep,
                  na.strings = c("NA", "", "N.A.", "N.A.//", 
                                 "-0009", NA),
                  colClasses = c("PUMA00" = "character",
                                 "PUMA10" = "character"))

filter.data[, year := substr(SERIALNO, 1,4)] #inserting a character vector
# corresponding to the year of the observation
#raw.data[, .N, by = .(year)] #count of observations per year

# year      N
# 1: 2010 360836
# 2: 2011 368809
# 3: 2012 368047
# 4: 2013 371403
# 5: 2014 372553
# Roughly symmetrc


## Have to adjust ADJINC and turn from integer to numeric
filter.data[, ADJINC := ADJINC/1000000.0]
## Turning integer columns to numeric so that we do not get
## integer overflow errors
cols <- c("INTP", "OIP", "PAP", "RETP",
           "SEMP", "SSIP", "SSP", "WAGP",
            "PERNP")

filter.data[, c("INTP", "OIP", "PAP", "RETP",
           "SEMP", "SSIP", "SSP", "WAGP",
            "PERNP") := lapply(.SD, as.numeric, na.omit = T),
            .SDcols = cols]
## Adjusting the necessary columns by ADJINC

filter.data[, `:=`(INTP = INTP*ADJINC,
                   OIP = OIP * ADJINC,
                   PAP = PAP * ADJINC,
                   RETP = RETP * ADJINC,
                   SEMP = SEMP * ADJINC,
                   SSIP = SSIP * ADJINC,
                   SSP = SSP * ADJINC,
                   WAGP = WAGP * ADJINC,
                   PERNP = PERNP * ADJINC,
                   PINCP = PINCP * ADJINC)]


## Filter down citizenship categories
## values of 1,2,3 = born a US citizen
## 4 = naturalized citizen
## 5 = not a citizen

filter.data[CIT %in% c(1,2,3), citizen := "Born"]
filter.data[CIT == 4, citizen := "Naturalized"]
filter.data[CIT == 5, citizen := "Non-citizen"]

## COW - Class of Worker
## 1,2,3,4,5 = employed -> "employed"
## 6,7 = self-employed -> "self-employed"
## 8 = working without pay in family business/farm -> "unemployed"
## 9 = Unemployed -> "Unemployed"
## NA -> "Unemployed"

filter.data[COW %in% c(1,2,3,4,5), empStat := "Employed"]
filter.data[COW %in% c(6,7), empStat := "Self-employed"]
filter.data[COW %in% c(8,9, NA), empStat := "Unemployed"]
 
## MAR - Marital Status
## 1 = Married -> "Married"
## 2, 3, 4, 5, = Unmarried -> "Unmarried"

filter.data[MAR == 1, marStat := "Married"]
filter.data[MAR %in% c(2,3,4,5), marStat := "Unmarried"]

## MIL - Military Service
## 1 -> "Active"
## 2,3 -> "Not-Active"
## NA, 4 -> "No Service"

filter.data[MIL == 1, milStat := "Active"]
filter.data[MIL %in% c(2,3), milStat := "Not-Active"]
filter.data[MIL %in% c(NA,4), milStat := "No Service"]

## SCHL - Educational Attainment
## NA, 1, 2,3 -> "None"
## 4,5,6,7,8 -> "Elementary"
## 9, 10, 11 -> "Middle"
## 12,13,14,15 -> "HS-no Diploma"
## 16, 17 -> "HS Diploma"
## 18, 19 -> "Some College"
## 20 -> "Associates"
## 21 -> "Bachelors"
## 22, 23, 24 -> "Graduate"

filter.data[is.na(SCHL) | SCHL %in% c(1,2,3), eduStat := "No HS Diploma"]
filter.data[SCHL %in% c(4,5,6,7,8), eduStat := "No HS Diploma"] #elementary
filter.data[SCHL %in% c(9,10,11), eduStat := "No HS Diploma"] #middle
filter.data[SCHL %in% c(12,13,14,15), eduStat := "No HS Diploma"]
filter.data[SCHL %in% c(16,17, 18, 19), eduStat := "HS Diploma"]
filter.data[SCHL == 20, eduStat := "Associates"]
filter.data[SCHL == 21, eduStat := "Bachelors"]
filter.data[SCHL %in% c(22,23,24), eduStat := "Post-Graduate"]

## MIGPUMA00 - Migration PUMA based on census 2000
## Note, this will not pick up people who moved within their PUMA
## NA -> "Same location"
## 1 -> "Not in US"
## >1 -> "Moved within US"
## -9 -> "NA"

filter.data[is.na(MIGPUMA00), mig00Stat := "Same Location"]
filter.data[MIGPUMA00 == 1, mig00Stat := "Not in US"]
filter.data[MIGPUMA00 > 1, mig00Stat := "Moved within US"]
filter.data[MIGPUMA00 == -9, mig00Stat := NA]

## MIGPUMA10 - Migration PUMA based on census 2010
## Same as MIGPUMA00

filter.data[is.na(MIGPUMA10), mig10Stat := "Same Location"]
filter.data[MIGPUMA10 == 1, mig10Stat := "Not in US"]
filter.data[MIGPUMA10 > 1, mig10Stat := "Moved within US"]
filter.data[MIGPUMA10 == -9, mig10Stat := NA]

filter.data[, year := as.Date(paste0(year, "-01-01"), 
                              format = "%Y-%m-%d")]

# for MIGS, NA means didn't move, 6 means moved within CA, -9 is NA
# a number greater than 6 is outside of CA. 
# a three digit number is outside of the USA
filter.data[, mobility := as.character(NA)]
filter.data[year <= as.Date("2011-01-01") & 
                (is.na(MIGSP05) | MIGSP05 == -9), 
            mobility := "Non-movers"]

filter.data[year <= as.Date("2011-01-01") & (MIGSP05 == 6),
              mobility := "In State"]
filter.data[year <= as.Date("2011-01-01") & MIGSP05 != 6 &
                  MIGSP05 < 100, mobility := "In US"]
filter.data[year <= as.Date("2011-01-01") & MIGSP05 >= 100,
              mobility := "Foreign"]

filter.data[year > as.Date("2011-01-01") & (is.na(MIGSP12) | MIGSP12 == -9), mobility := "Non-movers"]
filter.data[year > as.Date("2011-01-01") & (MIGSP12 == 6),
              mobility := "In State"]
filter.data[year > as.Date("2011-01-01") & MIGSP12 != 6 &
                  MIGSP05 < 100, mobility := "In US"]
filter.data[year > as.Date("2011-01-01") & MIGSP12 >= 100,
              mobility := "Foreign"]


## RAC1P - recoded race code
## 1 -> "White"
## 2 -> "African American"
## 3,4,5,7,8,9 -> "Other"
## 6 -> "Asian"

filter.data[RAC1P == 1, race := "White"]
filter.data[RAC1P == 2, race := "African American"]
filter.data[RAC1P %in% c(3,4,5,7,8,9), race := "Other"]
filter.data[RAC1P == 6, race := "Asian"]

# It seems that 2/3 of Hispanic people are calling themselves white
# filter.data[HISP != 1, .N, .(race)]
# race      N
# 1:            White 255909
# 2:            Other 158946
# 3:            Asian   2339
# 4: African American   3079

## Will also add a separate Hispanic flag variable
filter.data[, Hispanic := ifelse(HISP == 1, "Not Hispanic", "Hispanic")]

## Look at the OCC codes
## https://usa.ipums.org/usa/volii/acs_occtooccsoc.shtml
## We will make broad category columns but will retain
## the individual code if desired later

filter.data[!is.na(OCCP10), 
            Occupation10 := as.numeric(substr(OCCP10, 1, 2))]

## 0-5 are managers
filter.data[Occupation10 >= 0 & Occupation10 < 5,
            Occ := "Manager"]
## 5-8 are business operations
filter.data[Occupation10 >= 5 & Occupation10 < 8,
            Occ := "Business Operations"]
## 8-10 Financial Specialists
filter.data[Occupation10 >= 8 & Occupation10 < 10,
            Occ := "Financial Specialists"]
## 10-13 Computer/Math
filter.data[Occupation10 >= 10 & Occupation10 < 13,
            Occ := "Computer/Math"]
## 13-16 Architecture and Engineering
filter.data[Occupation10 >= 13 & Occupation10 < 16,
            Occ := "Architecture/Engineering"]
## 16-20 Life, Physical and Social Sciences
filter.data[Occupation10 >= 16 & Occupation10 < 20,
            Occ := "Physical/Social Science"]
## 20-21 Community and Social Services
filter.data[Occupation10 >= 20 & Occupation10 < 21,
            Occ := "Social Services"]
## 21-22 Legal
filter.data[Occupation10 >= 21 & Occupation10 < 22,
            Occ := "Legal"]
## 22-26 Education, Training, and Library Occupations
filter.data[Occupation10 >= 22 & Occupation10 < 26,
            Occ := "Education"]
## 26-30 Arts, Design, Entertainment, Sports, and Media Occupations
filter.data[Occupation10 >= 26 & Occupation10 < 30,
            Occ := "Arts/Entertainment/Sports"]
## 30 -36 Healthcare
filter.data[Occupation10 >= 30 & Occupation10 < 36,
            Occ := "Healthcare"]
## 36-37 Healthcare Support
filter.data[Occupation10 >= 36 & Occupation10 < 37,
            Occ := "Healthcare Support"]
## 37-40 Protective Services
filter.data[Occupation10 >= 37 & Occupation10 < 40,
            Occ := "Protectice Services"]
## 40-42 Food Preparation and Serving Occupations
filter.data[Occupation10 >= 40 & Occupation10 < 42,
            Occ := "Food Service"]
## 42-43 Building and Grounds Cleaning and Maintenance Operations
filter.data[Occupation10 >= 42 & Occupation10 < 43,
            Occ := "Maintenance"]
## 43-47 Personal Care and Service Occupations
filter.data[Occupation10 >= 43 & Occupation10 < 47,
            Occ := "Personal Care"]
## 47-50 Sales Occupations
filter.data[Occupation10 >= 47 & Occupation10 < 50,
            Occ := "Sales"]
## 50-60 Office and Administrative Support
filter.data[Occupation10 >= 50 & Occupation10 < 60,
            Occ := "Admin Support"]
## 60-62 Farming Fishing and Forestry
filter.data[Occupation10 >= 60 & Occupation10 < 62,
            Occ := "Farm/Fish/Forestry"]
## 62-68 Construction Trades
filter.data[Occupation10 >= 62 & Occupation10 < 68,
            Occ := "Construction"]
## 68-70 Extraction Workers
filter.data[Occupation10 >= 68 & Occupation10 < 70,
            Occ := "Extraction"]
## 70-77 Installation, Maintenance and Repair Workers
filter.data[Occupation10 >= 70 & Occupation10 < 77,
            Occ := "Repair"]
## 77-90 Production Occupations
filter.data[Occupation10 >= 77 & Occupation10 < 90,
            Occ := "Production"]
## 90-98 Transportation and Material Moving Occupations
filter.data[Occupation10 >= 90 & Occupation10 < 98,
            Occ := "Transportation"]
## 98-99 Military Specific
filter.data[Occupation10 >= 98 & Occupation10 < 99,
            Occ := "Military"]
## 99 Unemployed
filter.data[Occupation10 >= 99, 
            Occ := "Unemployed"]
## drop Occupation10 column
filter.data[,Occupation10 := NULL]

## Repeat above process for OCCP12
filter.data[!is.na(OCCP12), 
            Occupation12 := as.numeric(substr(OCCP12, 1, 2))]

## 0-5 are managers
filter.data[Occupation12 >= 0 & Occupation12 < 5,
            occ := "Manager"]
## 5-8 are business operations
filter.data[Occupation12 >= 5 & Occupation12 < 8,
            Occ := "Business Operations"]
## 8-10 Financial Specialists
filter.data[Occupation12 >= 8 & Occupation12 < 10,
            Occ := "Financial Specialists"]
## 10-13 Computer/Math
filter.data[Occupation12 >= 10 & Occupation12 < 13,
            Occ := "Computer/Math"]
## 13-16 Architecture and Engineering
filter.data[Occupation12 >= 13 & Occupation12 < 16,
            Occ := "Architecture/Engineering"]
## 16-20 Life, Physical and Social Sciences
filter.data[Occupation12 >= 16 & Occupation12 < 20,
            Occ := "Physical/Social Science"]
## 20-21 Community and Social Services
filter.data[Occupation12 >= 20 & Occupation12 < 21,
            Occ := "Social Services"]
## 21-22 Legal
filter.data[Occupation12 >= 21 & Occupation12 < 22,
            Occ := "Legal"]
## 22-26 Education, Training, and Library Occupations
filter.data[Occupation12 >= 22 & Occupation12 < 26,
            Occ := "Education"]
## 26-30 Arts, Design, Entertainment, Sports, and Media Occupations
filter.data[Occupation12 >= 26 & Occupation12 < 30,
            Occ := "Arts/Entertainment/Sports"]
## 30 -36 Healthcare
filter.data[Occupation12 >= 30 & Occupation12 < 36,
            Occ := "Healthcare"]
## 36-37 Healthcare Support
filter.data[Occupation12 >= 36 & Occupation12 < 37,
            Occ := "Healthcare Support"]
## 37-40 Protective Services
filter.data[Occupation12 >= 37 & Occupation12 < 40,
            Occ := "Protectice Services"]
## 40-42 Food Preparation and Serving Occupations
filter.data[Occupation12 >= 40 & Occupation12 < 42,
            Occ := "Food Service"]
## 42-43 Building and Grounds Cleaning and Maintenance Operations
filter.data[Occupation12 >= 42 & Occupation12 < 43,
            Occ := "Maintenance"]
## 43-47 Personal Care and Service Occupations
filter.data[Occupation12 >= 43 & Occupation12 < 47,
            Occ := "Personal Care"]
## 47-50 Sales Occupations
filter.data[Occupation12 >= 47 & Occupation12 < 50,
            Occ := "Sales"]
## 50-60 Office and Administrative Support
filter.data[Occupation12 >= 50 & Occupation12 < 60,
            Occ := "Admin Support"]
## 60-62 Farming Fishing and Forestry
filter.data[Occupation12 >= 60 & Occupation12 < 62,
            Occ := "Farm/Fish/Forestry"]
## 62-68 Construction Trades
filter.data[Occupation12 >= 62 & Occupation12 < 68,
            Occ := "Construction"]
## 68-70 Extraction Workers
filter.data[Occupation12 >= 68 & Occupation12 < 70,
            Occ := "Extraction"]
## 70-77 Installation, Maintenance and Repair Workers
filter.data[Occupation12 >= 70 & Occupation12 < 77,
            Occ := "Repair"]
## 77-90 Production Occupations
filter.data[Occupation12 >= 77 & Occupation12 < 90,
            Occ := "Production"]
## 90-98 Transportation and Material Moving Occupations
filter.data[Occupation12 >= 90 & Occupation12 < 98,
            Occ := "Transportation"]
## 98-99 Military Specific
filter.data[Occupation12 >= 98 & Occupation12 < 99,
            Occ := "Military"]
## 99 Unemployed
filter.data[Occupation12 >= 99, 
            Occ := "Unemployed"]
## drop Occupation12 column
filter.data[,Occupation12 := NULL]

## We will not play around with the SOC codes as
## those are almost identical to the OCC codes
filter.data[, Soc := ifelse(!is.na(SOCP10), SOCP10, SOCP12)]

filter.data[, sex := ifelse(SEX == 1, "Male", "Female")]


## Use the mapping created in puma_mapping.R
## Read in our PUMA to county crosswalks:
path <- "C:/Users/Owner/Documents/R/ACS/PUMAS/"

MCDC00 <- fread(paste0(path, "MCDC_crosswalk00.csv"))
MCDC00 <- MCDC00[, .(puma2k = ifelse(nchar(puma2k) == 3,
                                     paste0("00", as.character(puma2k)),
                                     ifelse(nchar(puma2k) == 4,
                                            paste0("0", as.character(puma2k)),
                                            as.character(puma2k))),
                     county_name = str_sub(county_name, 1, nchar(county_name)-3),
                     Total_Pop_2010_census)][, .(pop = sum(Total_Pop_2010_census)),
                                             .(puma2k, county_name)][order(puma2k,
                                                                           -pop)]
MCDC00[, count := rowid(puma2k)]
# Now we have PUMAs that are in multiple counties, we will 
# assign each puma the county name with the largest population
# in that PUMA
MCDC00 <- MCDC00[count == 1][, c("pop", "count") := NULL]


MCDC10 <- fread(paste0(path, "MCDC_crosswalk10.csv"))
MCDC10 <- MCDC10[, .(puma12 = ifelse(nchar(puma12) == 3,
                                     paste0("00", as.character(puma12)),
                                     ifelse(nchar(puma12) == 4,
                                            paste0("0", 
                                                   as.character(puma12)),
                                            as.character(puma12))),
                     county_name = str_sub(county_name, 1, 
                                           nchar(county_name)-3))] %>% 
    unique()

# Now we add the county name to our filter data table
filter.data[!is.na(PUMA00), county := MCDC00[match(PUMA00, 
                                                   MCDC00$puma2k), 
                                             county_name]]

filter.data[!is.na(PUMA10), county := MCDC10[match(PUMA10,
                                                   MCDC10$puma12),
                                               county_name]]

## Take a file we will use for mapping
map_data <- filter.data[AGEP >= 18 & !is.na(PUMA10), .(sex, PUMA10, AGEP, race, PWGTP, 
                            eduStat, year, Occ, PINCP, WKHP)]

map_data[, nage := round_any(AGEP, 5)]
map_data[, white := ifelse(race == "White", "White", "Non-white")]

write.csv(map_data,"C:/Users/Owner/Documents/R/PUMS/map_data.csv",
          row.names = F)

## Filter out anyone below 18 and above 75, place into 5 year buckets

filter.data <- filter.data[AGEP >= 18 & AGEP <= 65,]

ageLabels <- c("18-20", "21-25", "26-30", "31-35", "36-40", "41-45",
               "46-50", "51-55", "56-60", "61-65")

ageBreaks <- c(18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)

filter.data[, age := cut(AGEP, breaks = ageBreaks, 
                         labels = ageLabels,
                         include.lowest = TRUE)]


## Search for outliers in the PINCP category
## For now, let's check what happens if we keep within 3 devs of the
## mean for the log of PINCP by sex, race, education level,
## age, hours worked, and Occ

## NOT DOING THIS NOW, MAY REVISIT
# filter.data[, `:=`(nage = round_any(AGEP, 5),
#                    hour_bucket = round_any(WKHP, 5))]
# 
# test <- filter.data[!is.na(PINCP) & PINCP > 1000,
#                     .(nage, hour_bucket, Occ, sex, race, eduStat, PWGTP, 
#                       mean = log(PINCP, 10))][, 
#                                               .(sd = weighted.sd(mean, PWGTP, na.rm = T)),
#                                               .(nage, hour_bucket, Occ, sex, race, eduStat)]


write.csv(filter.data,"C:/Users/Owner/Documents/R/PUMS/filterData.csv",
          row.names = F)

# save.image(filter.data, map_data, 
#            file = "C:/Users/Owner/Documents/R/PUMS/envir.RData")
