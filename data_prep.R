library(data.table)
library(bit64)

raw.data <- fread("C:/Users/Owner/Documents/R/PUMS/ss14pca.csv", 
                  verbose = T)

raw.data[, year := substr(SERIALNO, 1,4)] #inserting a character vector
# corresponding to the year of the observation
#raw.data[, .N, by = .(year)] #count of observations per year

# year      N
# 1: 2010 360836
# 2: 2011 368809
# 3: 2012 368047
# 4: 2013 371403
# 5: 2014 372553
# Roughly symmetrc

cols.to.keep <- c(
    "year", #self calculated variable for the year of the observation
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
    "ENG", #Ability to speak english
    "FER", #Gave birth to child in the last 12 months
    "INTP", #interest devidends and net rental income past 12 months
    "JWMNP", #travel time to work
    "LANX", #language other than English spoken at home
    "MAR", #Marital Status
    "MIG", #Mobility status
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
    "SOCP12" #SOC occupation code for 2012 or later 
)
    
filter.data <- raw.data[, cols.to.keep, with = FALSE]

rm(raw.data)
## Have to adjust ADJINC and turn from integer to numeric
filter.data[, ADJINC := ADJINC/1000000.0]
## Turning integer columns to numeric so that we do not get
## integer overflow errors
filter.data[, lapply(.SD, as.numeric, na.omit = T),
            .SDcols = c("INTP", "OIP", "PAP", "RETP",
                        "SEMP", "SSIP", "SSP", "WAGP",
                        "PERNP")]
## Adjusting the necessary columns by ADJINC

filter.data[, `:=`(INTP = INTP*ADJINC,
                   OIP = OIP * ADJINC,
                   PAP = PAP * ADJINC,
                   RETP = RETP * ADJINC,
                   SEMP = SEMP * ADJINC,
                   SSIP = SSIP * ADJINC,
                   SSP = SSP * ADJINC,
                   WAGP = WAGP * ADJINC,
                   PERNP = PERNP * ADJINC)]

## Filter out anyone below 18 and above 75, place into 5 year buckets

filter.data <- filter.data[AGEP >= 18 & AGEP <= 75,]
                 
ageLabels <- c("18-20", "21-25", "26-30", "31-35", "36-40", "41-45",
               "46-50", "51-55", "56-60", "61-65", "66-70", "71-75")
  
ageBreaks <- c(18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75)

filter.data[, age := cut(AGEP, breaks = ageBreaks, 
                         labels = ageLabels,
                         include.lowest = TRUE)]

## Filter down citizenship categories
## values of 1,2,3 = born a US citizen
## 4 = naturalized citizen
## 5 = not a citizen

filter.data[CIT %in% c(1,2,3), citizen := "Born"]
filter.data[CIT == 4, citizen := "Naturalzied"]
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

filter.data[SCHL %in% c(NA, 1,2,3), eduStat := "None"]
filter.data[SCHL %in% c(4,5,6,7,8), eduStat := "Elementary"]
filter.data[SCHL %in% c(9,10,11), eduStat := "Middle"]
filter.data[SCHL %in% c(12,13,14,15), eduStat := "HS-no Diploma"]
filter.data[SCHL %in% c(16,17), eduStat := "HS Diploma"]
filter.data[SCHL == 20, eduStat := "Associates"]
filter.data[SCHL == 21, eduStat := "Bachelors"]
filter.data[SCHL %in% c(22,23,24), eduStat := "Graduate"]

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

## RAC1P - recoded race code
## 1 -> "White"
## 2 -> "African American"
## 3,4,5,7,8,9 -> "Other"
## 6 -> "Asian"

filter.data[RAC1P == 1, race := "White"]
filter.data[RAC1P == 2, race := "African American"]
filter.data[RAC1P %in% c(3,4,5,7,8,9), race := "Other"]
filter.data[RAC1P == 6, race := "Asian"]

