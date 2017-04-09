## Loading in the PUMA data to create PUMA level maps of CA
library(data.table)
library(ggplot2)
library(maptools)
library(mapproj)
library(stringr)

path <- "C:/Users/Owner/Documents/R/ACS/PUMAS/"

pumas <- readShapePoly(paste0(path,"cb_2015_06_puma10_500k.shp"), 
                       IDvar = "PUMACE10")

pumas.points <- data.table(fortify(pumas))

test <- ggplot(pumas.points, aes(x = long, y = lat, group = group)) +
    geom_polygon(color = "black", fill = "green", alpha = 0.9) +
    ggtitle(label = "Map of CA PUMAS 2010") +
    coord_fixed(1.2)

test

bayAreaPumas <- ggplot(pumas.points, 
                       aes(x = long, y = lat, group = group)) +
    geom_polygon(color = "red", fill = "cyan", alpha = 0.9) +
    coord_map(xlim = c(-123.5, -121.5), ylim = c(37, 39)) +
    ggtitle("SF Bay PUMAS")

bayAreaPumas

LAPumas <- ggplot(pumas.points, 
                  aes(x = long, y = lat, group = group)) +
    geom_polygon(color = "red", fill = "cyan", alpha = 0.9) +
    coord_map(xlim = c(-116.5, -119), ylim = c(33, 34.5)) +
    ggtitle("LA PUMAS")

LAPumas




# pumas.points[, num := ifelse(id == "27", "zero", "no")]
# 
# groupPumas <- ggplot(pumas.points,
#                       aes(x = long, y = lat, group = group)) +
#     geom_polygon(color = "black", aes(fill = num), alpha = 0.9) 
#     coord_map(xlim = c(-123, -120), ylim = c(36, 38))
# 
# groupPumas

#LA PUMAS pre 2012
# Source: http://answers.popdata.org/2000-PUMA-list-LA-county-q980822.aspx
LA.PUMA00 <- c(4500, 4600, 4700, 4800, 4900, 5000, 5100, 5200, 5300, 5401,
  5402, 5403, 5404, 5405, 5406, 5407, 5408, 5409, 5410, 5411,
  5412, 5413, 5414, 5415, 5416, 5417, 5418, 5419, 5420, 5421, 
  5422, 5423, 5424, 5500, 5600, 5701, 5702, 5703, 5800, 5900,
  6000, 6101, 6102, 6103, 6104, 6105, 6106, 6107, 6108, 6109,
  6110, 6111, 6112, 6113, 6114, 6115, 6116, 6117, 6118, 6119,
  6120, 6121, 6122, 6123, 6124, 6125, 6126)

LA.PUMA00 <- paste0("0", as.character(LA.PUMA00))

LA.PUMA10 <- c(3701, 3702, 3703, 3704, 3705, 3706, 3707, 3708, 3709,
               3710, 3711, 3712, 3713, 3714, 3715, 3716, 3717, 3718,
               3719, 3720, 3721, 3722, 3723, 3724, 3725, 3726, 3727, 
               3728, 3729, 3730, 3731, 3732, 3733, 3734, 3735, 3736,
               3737, 3738, 3739, 3740, 3741, 3742, 3743, 3744, 3745,
               3746, 3747, 3748, 3749, 3750, 3751, 3752, 3753, 3754,
               3755, 3756, 3757, 3758, 3759, 3760, 3761, 3762, 3763,
               3764, 3765, 3766, 3767, 3768, 3769)

LA.PUMA10 <- paste0("0", as.character(LA.PUMA10))

## Read in our PUMA to county crosswalks:
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
MCDC00 <- MCDC00[count == 1][, c("pop", "count") := NULL]
# Now we have PUMAs that are in multiple counties, we will 

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

sf_counties <- c("Sonoma", "Napa", "Solano", "Marin", "San Francisco",
                 "San Mateo", "Santa Cruz", "Santa Clara", "Alameda",
                 "Contra Costa")

la_counties <- c("Los Angeles", "Orange", "Riverside", "San Bernardino")