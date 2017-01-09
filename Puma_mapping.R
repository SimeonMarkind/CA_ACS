## Loading in the PUMA data to create PUMA level maps of CA
library(data.table)
library(ggplot2)
library(maptools)
library(mapproj)

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
