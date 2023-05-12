library(sf)
library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(pals)

setwd("C:/Users/shreya/Documents/nBox/JE Jie/")

data2 <- as.data.frame(readRDS("Generate/Total_Cases_Pixel/Cases_TotalXGB.Rds"))
head(data2)
data2$Total[data2$Total < 0 ] = 0
points <- data2[,1:2]
coordinates(points) <- ~x+y
mycrs <- CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
proj4string(points) <- mycrs

points.df <- SpatialPointsDataFrame(points, data.frame(data2$Total))


# region.shp <- readOGR('C:/Users/shreya/Documents/JE Jie/Generate Cases/Data/Ende_map_feed.shp')
# region <- spTransform(region.shp, mycrs)
# region.shp


colnames(points.df@data) <- c("Total_Cases")

jpeg("XGB.jpg", height = 100)

brks <- c(0,0.001,0.0015,0.002,0.005,0.0075,0.01,0.015,0.02,0.05,0.07,0.09,0.1,0.15,0.3,0.5,0.7,1,3,10,
          20,40,50,70,80)
col.palette <- brewer.pal(n = 24, name = "BuGn")

plot(rep(1,24),col=colfunc(24),pch=19,cex=3)
plot1 <- spplot(points.df, "Total_Cases",
       cuts = brks, key.space = "right", cex = 0.3, col.regions = colfunc(24))

plot1$legend$right$args$key$points$cex <- c(1,1,1,1)

print(plot1)

dev.off()
