# sf https://geocompr.github.io/post/2019/tmap-color-scales/
library('sf') # simple features
# install.packages('tmap')
library("tmap") # thematic map

setwd("C:/Users/shreya/Documents/nBox/JE Jie/")

library(dichromat)

colfunc <- colorRampPalette(c("#ee9b00","#55a630","#0a9396","#005f73"))
plot(rep(1,17),col=colfunc(17),pch=19,cex=5)


XGB <- read_sf(dsn = "C:/Users/shreya/Documents/nBox/JE Jie/Generate/Data/", layer = "Generate_Cases_SHP_Total_Cases_SHP")

XGB$Country[XGB$Country == "Low.NPL"] = "NPL"
XGB$Cases[XGB$Country == "NPL"] = XGB$Cases[XGB$Country == "NPL"] + XGB$Cases[XGB$Country == "High.NPL"]
XGB = XGB[!XGB$Country == "High.NPL",]
XGB = XGB[!XGB$Country == "MAC",]
XGB$Cases[XGB$Country == "CHN"] = XGB$Cases[XGB$Country == "CHN"] + XGB$Cases[XGB$Country == "HKG"]
XGB = XGB[!XGB$Country == "HKG",]

RF <-  read_sf(dsn = "C:/Users/shreya/Documents/nBox/JE Jie/Generate/Data/", layer = "Generate_Cases_SHP_RF_Total_Cases_SHP")
RF$Country[RF$Country == "Low.NPL"] = "NPL"
RF$Cases[RF$Country == "NPL"] = RF$Cases[RF$Country == "NPL"] + RF$Cases[RF$Country == "High.NPL"]
RF = RF[!RF$Country == "High.NPL",]
RF = RF[!RF$Country == "MAC",]
RF$Cases[RF$Country == "CHN"] = RF$Cases[RF$Country == "CHN"] + RF$Cases[RF$Country == "HKG"]
RF = RF[!RF$Country == "HKG",]
RF$Cases[RF$Cases<0] = 0

GB <-  read_sf(dsn = "C:/Users/shreya/Documents/nBox/JE Jie/Generate/Data/", layer = "Generate_Cases_SHP_GB_Total_Cases_SHP")
GB$Country[GB$Country == "Low.NPL"] = "NPL"
GB$Cases[GB$Country == "NPL"] = GB$Cases[GB$Country == "NPL"] + GB$Cases[GB$Country == "High.NPL"]
GB = GB[!GB$Country == "High.NPL",]
GB = GB[!GB$Country == "MAC",]
GB$Cases[GB$Country == "CHN"] = GB$Cases[GB$Country == "CHN"] + GB$Cases[GB$Country == "HKG"]
GB = GB[!GB$Country == "HKG",]
GB$Cases[GB$Cases<0] = 0

WHO_IG = read_sf(dsn = "C:/Users/shreya/Documents/nBox/JE Jie/Generate/Data/", layer = "Generate_Cases_SHP_Total_Cases_SHP")
WHO_IG$Country[WHO_IG$Country == "Low.NPL"] = "NPL"
WHO_IG$Cases[WHO_IG$Country == "NPL"] = WHO_IG$Cases[WHO_IG$Country == "NPL"] + WHO_IG$Cases[WHO_IG$Country == "High.NPL"]
WHO_IG = WHO_IG[!WHO_IG$Country == "High.NPL",]
WHO_IG = WHO_IG[!WHO_IG$Country == "MAC",]
WHO_IG$Cases[WHO_IG$Country == "CHN"] = WHO_IG$Cases[WHO_IG$Country == "CHN"] + WHO_IG$Cases[WHO_IG$Country == "HKG"]
WHO_IG = WHO_IG[!WHO_IG$Country == "HKG",]
Combined <- read.csv("C:/Users/shreya/Documents/nBox/JE Jie/Generate/Data/Comparision_with_WHO_estimates.csv")
WHO_cases <- Combined[Combined$Method == "WHO-IG",c(2,4)]
for (i in WHO_cases$Country){
  WHO_IG$Cases[WHO_IG$Country == i] = WHO_cases$Cases[WHO_cases$Country == i]
}


library(grid)
brks <- seq(0,80000,5000)
#A: WHO-IG
who_m <- tm_shape(WHO_IG) +
  tm_fill(col='Cases', # plot color by cases. to include borders, replace tm_fill with tm_polygons
          palette = "-viridis", contrast = c(0, 0.8), # see tmaptools::palette_explorer() for more
          style = "cont", # legend style (cont as opposed to binned values)
          breaks = brks) + # number of legend labels - this also controls the height of the legend
  tm_layout(frame=F,legend.show = F, title = "A",title.position = c("right","TOP"))

#B: XGB

xgb_m <- tm_shape(XGB) +
  tm_fill(col='Cases', # plot color by cases. to include borders, replace tm_fill with tm_polygons
          palette = "-viridis", contrast = c(0, 0.8), # see tmaptools::palette_explorer() for more
          style = "cont", # legend style (cont as opposed to binned values)
          breaks = brks) + # number of legend labels - this also controls the height of the legend
  tm_layout(frame=F,legend.show = F, title = "B",title.position = c("right","TOP"))

#C: GB

gb_m <- tm_shape(GB) +
  tm_fill(col='Cases', # plot color by cases. to include borders, replace tm_fill with tm_polygons
          palette ="-viridis", contrast = c(0, 0.8), # see tmaptools::palette_explorer() for more
          style = "cont", # legend style (cont as opposed to binned values)
          breaks = brks) + # number of legend labels - this also controls the height of the legend
  tm_layout(frame=F,legend.show = F, title = "1",title.position = c("right","TOP"))


#D: RF

rf_m <- tm_shape(RF) +
  tm_fill(col='Cases', # plot color by cases. to include borders, replace tm_fill with tm_polygons
            palette = "-viridis", contrast = c(0, 0.8), # see tmaptools::palette_explorer() for more
          style = "cont", # legend style (cont as opposed to binned values)
          breaks = brks) + # number of legend labels - this also controls the height of the legend
  tm_layout(frame=F,legend.show = F, title = "2",title.position = c("right","TOP"))


legend.map <- tm_shape(GB) + 
  tm_fill(col = "Cases", style = 'cont', palette = "-viridis", contrast = c(0, 0.8), breaks = brks) +
  tm_layout(legend.only = TRUE)

grid.newpage()
page.layout <- grid.layout(nrow = 2, ncol = 3, widths=c(.4,.4,.1), heights = c(.5,.5))
pushViewport(viewport(layout = page.layout))

# print(who_m, vp=viewport(layout.pos.row = 1:2, layout.pos.col = 1))
# print(xgb_m, vp=viewport(layout.pos.row = 1:2, layout.pos.col = 2))
print(gb_m, vp=viewport(layout.pos.row = 1:2, layout.pos.col = 1))
print(rf_m, vp=viewport(layout.pos.row = 1:2, layout.pos.col = 2))
print(legend.map, vp=viewport(layout.pos.row = 1:2, layout.pos.col = 3))

print(xgb_m)
