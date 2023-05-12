# change file path to folder containing all related files
#fp = '/Users/jiesun/Dropbox/PROJECT_JE/Generate_Cases/Generate/Cases_SHP'

# sf https://geocompr.github.io/post/2019/tmap-color-scales/
library('sf') # simple features
# install.packages('tmap')
library("tmap") # thematic map

# read data
region.shp = st_read(dsn=paste0(fp),   
              layer='Generate_Cases_SHP_Total_Cases_SHP') 

# plot map (takes about ~2min for this dataset)
m = tm_shape(region.shp) +
  tm_fill(col='Cases', # plot color by cases. to include borders, replace tm_fill with tm_polygons
          palette = "-viridis", contrast = c(0, 0.8), # see tmaptools::palette_explorer() for more
          style = "cont", # legend style (cont as opposed to binned values)
          n = 10) + # number of legend labels - this also controls the height of the legend
  tm_layout(frame=F,legend.outside = T)


tmap_save(m, 'Total_Cases_XGB.png', height = 10)
