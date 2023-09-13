## Create a map using mapview 
## 03/06/2022
## Mellina Sidous


#load packages 
#install.packages("rgdal")
library(raster)
library(mapview)
library(ggplot2)
library(tidyverse)
library(sf)


#load data
hey<-st_read("map/coordinates_waterhole/final.shp")
 plot(hey)
 ggplot() +
   geom_sf(data = hey) 

########## Create a map using mapview 

# get blocks
blocks <- st_read("data/data_gis/blocks/Blocks.shp")

#get the border of Hwange
bord<-st_read("data/data_gis/borders/Border.shp")
mapview(bord, col.regions = "darkgreen")

# Get its referentiel of borders to then apply it to other shapefiles 
lok<-st_crs(bord) 
## Correction of referentiel
st_crs(blocks)<- lok
st_crs(hey)<- lok


###### Map of the blocks
mapview(blocks)
mapview(list(blocks, bord),
        layer.name = c("blocks", "border"))
mapview(blocks,legend = TRUE, col.regions = pal(10))

## the blocks studied : 
vf<- blocks %>%
  filter(ID == "Sinamatella" | ID == "Mtoa" | ID == "Robins"| ID == "Ngamo"| ID == "Main Camp" )

vf$blc<-c(1,2, 2, 4, 3)

bc("Dandari", "Shapi", "Shakwankie", "Josivanini", "Central")

blocks$blc <- c(1,1,3,1,3,3,3,3,2,2)
#with colors:
mapview(bord, legend = FALSE,alpha.regions = 0.3, aplha = 1.4, col.regions = "grey") +
  mapview(blocks, zcol = "blc", layer.name ="blocs") #+#, col.regions = c("purple", "yellow", "yellow", "blue", "green"))
# mapview(hey, cex = 4, layer.name ="points d'eau")

mapview(list(vf, bord),
        layer.name = c("vf", "border"))

#with colors:
mapview(bord, legend = FALSE,alpha.regions = 0.3, aplha = 1.4, col.regions = "grey") +
  mapview(vf, zcol = "blc", layer.name ="blocs") #+#, col.regions = c("purple", "yellow", "yellow", "blue", "green"))
# mapview(hey, cex = 4, layer.name ="points d'eau")


###### Map of waterholes
mapview(hey, cex = 1)

mapview(list(hey, bord),
        layer.name = c("vf", "border"))


###### Map of waterholes + blocks 
mapview(bord, alpha.regions = 0.3, col.regions = "grey", alpha = 2) + 
  mapview(hey, cex = 3, layer.name ="points d'eau", col.regions = "black")


######  FINAL PLOT
mapview(bord, legend = FALSE,alpha.regions = 0.3, aplha = 1.4, col.regions = "grey") +
  mapview(vf, zcol = "blc",  legend = FALSE) +#, col.regions = c("purple", "yellow", "yellow", "blue", "green"))
  mapview(hey, cex = 4, legend = FALSE)
#%
addLabelOnlyMarkers(data=data_foot,
                    label=~as.character(score), 
                    labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T,textsize="13px"))



##### Les cartes avec ggplot 


ggplot() +
  geom_sf(data = blocks) 

ggplot() +
  geom_sf(data = blocks, alpha = 0.3) +
  geom_sf(data = hey , aes(color = blockID))

ggplot() +
  #geom_sf(data = blocks, alpha = 0.3) +
  geom_sf(data = hey) 













############################################################################################################
############################## TRASH #######################################################################
############################################################################################################

ggplot() +
  geom_sf(data = blocks, alpha = 0.3) +
  geom_sf(data = datXY_try ) +
  geom_sf_label(data = datXY_try  , aes(label = waterpoint) ,
                label.size = 0.0000000001, 
                label.padding = unit(0.005, "lines"))

## try without one point : 

findout<- hey %>% subset( waterpoint == "scott's_pan" |  waterpoint == "back_pan" | waterpoint == "makololo" | waterpoint =="ngamo_2_major"  | waterpoint =="little_makololo")


findout<- hey %>% subset( waterpoint != "scott's_pan" &  waterpoint != "back_pan" & waterpoint != "makololo" & waterpoint !="ngamo_2_major"  & waterpoint !="little_makololo" & waterpoint !="makololo_madison" & waterpoint !="ngamo"&
                            waterpoint != "makololo_3")

findout<- findout %>% subset( waterpoint != "ngweshla"&  waterpoint != "masumamalisa" & waterpoint != "manga_3" &waterpoint != "jambile"& waterpoint != "somalisa_acacia"& waterpoint != "ostrich" & waterpoint != "samavundhla"& waterpoint != "ray's_pan"& waterpoint != "broken_rifle") 

ggplot() +
  geom_sf(data = blocks, alpha = 0.3) +
  geom_sf(data = findout ) +
  geom_sf_label(data = findout  , aes(label = waterpoint) ,
                label.size = 0.0000000001, 
                label.padding = unit(0.005, "lines"))




library(rworldmap)
library(rgeos)

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
centroid_df <- as.data.frame(centroids)


# affichage des premières lignes
head(centroid_df)

mapview(-69.98267 , 12.52089)
#-69.98267  12.52089

leaflet(-69.98267 , 12.52089) %>% 
  fitBounds(-20,65,20,40) 

library(leaflet)
leaflet() %>% 
  fitBounds(-20,65,20,40) %>% 
  addTiles() 




#capture.output(fr$ Count_site, file = "my_sites.out")

library(rworldmap)
library(rgeos)

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
centroid_df <- as.data.frame(centroids)


# affichage des premières lignes
head(centroid_df)

mapview(-69.98267 , 12.52089)
#-69.98267  12.52089

leaflet(-69.98267 , 12.52089) %>% 
  fitBounds(-20,65,20,40) 

library(leaflet)
leaflet() %>% 
  fitBounds(-20,65,20,40) %>% 
  addTiles() 






ID <- "Sinamatella_Dandari"
SURFACE <- blocksx$SURFACE[2] + blocksx$SURFACE[3]
geometry<- c(blocksx$geometry[2],blocksx$geometry[3])
geometry<-0
zazz<- data_frame(ID, SURFACE, geometry)
trys<-c("Sinamatella_Dandari", rep(0,6), blocks[c(2,3),8],c(blocks$geometry[2,3]))

blocksx %>% 
  filter(ID = "Sinamatella"|ID = "Dandari" )%>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

st_geometry(zazz)<-lb



is.character(lb)
as.character(zazz)

st(zazz)

zazz$lg<- list(1:31)
# S3 method for data.frame
st_as_sf(
  zazz,
  #agr = NA_agr_,
  lb,
  #wkt,
  #dim = "XYZ",
  #remove = TRUE,
  #na.fail = TRUE,
  sf_column_name = "gemetry"
)

st_drop_geometry(lb)

spdf = SpatialPolygonsDataFrame(lb,zazz)
st_geometry(lb)
lh<-st_set_geometry(zazz, lb)
st_geometry(zazz) <- lb
li<-st_union(blocksx$geometry[2],blocksx$geometry[3])

mapview(lb)

lb<-li[[1]]
typeof(lb)

CRS(blocks) <- CRS("+proj=UTM +zone=35 +datum=WGS84")

trys<-c("Sinamatella_Dandari", rep(0,6), blocks[c(2,3),8],c(blocks$geometry[2,3]))
