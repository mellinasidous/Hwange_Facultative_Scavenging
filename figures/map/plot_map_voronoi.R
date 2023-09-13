######################### create final map

#'load packages 
#-----------------------------
library(raster)
library(mapview)
library(ggplot2)
library(tidyverse)
library(sf)
library(tidyverse)
library(RColorBrewer) 
#display.brewer.all()
library(maps)
library(maptools)
library(spatstat)
library(rgeos)
library(sf)
library(spatstat.utils)
library(ggspatial)
library(ggpubr)
library(ggpubr)
#'
#'
#'
#'load data
#-----------------------------
#'waterhole coordinates
hey<-st_read("figures/map/map/coordinates_waterhole/final.shp")
#'blocks coordinates
blocks <- st_read("figures/map/data_gis/blocks/Blocks.shp")
#'border
bord<-st_read("figures/map/data_gis/borders/Border.shp")
#'
#'
#' Create a map using mapview 
#-----------------------------
mapview(bord, col.regions = "darkgreen")
#'
#'Get the referential of borders to  apply it to other shapefiles 
#-----------------------------
lok<-st_crs(bord) 
#' Correct referential
st_crs(blocks)<- lok
st_crs(hey)<- lok
#'
#'
#'Now we want to create a map with only our two blocks colored
#-----------------------------
Main_camp<- blocks %>%
  filter( ID == "Ngamo"| ID == "Main Camp" )
#'plot
mapview(bord, legend = FALSE,alpha.regions = 0.3, aplha = 1.4, col.regions = "grey") +
mapview(Main_camp,  layer.name ="Main camp area", col.regions = "orange")+
mapview(hey, cex = 0.3, layer.name ="waterholes", col.regions = "black")
#'
#'
###############################################################################
##################### NOW LET'S GET VORONOI DIAGRAM ###########################
###############################################################################
#'
#'
#' code that pool all the waterhole according to nolwenn's phd
#------------------------------------------------
source(file="figures/map/voronoi/poolwt_to_create_voronoi.R", encoding ="UTF-8")
#'
#'
#' now select a file with only centroids of the waterpoint that are supposed to be in the same cells of the tesselation
#------------------------------------------------
to_tess <- st_drop_geometry(to_tess) %>% dplyr::select(tess_name,meanx, meany) %>% group_by(tess_name)%>% slice(1) %>% ungroup()
#'
#'
#'### Perform tesselation
#------------------------------------------------
W<-as.owin(bord)## transform bord into owin class to have tesselation limited to the borders 
### points to do the voronoi diagram 
X <- ppp(x=to_tess$meanx,
         y=to_tess$meany, window = W)
y <- dirichlet(X) # Dirichlet tesselation
#'
#'
#' PLOT TO VISUALIZE
#-------------------------------------------------
plot(y )#+# Plot tesselation
plot(X, add = TRUE) #add coordinates used for the voronoi diagram
text(to_tess$meanx, to_tess$meany+2000, labels=to_tess$tess_name, cex = 0.4)
#'
#'
#'
#'
#' transform tess into polygons so I can plot it with mapview
owin2Polygons <- function(x, id="1") {
  stopifnot(spatstat.geom::is.owin(x))
  x <- spatstat.geom::as.polygonal(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords=closering(cbind(p$x,p$y)),
                             hole=is.hole.xypolygon(p))  })
  z <- Polygons(pieces, id)
  return(z)
}

tess2SP <- function(x) {
  stopifnot(spatstat.geom::is.tess(x))
  y <- spatstat.geom::tiles(x)
  nam <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nam[i])
  return(SpatialPolygons(z))
}
#'tranform y into polygon
#-------------------------------
fggh<-tess2SP(y)
#' now transform it into an sf object
#-------------------------------
st_voronoi <- st_as_sf(fggh)
#' Get the referential of the border for the voronoi polygons
#------------------------------
st_crs(st_voronoi)<- lok

###############################################################################
############################# FINAL PLOT ######################################
###############################################################################
mapview(bord, legend = FALSE,alpha.regions = 0.3, aplha = 1.4, col.regions = "grey") +
  mapview(Main_camp,  layer.name ="Main camp area", col.regions = "orange")+
  mapview(hey, cex = 0.3, layer.name ="waterholes", col.regions = "black")+
  mapview(st_voronoi, alpha.regions=0.1, layer.name= "voronoi polygons")


main_camp_voronoi <- st_voronoi[c(8, 13, 14, 16, 18, 22, 26, 28, 31,37, 43,45,50, 51, 56, 60, 64 ),]

mapview(bord, legend = FALSE,alpha.regions = 0.3, aplha = 1.4, col.regions = "grey") +
  mapview(Main_camp,  layer.name ="Main camp area", col.regions = "orange")+
  mapview(hey, cex = 0.3, layer.name ="waterholes", col.regions = "black")+
  mapview(main_camp_voronoi, alpha.regions=0.1, layer.name = "polygons used for the analysis")



#31 is manga_1
# 28 is makwa
# 18 is kennedy_2
#22 is livingi
#8 is caterpillar
#13 is dopi
# 14 is guvalala
# 16 is jambile
#43 is ngweshla
#45 is nyamandh
# 50 is samavund
# 51 is secheche
# 26 is makololov
# 37 is mfagazaan

# Do it using ggplot
st_crs(Main_camp)<- lok
st_crs(blocks)
st_crs(Main_camp)

##################################################################"" USING GGPLOT
#Import data
world<-st_read("figures/map/world_boundaries/world-administrative-boundaries.shp")
#get data to map africa only
africa <- world%>% subset(continent=="Africa") 
#get data to get Zimbabwe only
zimbabwe <- world%>% subset(name=="Zimbabwe")



Africa_plot <-
  ggplot() +
 ylim(c(35, -37))+
 xlim(c(-15, 50))+
  geom_sf(data = africa, color="black", fill="grey88") +
  geom_sf(data = zimbabwe , fill="tan4",alpha=0.5, color="tan4")+ 
#geom_sf(data = bord, color="tan", fill="tan" )+
  theme( panel.grid.major = element_blank(),
         panel.background = element_rect(fill='transparent'),#, #transparent panel bg
         plot.background = element_rect(fill='transparent', color=NA),
         #panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.7))+
        # panel.background =element_blank()) +
        #plot.background = element_rect(fill = 'white', color = 'white')) +
  theme(axis.text=element_blank(), 
        axis.ticks=element_blank(), 
        axis.title = element_blank())

Africa_plot
 # labs(y=" ", x=" ")#+
 # annotate("text", x = 44, y = 27, label = "Zimbabwe", colour="gray28", size = 6, fontface = "bold")
 
ggsave(filename = "Africa_map.png",
       plot = Africa_plot,
       bg = "transparent", 
       width = 2, height = 1.5, units = "in")

ggplot() +
  labs(x="Longitude", y= "Latitude")+
  scale_fill_manual(values = c("Study area" = "darkorange1", "Voronoi polygons"= "tan2", "Waterholes"= "white"),
 # scale_fill_manual(values = c("Study area" = "grey", "Voronoi polygons"= "grey61", "Waterholes"= "white"), 
                    guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "blank"), shape =c( NA, NA, 16)))) +
 # scale_color_manual("",values = c( "Study area"="grey", "Voronoi polygons"= "black", "waterholes"= "grey0"), 
  #                   guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))+
  geom_sf(data = bord, alpha = 0.3) +
  geom_sf(data = Main_camp, aes( fill="Study area"))+
  geom_sf(data=main_camp_voronoi, aes(fill="Voronoi polygons"))+
   #scale_shape_manual(values = c("waterholes"=16),
                   #   guide = guide_legend(override.aes = list(linetype = c("blank"), 
                                                           #    shape = c(16))))+
  geom_sf(data = hey , color="grey0", aes( fill="Waterholes"),size=1, show.legend = "point")+
  theme( panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_rect(colour = "black", fill=NA, size=0.7),
       panel.background = element_blank(), 
       legend.title= element_blank()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering, width = unit(1, "cm"), height = unit(1, "cm"))+
  ylim(7800860, 7990972)+
  xlim(299981, 559299)+
   labs(color=NULL)+
theme(legend.position = c(0.15,0.36)) +
  annotation_custom(ggplotGrob(Africa_plot),xmin = 147781,xmax=480990 ,ymin = 7910960, ymax = 8004099)

  #annotation_custom(ggplotGrob(Africa_plot),xmin = 151081,xmax=490990 ,ymin = 7890060, ymax = 7994999)

#################################Hwange only

hwange_map <- ggplot() +
  ylim(c(20, 18.4))+
  xlim(c(25.6, 27.6))+
  labs(x="Longitude", y= "Latitude")+
  geom_sf(data = around_zim, fill="grey94") +
  geom_sf(data = zimbabwe, color="black", fill="grey94" )+
  scale_fill_manual(values = c("Study area" = "burlywood4", "Voronoi polygons"= "transparent", "Hwange National Park"= "tan", "Waterholes"= "white"),
                      guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "solid", "blank"), shape =c( NA, NA, NA, 16)))) +
    geom_sf(data = bord,  aes(fill="Hwange National Park") )+
  geom_sf(data = Main_camp, aes( fill="Study area"))+
  geom_sf(data=main_camp_voronoi, alpha=0.3,color="maroon", aes(fill="Voronoi polygons"))+
  geom_sf(data = hey , color="grey0", aes( fill="Waterholes"),size=1, show.legend = "point")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
         legend.title= element_blank(),
         axis.line.x.bottom = element_line(color = 'black'),
         axis.line.y.left   = element_line(color = 'black'),
          axis.text.y.right  = element_blank(),
         axis.ticks.y.right = element_blank(),
         panel.border       = element_blank(),
         legend.text=element_text(size=14),
         axis.text=element_text(size=12),
         axis.title =element_text(size=15), 
         legend.background = element_rect(fill='transparent')) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering, width = unit(1, "cm"), height = unit(1, "cm"))+
   labs(color=NULL)+
  annotate(geom="text", x=27.3, y=18.7 ,label="Zimbabwe", color="grey42", fontface=3, size=5)+
  annotate(geom="text", x=25.8, y=19.5 ,label="Botswana", color="grey42", fontface=3, size=5)+
  theme(legend.position = c(0.81,0.18)) +
  #theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 3))#c(0.82,0.16)) 
 

hwange_map
################ Hwange on zimbabwe

#get data to get Zimbabwe only
around_zim <- africa%>% subset(name=="Zimbabwe"| name=="Botswana"| name == "Zambia"|name == "Angola"| name=="Namibia"| name=="Mozambique"| name=="South Africa"| name=="Malawi")

library(tidyr)

 around_plot<- ggplot() +
    ylim(c(25, 14))+
    xlim(c(24, 38))+
  geom_sf(data = around_zim, fill="wheat") +
  #geom_sf(data = around_zim, fill="grey88") +
   #geom_sf(data = zimbabwe, color="wheat3", fill="wheat" )+
   geom_sf(data = zimbabwe, color="tan4", fill="tan4", alpha=0.4 )+
    geom_sf(data = bord, color="black", fill="indianred4", alpha=0.8)+
    theme(legend.position = "bottom", 
          axis.line.x.bottom = element_line(color = 'black'),
          axis.line.y.left   = element_line(color = 'black'),
          legend.title = element_blank(), 
          panel.background= element_rect(fill="lightblue"), 
          panel.grid.major = element_blank(),
          axis.title = element_text(size=15), 
          axis.text=element_text(size=12))+
   annotation_scale(location = "br", width_hint = 0.2) +
   annotation_north_arrow(location = "br", which_north = "true", 
                          pad_x = unit(0.45, "in"), pad_y = unit(0.3, "in"),
                          style = north_arrow_fancy_orienteering, width = unit(1, "cm"), height = unit(1, "cm"))+
     annotate(geom="text", x=25, y=22 ,label="Botswana",
               color="black", fontface=3, size=5)+
    annotate(geom="text", x=26, y=15 ,label="Zambia",
             color="black", fontface=3, size=5)+
    annotate(geom="text", x=30, y=19 ,label="Zimbabwe",
             color="black", fontface=3, size=5)+
    annotate(geom="text", x=29.5, y=24 ,label="S. Africa",
             color="black", fontface=3, size=5)+
    annotate(geom="text", x=33.5, y=23 ,label="Mozambique",
             color="black", fontface=3, size=5)+
    annotate(geom="text", x=26.5, y=20.2 ,label="Hwange National Park",
             color="orangered4", fontface=3)+
    annotation_custom(ggplotGrob(Africa_plot),xmin = 33,xmax=38.8 ,ymin = -20 , ymax = -12.7)+
   labs(x="Longitude", y="Latitude")
 
 
 around_plot#  
 
 
 grid.arrange( around_plot, hwange_map,nrow=1) 
 