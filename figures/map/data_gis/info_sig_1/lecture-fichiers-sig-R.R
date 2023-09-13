
library(sf) # package pour faire des SIG
library(tidyverse) # manip et visualisation 
theme_set(theme_light()) # je change un peu les defauts de ggplot

# 1. on lit waterholes
wh <- st_read("waterholes.shp")

# il y a quoi dedans? des points 
wh

table(wh$WATERHOLE)

# visualise
wh %>%
  ggplot() +
  geom_sf()

# 2. on lit waterholes update Hugo
whhugo <- st_read("waterhole_park_update2_Hugo_2014.shp")

# il y a quoi dedans? des points 
whhugo

# et d'autres infos
whhugo %>%
  count(pump)

whhugo %>%
  count(Sym)

# visualise
whhugo %>%
  ggplot() +
  geom_sf()

# visualise avec pump
whhugo %>%
  ggplot() +
  geom_sf(aes(color = pump))+
  geom_sf_label(aes(label = name, size="2"), label.size= 0.0000000001, label.padding = unit(0.005, "lines"))

# 3. on lit les fichiers qui concernent l'aire d'étude
borderlines <- st_read("Border_lines.shp") # frontières de l'aire d'étude
border <- st_read("Border.shp") # polygone de l'aire d'étude

# 4. visualise le tout

ggplot() + 
  geom_sf(data = borderlines) + # 
  geom_sf(data = whhugo, aes(color = pump))#+
  #geom_sf_label(data=whhugo$name, aes(label = name, size="2"), label.size= 0.0000000001, label.padding = unit(0.005, "lines"))

# idem mais en supprimant les NAs 
ggplot() + 
  geom_sf(data = borderlines) + # 
  geom_sf(data = whhugo %>% 
            filter(!is.na(pump)), 
          aes(color = pump))

ggsave("sig-hwange.png", dpi = 300, width = 8, height = 8)


#nouvelle données SIG du 1er février 

wh <- st_read("waterpoints.shp")

wh

wh %>%
  ggplot() +
  geom_sf()+
  geom_sf_label(aes(label = Waterhole, size="2"), label.size= 0.0000000001, label.padding = unit(0.005, "lines"))


####last SIG data 

wl <- st_read("Waterholes.shp")

wl
wl %>%
  ggplot() +
  geom_sf()+
  #geom_sf(data = borderlines)
  geom_sf_label(aes(label = WATERPOINT, size="2"), label.size= 0.0000000001, label.padding = unit(0.005, "lines"))



