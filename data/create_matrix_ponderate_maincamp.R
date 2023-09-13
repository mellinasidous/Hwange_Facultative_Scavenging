## Ponderate the waterhole counts with the number of waterhole followed : 
## 

rm(list=ls())

library(tidyverse)
library(sf)
library(spatstat)
library(gridExtra)

setwd("C:/Users/msidous/Desktop/projet_1/hwange_hypohyper")

## Load data
load("data/data_block_tessname.RData") # dat
load("data/Dir_tesselation.RData") #Dir
# get the number of waterhole 
load("data/nb_wt_overt_incells.RData") # n_wt_tess
## get area of cells 
load ("data/size_of_tess.RData") #getarea
#position of na 
load("data/where_nas_incells.RData") #where_na


# remove geometry to add area to this file
dat<-st_drop_geometry(dat)
### Give the size of each polygon
dataz<-  right_join(dat, getarea[,c(1,4)], by = "tess_name")

## Waterhole followed enough to be used for analysis :
dat <- dataz%>% subset(tess_name == "baobab" | tess_name == "caterpillar" |tess_name == "chingahobe" |
                         tess_name == "detepicnic" |tess_name == "dopi" |tess_name == "guvalala" |tess_name == "inyantue"|
                         tess_name == "jambile"  |tess_name == "kennedy_2"  |tess_name == "littletoms"  |
                         tess_name == "livingi"  |tess_name == "mabuya" | tess_name == "makololov"  | tess_name == "makwa"  |
                         tess_name == "mandavu" | tess_name == "manga_1" | tess_name == "masuma" | tess_name == "samavund" | 
                         tess_name == "mfagazaan"  |tess_name == "ngweshla"  | tess_name == "nehimba" | 
                         tess_name == "nyamandhlovu"  |tess_name == "reedbuck_vlei"|tess_name == "saltpan"|
                         tess_name == "samavundhla"  |tess_name == "shumba"  |tess_name == "sinanga"  |tess_name == "secheche" | 
                         tess_name == "tsamhole"  |tess_name == "tshakabika"  |tess_name == "tshebe"  | tess_name == "robins"  |
                         tess_name == "tshompani"  |tess_name == "umkhawu"  |tess_name == "wexcau"  |tess_name == "shapi"  |
                         tess_name == "josivanini"  |tess_name == "bigtoms" )


################################################### Formate species names ---
#plotdensities <- dat %>% group_by(tess_name, date, species) %>% summarise(abondance = sum(Total), area = (area/ 10e6), species = species, blockID = blockID, bblok=bblok, density= (abondance/area)) #%>%ungroup()%>% summarise(density= abondance/area)
sp_type <- as.data.frame(table(dat$species))
herb <- c("roan","kudu","warthog","impala","waterbuck","wildebeest", "sable", "zebra")

sp_type$type <- "other"
for(i in 1 : nrow(sp_type)){ 
  if(sp_type$Var1[i] %in% herb ){ sp_type$type[i] <- "herbivore" }
  if(sp_type$Var1[i] == "hyaena_spotted"  ){ sp_type$type[i] <- "hyena" } 
}

## transform the herbivores names into herbivores
colnames(sp_type)<- c("species", "occurence", "type")
dat <- right_join(dat, sp_type[,c(1,3)], by= "species") 



###################### For herbivores : 
herbivore_matrix <- dat %>%
  subset(blockID == "bloc9" | blockID =="bloc10") %>%
  filter (type == "herbivore"  ) %>%
  select(date, waterpoint, species, Total, tess_name) %>%
  mutate(date = as.numeric(date), species = as_factor(species),  Total = as.numeric(Total)) %>%
  group_by(tess_name, date) %>%
  summarise(Total = sum(Total)) %>%
  ungroup() %>%
  group_by(tess_name) %>%
  complete(date = 1972:2020) %>%
  pivot_wider(names_from = tess_name, values_from = Total) %>%
  select(-date)

## divide counts by the number of waterholes
for(m in 1:length(n_wt_tess)){ 
  for(g in 1: ncol(herbivore_matrix)){
    if(isTRUE(colnames(herbivore_matrix)[g] == names(n_wt_tess)[[m]])){ herbivore_matrix[,g] <-  herbivore_matrix[,g] / n_wt_tess[[m]]}  
  }
}

## Transform into density
for(m in 1:nrow(getarea)){ 
  for(g in 1: ncol(herbivore_matrix)){
    if(isTRUE(colnames(herbivore_matrix)[g] == getarea$tess_name[m])){ herbivore_matrix[,g] <-  herbivore_matrix[,g] / getarea$area[m]}  
  }
}

## get true nas vs 0s
herbivore_matrix[is.na(herbivore_matrix)] <- 0
#replace nas 
for(m in 1:length(where_na)){ 
  for(g in 1: ncol(herbivore_matrix)){
    if(isTRUE(colnames(herbivore_matrix)[g] == names(where_na)[[m]])){
      herbivore_matrix[  where_na[[m]], g ] <- NA
    }  #end if
  } # enf col herb 
} # end length where na


## FIN DE LA MISE EN FORME DE LA MATRICE 
herbivore_matrix<-t(herbivore_matrix)
colnames(herbivore_matrix)<- 1972:2020
### save with name of the block and the species 
save(herbivore_matrix, file = "data/matrix/matrix_tess_herbivore_ponderate.RData")


###################### For hyenas :  
hyena_matrix <- dat %>%
  subset(blockID == "bloc9" | blockID =="bloc10")%>%
  filter ( type == "hyena" ) %>%
  select(date, waterpoint, species, Total, tess_name) %>%
  mutate(date = as.numeric(date), species = as_factor(species),  Total = as.numeric(Total)) %>%
  group_by(tess_name, date) %>%
  summarise(Total = sum(Total)) %>%
  ungroup() %>%
  group_by(tess_name) %>%
  complete(date = 1972:2020) %>%
  pivot_wider(names_from = tess_name, values_from = Total) %>%
  select(-date)

## divide counts by the number of waterholes
for(m in 1:length(n_wt_tess)){ 
  for(g in 1: ncol(hyena_matrix)){
    if(isTRUE(colnames(hyena_matrix)[g] == names(n_wt_tess)[[m]])){ hyena_matrix[,g] <-  hyena_matrix[,g] / n_wt_tess[[m]]}  
  }
}
  
## Transform into density
for(m in 1:nrow(getarea)){ 
  for(g in 1: ncol(hyena_matrix)){
    if(isTRUE(colnames(hyena_matrix)[g] == getarea$tess_name[m])){ hyena_matrix[,g] <-  hyena_matrix[,g] / getarea$area[m]}  
  }
}

## Transform Na in 0s 
hyena_matrix[is.na(hyena_matrix)] <- 0
#replace nas 
for(m in 1:length(where_na)){ 
  for(g in 1: ncol(hyena_matrix)){
    if(isTRUE(colnames(hyena_matrix)[g] == names(where_na)[[m]])){
      hyena_matrix[  where_na[[m]], g ] <- NA
    }  #end if
  } # enf col herb 
} # end length where na



## FIN DE LA MISE EN FORME DE LA MATRICE 
hyena_matrix<-t(hyena_matrix)
colnames(hyena_matrix)<- 1972:2020

### save with name of the block and the species 
save(hyena_matrix, file = "data/matrix/matrix_tess_hyena_ponderate.RData")








