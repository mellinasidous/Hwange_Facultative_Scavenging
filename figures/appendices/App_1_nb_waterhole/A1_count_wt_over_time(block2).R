## Figure 2 : Counts the number of waterhole over time
## 18/12/2022

rm(list=ls())

#'libraries
#-----------------------------------------
library(tidyverse)
library(sf)
library(gridExtra)
#' load data
#-----------------------------------------
load("figures/appendices/App_1_nb_waterhole/data_block_tessname.RData")
pict_ele <- image_read("figures/picture/elephant.png")
#' arrange data 
dat2 <- dat %>% st_drop_geometry()
#'
#'
###############################################
### A) NUMBER OF WATERHOLE IN MAIN CAMP AREA ##
###############################################
#'
#' keep only waterholes in main camp area
#-----------------------------------------
dat3<-dat2%>% subset(blockID == "bloc9" | blockID == "bloc10")
#' what are the waterholes inside of main camp ?
#-----------------------------------------
table(dat3$waterpoint)
#' create a dataframe with the counts of the different waterholes each year (to see if they're counted or not)
#-----------------------------------------
sites <- dat3 %>%
  dplyr::select(date, waterpoint, Total) %>%
  mutate(date = as.numeric(date), Total = as.numeric(Total)) %>%
  group_by(waterpoint, date) %>%
  summarise(Total = sum(Total)) 
#'
#'
# get waterholes in row and year in columns so we see better what year each waterhole was monitored or not
#-----------------------------------------
HNP_data_site <-  sites%>%
  group_by(waterpoint) %>%
  #summarise(Total = sum(Total)) %>%
  complete(date = 1972:2020) %>%
  pivot_wider(names_from = waterpoint, values_from = Total)  %>% t()
#'
#'
# arrange this matrix
#-----------------------------------------
colnames(HNP_data_site) <- HNP_data_site[1,]
table_bloc_f <- HNP_data_site[-1,]
#'
#'
#' change year monitored in 1s and year not monitored in 0s
#-----------------------------------------
site_count <- table_bloc_f
site_count[!is.na(site_count)] <- 1
site_count[is.na(site_count)] <- 0
#'
#'
# make the sum of 1s (so waterholes followed) each year
#-----------------------------------------
n_wt_pyear<- apply(site_count, 2, sum, na.rm=T) %>% as.data.frame()
colnames(n_wt_pyear) <- c("n_wt")
n_wt_pyear$date <- as.numeric(rownames(n_wt_pyear))
#'
#'
####  Plot that : 
#-----------------------------------------
ngamo_area <- ggplot(n_wt_pyear, aes(x=date, y=n_wt, group=1)) +
  geom_bar(stat="identity", alpha = 0.1) +
  geom_bar(stat="identity", color= "black", fill = NA) +
  #geom_point(color="black") + 
  #geom_smooth(color="black", alpha = 0.2)+
  labs(x = "Year",  y = "Number of waterholes holding water\nat the end of the dry season in the study area") +
  #ggtitle("Number of waterhole in Main Camp area over time")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_line()) +
  annotation_custom(rasterGrob(pict_ele), xmin=1973 , xmax= 1979,ymin=36, ymax =39)+
  annotation_custom(rasterGrob(pict_ele), xmin=1996 , xmax= 2002,ymin=36, ymax =39)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  geom_vline(xintercept = c(1986,1992, 2008), 
             size=1,  linetype=2)
#'
#'
#'
##############################################################
############ DO IT FOR ALL THE PARK ##########################
##############################################################
#' get all the waterholes this time (and transform another table than dat2)
#-----------------------------------------
dat3b<-dat2
#'
#'
#d' what are the waterholes inside of the park ?
#-----------------------------------------
nrow(table(dat3b$waterpoint) %>% as.data.frame()) # 138 waterhole in total 
#'
#'
#' create a dataframe with the counts of the different waterholes each year (to see if they're counted or not)
#-----------------------------------------
sitesb <- dat3b %>%
  dplyr::select(date, waterpoint, Total) %>%
  mutate(date = as.numeric(date), Total = as.numeric(Total)) %>%
  group_by(waterpoint, date) %>%
  summarise(Total = sum(Total)) 
#'
#'
#'
#' get waterholes in row and year in columns so we see better what year each waterhole is monitored or not
#-----------------------------------------
HNP_data_siteb <-  sitesb%>%
  group_by(waterpoint) %>%
  #summarise(Total = sum(Total)) %>%
  complete(date = 1972:2020) %>%
  pivot_wider(names_from = waterpoint, values_from = Total)  %>% t()
#'
#'
#' arrange this matrix
#-----------------------------------------
colnames(HNP_data_siteb) <- HNP_data_siteb[1,]
table_bloc_fb <- HNP_data_siteb[-1,]
#'
#'
#' change year monitored in 1s and year not monitored in 0s
#-----------------------------------------
site_countb <- table_bloc_fb
site_countb[!is.na(site_countb)] <- 1
site_countb[is.na(site_countb)] <- 0
#'
#'
#' make the sum of 1s (so waterholes monitored) each year
#-----------------------------------------
n_wt_pyearb<- apply(site_countb, 2, sum, na.rm=T) %>% as.data.frame()
colnames(n_wt_pyearb) <- c("n_wt")
n_wt_pyearb$date <- as.numeric(rownames(n_wt_pyear))
#'
#'
#'
####  Plot that : 

all_park <- ggplot(n_wt_pyearb, aes(x=date, y=n_wt, group=1)) +
  geom_bar(stat="identity", alpha = 0.1) +
  geom_bar(stat="identity", color= "black", fill = NA) +
  #geom_point(color="black") + 
  #geom_smooth(color="black", alpha = 0.2)+
  labs(x = "Year",  y = "Number of waterholes holding water\nat the end of the dry season in the Park") +
  #ggtitle("Number of waterhole in Hwange National Park over time")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_line()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  annotation_custom(rasterGrob(pict_ele), xmin=1973 , xmax= 1979,ymin=80, ymax =87)+
  annotation_custom(rasterGrob(pict_ele), xmin=1996 , xmax= 2002,ymin=80, ymax =87)+
  geom_vline(xintercept = c(1986,1992, 2008), 
             size=1,  linetype=2)

#'
#'
#'
#' GET BOTH GRAPHS TOGETHER
grid.arrange(all_park, ngamo_area, ncol=1)

######################### IN the text : number minimal and maximal of waterhole monitored
min(n_wt_pyearb$n_wt)#21
max(n_wt_pyearb$n_wt)#86

max(n_wt_pyear$n_wt)#38
min(n_wt_pyear$n_wt)#6

