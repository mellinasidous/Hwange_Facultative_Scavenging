## Final Hwange figure 2 
## 19/01/2023

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(scales)
library(gridExtra)
library(magick)
library(grid)
#'
#'
#'
#' 
###Load data
#'---------------------------------------------- 
#' raw data matrix for herbivore and hyena
load("data/matrix_tess_herbivore_ponderate.RData") # HERBIVORES herbivore_matrix
load("data/matrix_tess_hyena_ponderate.RData") # HYENAS hyena_matrix
#'
#'
#' load picture of wildebeest, hyaena and elephant
pict_hyen <- image_read("figures/picture/Fig2_hyene.png")
pict_herb <- image_read("figures/picture/herbivore_Fig2.png")
pict_elep <- image_read("figures/picture/grey_elephant.png")

#### PLOT OBSERVATIONS 
################ HERBIVORES 
herbivore_new <- herbivore_matrix[-17,] 
#'
#'Get 95% of the data so we remove extreme value and see better the trands
coord <- which (herbivore_new >= quantile(herbivore_new,probs=c(.95), na.rm= TRUE)  )
# arrange data
herbivore_new[coord] <- NA
herbivore_new[1, 42] <- NA
# modify data
sp_data <- herbivore_matrix[-17,]
sp_data[1, 42] <- NA
# create df tà plot
est_to_plot <- cbind(t(herbivore_new), time= 1972:2020, apply(t(sp_data), 1, median, na.rm=T))
colnames(est_to_plot)[19] <- "density"
to_plot_herb<- est_to_plot %>% as.data.frame()

############################### PLOT

## 2 - estimated + observed
obs_herb<-ggplot(data= to_plot_herb, aes(x = time)) +
  annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=1.7 ,ymax = 1.93)+
  annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=1.7, ymax =1.93)+
  geom_line(aes(y=density),color="black") + 
  geom_smooth(aes(y=density), color="black", alpha = 0.6)+
  scale_color_manual("",
                     values = c( "caterpillar"="orangered1", 
                                 "dopi"= "purple","jambile" ="green",  "kennedy_2"= "cyan4", 
                                 "livingi"= "yellow" , "makwa"= "royalblue2", "manga_1"= "darkgoldenrod1" , 
                                 "ngweshla"= "olivedrab4", "nyamandhlovu"= "gold", "sinanga"= "firebrick4",  "tshebe tshebe" = "tan4" ,
                                 #"umkhawu" = "pink",
                                 "guvalala"="orange","makololov"="palegreen4",
                                 "samavund"="wheat","mfagazaan"= "brown","wexcau"="chocolate"))+
  geom_point (aes(x= time, y=caterpillar, colour = "caterpillar"))+
  geom_point (aes(x= time, y=dopi, colour = "dopi"))+
  geom_point (aes(x= time, y=jambile, colour = "jambile"))+
  geom_point (aes(x= time, y=kennedy_2, colour = "kennedy_2"))+
  geom_point (aes(x= time, y=livingi, colour = "livingi"))+
  geom_point (aes(x= time, y=makwa, colour = "makwa"))+
  geom_point (aes(x= time, y=manga_1, colour = "manga_1"))+
  geom_point (aes(x= time, y=ngweshla, colour = "ngweshla"))+
  geom_point (aes(x= time, y=nyamandhlovu, colour = "nyamandhlovu"))+
  geom_point (aes(x= time, y=sinanga, colour = "sinanga"))+
  geom_point (aes(x= time, y=tshebe, colour = "tshebe tshebe"))+
  # geom_point (aes(x= time, y=umkhawu, colour = "umkhawu"))+
  geom_point (aes(x= time, y=guvalala, colour = "guvalala"))+
  geom_point (aes(x= time, y=makololov, colour = "makololov"))+
  geom_point (aes(x= time, y=samavund, colour = "samavund"))+
  geom_point (aes(x= time, y=secheche, colour = "secheche"))+
  geom_point (aes(x= time, y=mfagazaan, colour = "mfagazaan"))+
  geom_point (aes(x= time, y=wexcau, colour = "wexcau"))+
  labs(x = "Year",  y = c(as.expression(bquote("Observed density (individual/" ~ km^2 ~ ")"))), tag= "A")+
  scale_x_continuous(breaks= seq(1972, 2020, by= 4 )) +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_line()) +
  theme(legend.position = "none",
        plot.tag = element_text(size=28)) +
  geom_vline(xintercept = c(1986,1992, 2008), 
             color = "black", size=0.3, linetype=2)+
  theme(axis.text=element_text(size=16),
       axis.title=element_text(size=24), 
       plot.title = element_text(size = 28,hjust = 0), 
      plot.subtitle = element_text(size=24))
   #annotation_custom(rasterGrob(pict_herb), xmin=2012 , xmax= 2020,ymin=2.15, ymax =2.5)
   
obs_herb


################ Hyenas observation
# arrange data
hyena_new <- hyena_matrix[-17,] 
# get 95% of the data
coord <- which (hyena_new >= quantile(hyena_new,probs=c(.95), na.rm= TRUE)  )
# keep arranging data
hyena_new[coord] <- NA
hyena_new[1, 42] <- NA
sp_data <- hyena_matrix[-17,]
sp_data[1, 42] <- NA

#create the dataframe to plot
est_to_plot <- cbind(t(hyena_new), time= 1972:2020, apply(t(sp_data), 1, median, na.rm=T))
colnames(est_to_plot)[19] <- "density"
to_plot_hyen<- est_to_plot %>% as.data.frame()


### PLOT
## 2 - estimated + observed
obs_hyen<-ggplot(data= to_plot_hyen, aes(x = time)) +
  #annotation_custom(rasterGrob(pict_elep), xmin=1970 , xmax= 1976,ymin=0.1025, ymax =0.117)+
  #annotation_custom(rasterGrob(pict_elep), xmin=1992.6 , xmax= 1998.6,ymin=0.1025, ymax =0.117)+
  annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=0.1025, ymax =0.117)+
  annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=0.1025, ymax =0.117)+
  geom_line(aes(y=density), color="black")+
  geom_smooth(aes(y=density), color="black", alpha=0.6)+
  scale_color_manual("",
                     values = c( "caterpillar"="orangered1", 
                                 "dopi"= "purple","jambile" ="green",  "kennedy_2"= "cyan4", 
                                 "livingi"= "yellow" , "makwa"= "royalblue2", "manga_1"= "darkgoldenrod1" , 
                                 "ngweshla"= "olivedrab4", "nyamandhlovu"= "gold", "sinanga"= "firebrick4",  "tshebe tshebe" = "tan4" ,
                                 #"umkhawu" = "pink",
                                 "guvalala"="orange","makololov"="palegreen4",
                                 "samavund"="wheat","mfagazaan"= "brown","wexcau"="chocolate"))+
  geom_point (aes(x= time, y=caterpillar, colour = "caterpillar"))+
  geom_point (aes(x= time, y=dopi, colour = "dopi"))+
  geom_point (aes(x= time, y=jambile, colour = "jambile"))+
  geom_point (aes(x= time, y=kennedy_2, colour = "kennedy_2"))+
  geom_point (aes(x= time, y=livingi, colour = "livingi"))+
  geom_point (aes(x= time, y=makwa, colour = "makwa"))+
  geom_point (aes(x= time, y=manga_1, colour = "manga_1"))+
  geom_point (aes(x= time, y=ngweshla, colour = "ngweshla"))+
  geom_point (aes(x= time, y=nyamandhlovu, colour = "nyamandhlovu"))+
  geom_point (aes(x= time, y=sinanga, colour = "sinanga"))+
  geom_point (aes(x= time, y=tshebe, colour = "tshebe tshebe"))+
  # geom_point (aes(x= time, y=umkhawu, colour = "umkhawu"))+
  geom_point (aes(x= time, y=guvalala, colour = "guvalala"))+
  geom_point (aes(x= time, y=makololov, colour = "makololov"))+
  geom_point (aes(x= time, y=samavund, colour = "samavund"))+
  geom_point (aes(x= time, y=secheche, colour = "secheche"))+
  geom_point (aes(x= time, y=mfagazaan, colour = "mfagazaan"))+
  geom_point (aes(x= time, y=wexcau, colour = "wexcau"))+
  labs(x = "Year",  y = c(as.expression(bquote("Observed density (individual/" ~ km^2 ~ ")"))), tag="B")+
      # subtitle = "B") +
  scale_y_continuous(limit=c(0,0.15),oob=squish)+
  scale_x_continuous(breaks= seq(1972, 2020, by= 4 )) +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_line()) +
  theme(legend.position = "none",
        plot.tag = element_text(size=28)) +
  geom_vline(xintercept = c(1986,1992, 2008), 
             color = "black", size=0.3, linetype=2)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=24), 
        plot.title = element_text(size = 28,hjust = 0), 
        plot.subtitle = element_text(size=24))
  #annotation_custom(rasterGrob(pict_hyen), xmin=2012 , xmax= 2021,ymin=0.134, ymax =0.157)

  #annotation_custom(rasterGrob(pict_hyen), xmin= 1970, xmax= 1978, ymin = 1.7, ymax =1.9)
obs_hyen

########################################################################################################
####################################### NOW PLOT MODEL'S RESULTS #######################################
########################################################################################################

## Results wuthout caterpillar 2013 and without R diff
load("results/herbivore_estimates_newrain.RData")
load("results/hyena_estimates_newrain.RData")

# Change name to have it shorter
herbivore_estimates <- herbivore_estimates_newrain

## Choose some of these colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# extract estimates
Cst_est <- herbivore_estimates[[1]]
Tim_est <- herbivore_estimates[[2]]
Per_est <- herbivore_estimates[[3]] 
######################################################## PLOT CONSTANT MODEL
############################## PLOT CONSTANT MODEL
# final dataframe to plot
ggt_cst<-Cst_est%>% dplyr::select(est, conf_low, conf_up)
ggt_ut<-Tim_est%>% dplyr::select(est, conf_low, conf_up)
ggt_per <-Per_est%>% dplyr::select(est, conf_low, conf_up)

# Create the dataframe to plot to have all the estimates of the three models together for herbivores
ggt <- cbind(ggt_cst, ggt_ut, ggt_per)
colnames(ggt) <- c("cst_est", "cst_low", "cst_up", "ut_est", "ut_low", "ut_up", "per_est", "per_low", "per_up")
estim_r<- ggt %>% exp()# %>% -0.00001 
estim_r$time <- 1972:2020
#est_to_plot <- cbind(t(herbivore_matrix[-17,]), estim_r)
estim_r_herb<- estim_r

### PLOTS
## 1 st = estimated alone 
est_x_cst_herb<- ggplot(data= estim_r_herb, aes(x = time)) +
  #annotation_custom(rasterGrob(pict_elep), xmin=1970 , xmax= 1976,ymin=1.32, ymax =1.47)+
  #annotation_custom(rasterGrob(pict_elep), xmin=1992.6 , xmax= 1998.6,ymin=1.32, ymax =1.47)+
  annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=1.32, ymax =1.47)+
  annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=1.32, ymax =1.47)+
  scale_color_manual("",
                     values = c( "constant"="#009E73" , "time varying"="#CC79A7", 
                                 "period"= "#E69F00"))+
  geom_ribbon(aes(ymin =cst_low, ymax = cst_up), fill = "grey92", alpha = 0.6) +
  geom_line(aes(y=cst_est, colour= "constant"))+
  geom_line(aes(y=ut_est,colour= "time varying"))+
  geom_ribbon(aes(ymin =ut_low, ymax = ut_up), fill = "grey92", alpha = 0.6) +
  geom_line(aes(y=per_est,colour= "period"), linewidth=1)+
  geom_ribbon(aes(ymin =per_low, ymax = per_up), fill = "grey92", alpha = 0.6) +
  scale_x_continuous(breaks= seq(1972, 2020, by= 4 )) +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  geom_vline(xintercept = c(1986,1992, 2008), 
             color = "black", size=0.3, linetype=2)+
    labs(y=  c(as.expression(bquote("Estimated density (individual/" ~ km^2 ~ ")"))),x = "Year", tag="C")+ 
  theme(axis.line=element_line()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=24), 
        #plot.title = element_text(size=20,hjust = 0.5),
        plot.subtitle = element_text(size=24)) + 
  theme(legend.key.size = unit(1, 'cm'),
        plot.tag = element_text(size=28),
        plot.title = element_text(hjust = 0, size=25),
          #legend.position = c(.02, .88),
        legend.position = c(0.75,1), # other version 1
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.margin =margin(1, 1, 1, 1), 
        legend.text=element_text(size=16),
        legend.title=element_blank())+
  annotation_custom(rasterGrob(pict_herb), xmin= 2012, xmax= 2020, ymin = 1.63, ymax =1.85)#+
  
  


est_x_cst_herb

### Do it for hyenas : 
# chnage name to have it shorter
hyena_estimates <- hyena_estimates_newrain
# exctract model estimates
Cst_est <- hyena_estimates$esti_hyen_cst
Tim_est <- hyena_estimates$esti_hyen_ut
Per_est <- hyena_estimates$esti_hyen_per
########################################################
############################## PLOT CONSTANT
# final dataframe to plot
ggt_cst<-Cst_est%>% dplyr::select(est, conf_low, conf_up)
ggt_ut<-Tim_est%>% dplyr::select(est, conf_low, conf_up)
ggt_per <-Per_est%>% dplyr::select(est, conf_low, conf_up)
# Create the dataframe to plot to have all the estimates of the three models together for hyena
ggt <- cbind(ggt_cst, ggt_ut, ggt_per)
colnames(ggt) <- c("cst_est", "cst_low", "cst_up", "ut_est", "ut_low", "ut_up", "per_est", "per_low", "per_up")
estim_r<- ggt %>% exp()# %>% -0.00001 
estim_r$time <- 1972:2020
#est_to_plot <- cbind(t(herbivore_matrix[-17,]), estim_r)
estim_r_hyen <- estim_r

### PLOTS
## 1 st = estimated alone 
est_x_cst_hyen<- ggplot(data= estim_r_hyen, aes(x = time)) +
  #annotation_custom(rasterGrob(pict_elep), xmin=1970 , xmax= 1976,ymin=0.0335, ymax =0.038)+ # prop2
  #annotation_custom(rasterGrob(pict_elep), xmin=1992.6 , xmax= 1998.6,ymin=0.0335, ymax =0.038)+# prop2
  annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=0.0335, ymax =0.038)+ # prop2
  annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=0.0335, ymax =0.038)+# prop2
  scale_color_manual("",
                     values = c( "constant"="#009E73" , "time varying"="#CC79A7",  #"orangered1", 
                                 "period"= "#E69F00"))+
  geom_ribbon(aes(ymin =cst_low, ymax = cst_up), fill = "grey92", alpha = 0.6) +
  geom_line(aes(y=cst_est, colour= "constant"))+
  geom_line(aes(y=ut_est,colour= "time varying"))+
  geom_ribbon(aes(ymin =ut_low, ymax = ut_up), fill = "grey92", alpha = 0.6) +
  geom_line(aes(y=per_est,colour= "period"), linewidth = 1)+
  geom_ribbon(aes(ymin =per_low, ymax = per_up), fill = "grey92", alpha = 0.6) +
  scale_x_continuous(breaks= seq(1972, 2020, by= 4 )) +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  geom_vline(xintercept = c(1986,1992, 2008), 
             color = "black", size=0.3, linetype=2)+
  labs(y=  c(as.expression(bquote("Estimated density (individual/" ~ km^2 ~ ")"))),x = "Year", tag="D")+ 
       #title = "Estimated density of hyena") +
       #subtitle = "D") +
  theme(axis.line=element_line()) +
  theme(plot.tag = element_text(size=28),
        axis.text=element_text(size=16),
        axis.title=element_text(size=24), 
        plot.title = element_text(size=28,hjust = 0),
        plot.subtitle = element_text(size=24)) + 
  theme(legend.key.size = unit(1, 'cm'),
          #legend.position = c(.02, .88),
          legend.position = c(0.75,1), # other version 1
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.margin = margin(0.1,0.1, 0.1, 0.1), 
          legend.text=element_text(size=16),
          legend.title=element_blank())+
  geom_line(aes(y=ut_est,colour= "time varying"))+
  #annotation_custom(rasterGrob(pict_hyen), xmin= 1972, xmax= 1978, ymin = 0.044, ymax =0.052)
#annotation_custom(rasterGrob(pict_hyen), xmin= 2012, xmax= 2020, ymin = 0.041, ymax =0.048)

  #annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=0.0435, ymax =0.048)+
  #annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=0.0435, ymax =0.048)



est_x_cst_hyen



#### FINAL GRAPH :
grid.arrange(obs_herb, obs_hyen,est_x_cst_herb, est_x_cst_hyen)

