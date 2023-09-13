## Figure 1  : Annual Rainfall 
## 22/12/2022
#'
rm(list=ls())
#'
#'libraries
#-----------------------------------------
library(readxl)
library(kableExtra)
library(knitr)
library(magick)
#'
#'load data
#-----------------------------------------
load("data/annual_rainfall.RData") #annual_rain
pict_ele <- image_read("figures/picture/grey_elephant.png")
#'
## Plot rainfall : 
#'
#' arrange (get date and rainfall in columns)
#-----------------------------------------
annual_rain_plot<- annual_rain %>% t() %>% cbind(date= 1972:2020)  %>% as.data.frame() #%>% as.numeric()
#'
#'Now set date and pluvio values as numeric
#-----------------------------------------
annual_rain_plot$date <- as.numeric(annual_rain_plot$date)
annual_rain_plot$annual_pre <- as.numeric(annual_rain_plot$annual_pre)
#' plot
#-----------------------------------------
rain_plot<-ggplot(data = annual_rain_plot, aes(x=date , y=annual_pre))+
  geom_bar(stat="identity", alpha = 0.2) +
  geom_bar(stat="identity", color= "black", fill = NA) +
  labs(x = "Year",  y = "Annual rainfall (mm)") +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_line()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20)) +
  ggtitle("A")+
  geom_vline(xintercept = c(1986,1992, 2008), 
             size=0.3,  linetype=2)+
  annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=1100, ymax =1185)+
  annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=1100, ymax =1185)+
  theme(plot.title = element_text(size = 24))

#### Now I should do the with or without difference :   

#' B I run the models without rainfall as a correction
#-------------------------------------------------------------------------------
#' load data to do the models 
#' 
load("data/matrix_tess_herbivore_ponderate.RData") # HERBIVORES 
# put na for caterpillar 2013
herbivore_matrix[1,42] <- NA
# remove umkawhazaan
sp_data <- herbivore_matrix[-17,]# - umkhawuazaan
# modificiation of the datat
hm<-sp_data %>% +0.0001%>% log() 
# get number of year and waterhole for the models 
nwt<-nrow(sp_data)
nyear <- ncol(sp_data)
#'
#'
#'Set the model 
#-----------------------------------
modeluconstant_norain <- list(
  Z =  matrix( 1, nrow=nwt, ncol=1), 
  A = "equal")
#' Run the model
#-----------------------------------
fituconstant_norain <- MARSS(hm, model = modeluconstant_norain, method='kem') # AIC is 
#'
#'Get the estimated densities
#-----------------------------------
esti_norain<-tsSmooth(fituconstant_norain , interval =  "confidence")
colnames(esti_norain)<- c("rownames", "time", "est", "se", "conf_low", "conf_up" )
esti_norain <- esti_norain %>% mutate(date = 1972:2020)
#'
#'
#'# after back transformation
without_cov<-esti_norain %>% 
  ggplot(aes(x=date, y=exp(est) )) + 
  geom_ribbon(aes(ymin = exp(conf_low),
                  ymax = exp(conf_up)),    # shadowing cnf intervals
              fill = "black", alpha = 0.2) + 
  geom_line(color = "black", size=1) +
  geom_point( aes(x=date, y=apply((sp_data + 0.00001)  ,2, median, na.rm=TRUE)))+
  #labs(title="Estimation herbivore density constant model without covariate")+
  labs(x = "Year",  y = "Density of herbivores (indiv/km2)") +
  ggtitle("B")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_line()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))+
  geom_vline(xintercept = c(1986,1992, 2008), 
             size=0.3,  linetype=2)+
  annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=1.67, ymax =1.8)+
  annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=1.67, ymax =1.8)+
  theme(plot.title = element_text(size = 24))
  #geom_smooth(color="mediumslateblue") +
 # scale_y_continuous(limit=c(0,1.5),oob=squish)
#'
#'
##############################################
#'Panel C : model selection
##############################################
#'
#'
#'I need to run all the models wihtout covariate (period, constant time varying for both hyaenas and preys)
#'
########### HERBIVORES 
#'I already get constant for herbivores 
#------------------------------------------
herb_cst_norain <- fituconstant_norain$AICc
#' Now time varying for herbivores 
#------------------------------------------
#'define U
Ut <- array(list(0), dim = c(1, 1 , nyear)) # create the U matrix that depends on time and fills it 
for(i in 1: nyear){ 
  val <- paste( "U" , i) %>% str_remove_all(" ")
  Ut[,,i]<- list(val) 
}
#' set model
modelut_norain <- list(
  Z =  matrix( 1, nwt, ncol=1), 
  U = Ut,#process error   #time varying u
  #R = "diagonal and unequal", 
  A = "equal") #obs error)
#'
#'run model
fitut_norain <- MARSS(hm, model = modelut_norain, method='BFGS') #
#'
#'save AICc
herb_ut_norain <- fitut_norain$AICc
#'
#'
#' Now period for herbivores 
#------------------------------------------
#' define U
U1 <- "U1" %>%as.matrix() 
U2 <- "U2" %>% as.matrix()#period 2
U3 <- "U3" %>% as.matrix()#period 3
U4 <- "U4" %>% as.matrix() # last period
Up <- array(U1, dim = c(dim(U1), nyear )) # construit la matrice B qui depend du temps en la remplissant de B1
Up[16:21] <- U2 # from 1987 to 1992
Up[22:37] <- U3 #from 1993 to 2008
Up[38:nyear]<-U4 # from 2009 to 2020
#38 is 2009 and 36 is 2006
#' set model
modeluperiod_norain <- list(
  Z =  matrix( 1, nwt, ncol=1), 
  U = Up,#process error
  #R = "diagonal and unequal", 
  A = "equal") #obs error)
#'
#' run model
fituperiod_norain <- MARSS(hm, model = modeluperiod_norain, method='BFGS') ##  AICc is 3405.033
#'
#'save AICc 
herb_per_norain <- fituperiod_norain$AICc
#'
#'
########### HYENAS
#'load and arrange data
#---------------------------------------
load("data/matrix_tess_hyena_ponderate.RData") # HYENAS
# put na for caterpillar 2013
hyena_matrix[1,42] <- NA
# remove umkawhazaan
sp_data <- hyena_matrix[-17,]
# modificiation of the datat
hm<-sp_data %>% +0.0001%>% log() 
# get number of year and waterhole for the models 
nwt<-nrow(sp_data)
nyear <- ncol(sp_data)
#'
#'
#'Start by the constant model for hyenas
#---------------------------------------
#'Set the model 
#-----------------------------------
modeluconstant_norain <- list(
  Z =  matrix( 1, nrow=nwt, ncol=1), 
  A = "equal")
#' Run the model
#-----------------------------------
fituconstant_norain_hyena <- MARSS(hm, model = modeluconstant_norain, method='kem') # AIC is 
#'save AICc 
hyen_cst_norain <- fituconstant_norain_hyena$AICc
#'
#'
#'Now the time varying model for hyenas 
#------------------------------------------
#'define U
Ut <- array(list(0), dim = c(1, 1 , nyear)) # create the U matrix that depends on time and fills it 
for(i in 1: nyear){ 
  val <- paste( "U" , i) %>% str_remove_all(" ")
  Ut[,,i]<- list(val) 
}
#' set model
modelut_norain <- list(
  Z =  matrix( 1, nwt, ncol=1), 
  U = Ut,#process error   #time varying u
  #R = "diagonal and unequal", 
  A = "equal") #obs error)
#'
#'run model
fitut_norain_hyena <- MARSS(hm, model = modelut_norain, method='BFGS') #
#'
#'save AICc
hyen_ut_norain <- fitut_norain_hyena$AICc
#'
#'
#' Now period for herbivores 
#------------------------------------------
#' define U
U1 <- "U1" %>%as.matrix() 
U2 <- "U2" %>% as.matrix()#period 2
U3 <- "U3" %>% as.matrix()#period 3
U4 <- "U4" %>% as.matrix() # last period
Up <- array(U1, dim = c(dim(U1), nyear )) # construit la matrice B qui depend du temps en la remplissant de B1
Up[16:21] <- U2 # from 1987 to 1992
Up[22:37] <- U3 #from 1993 to 2008
Up[38:nyear]<-U4 # from 2009 to 2020
#38 is 2009 and 36 is 2006
#' set model
modeluperiod_norain <- list(
  Z =  matrix( 1, nwt, ncol=1), 
  U = Up,#process error
  #R = "diagonal and unequal", 
  A = "equal") #obs error)
#'
#' run model
fituperiod_norain_hyena <- MARSS(hm, model = modeluperiod_norain, method='BFGS') ##  AICc is 3405.033
#'
#'save AICc 
hyena_per_norain <- fituperiod_norain_hyena$AICc
#'
#'
#'NOW LOAD RESULTS of the models with the rainfall
load("results/herbivore_models_newrain.RData") #herbivore_models_newrain
load("results/hyena_models_newrain.RData") #hyenas_models_newrain
# AICc for herbivores
herbivore_models_newrain$herb_cst$AICc
herbivore_models_newrain$herb_ut$AICc
herbivore_models_newrain$herb_per$AICc
#AICc for hyenas
hyenas_models_newrain$hyen_cst$AICc
hyenas_models_newrain$hyen_ut$AICc
hyenas_models_newrain$hyen_per$AICc
#'
#'
# now create the dataframe
# for herbivores
Species <- c("prey", "prey", "prey", "hyaena", "hyaena", "hyaena")
Model <- c("Constant", "Time varying", "Period", "Constant", "Time varying", "Period")
No.Rainfall <- c(herb_cst_norain, herb_ut_norain,herb_per_norain ,hyen_cst_norain, hyen_ut_norain, hyena_per_norain )
Rainfall <- c(herbivore_models_newrain$herb_cst$AICc, herbivore_models_newrain$herb_ut$AICc,herbivore_models_newrain$herb_per$AICc, hyenas_models_newrain$hyen_cst$AICc,  hyenas_models_newrain$hyen_ut$AICc,  hyenas_models_newrain$hyen_per$AICc)  

model_selec<-tibble(Species, Model, No.Rainfall,Rainfall ) %>% as.data.frame() ## HERE I HAVE THE VALUES IN THE DATAFRAME
#'
#' What I do now because I can't import the df in R is that I create a blank plot on wich I will paste the table
#----------------------------------
blan<- tibble(time=1:3, est=2:4)
blan_plot <-ggplot(blan, aes(x=time, y=est))+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_blank()) +
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())+
  ggtitle("C")+  theme(plot.title = element_text(size = 24))
#'try again import
pict_table <- image_read("figures/appendices/App_3_rainfall/table_rainfallornot.png")

##################Panel D plot the estimate with rainfall 
#'
#'
#'Load density estimates of herbivores models
load("results/herbivore_estimates_newrain.RData")
#'
#'Choose the one that correspond to the contant model
toplot_herb_rain <- cbind(herbivore_estimates_newrain[[1]], date = 1972:2020)
#'
#'
#' reset the sp_data so it is actually that of herbivore
#----------------------------------
sp_data <- herbivore_matrix[-17,]# - umkhawuazaan
# modificiation of the datat
hm<-sp_data %>% +0.0001%>% log() 
# get number of year and waterhole for the models 
nwt<-nrow(sp_data)
nyear <- ncol(sp_data)
#'
#'
#' PLOT NOW
#----------------------------------
wcov<-toplot_herb_rain %>% 
  ggplot(aes(date, exp(est) )) + 
  geom_ribbon(aes(ymin = exp(conf_low),
                  ymax = exp(conf_up)),    # shadowing cnf intervals
              fill = "black", alpha = 0.2) + 
  geom_line(color = "black", size=1) +
  geom_point( aes(x=date, y=apply((sp_data + 0.00001)  ,2, median, na.rm=TRUE)))+
  #labs(title="Estimation herbivore density constant model with covariate") + 
  labs(x = "Year",  y = "Density of herbivores (indiv/km2)") +
  ggtitle("D")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()) +
  theme(axis.line=element_line()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))+
  geom_vline(xintercept = c(1986,1992, 2008), 
             size=0.3,  linetype=2)+
  annotation_custom(rasterGrob(pict_elep), xmin=1973 , xmax= 1979,ymin=1.40, ymax =1.5)+
  annotation_custom(rasterGrob(pict_elep), xmin=1996 , xmax= 2002,ymin=1.40, ymax =1.5)+
  theme(plot.title = element_text(size = 24))
#geom_smooth(color="mediumslateblue") 

grid.arrange(rain_plot, without_cov, blan_plot, wcov)

#grid.arrange(rain_plot, without_cov, pict_table, wcov) # this one doesn't work
