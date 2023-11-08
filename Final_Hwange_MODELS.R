## FINAL model Hwange 
# 16/01/2023


rm(list = ls())

#'Load packages 
#----------------------
library(tidyverse)
library(MARSS)
library(gridExtra)
library(ggplot2)
library(ade4)
#'
#'
#'Load data
#----------------------
## ANNUAL RAINFALL
load("data/annual_rainfall.RData") # annual_rain
# arrange the covariate in a table, zscore it and log it   
table_rain <- tibble(date = 1972 : 2020, rainfall=0)
for (i in 1:length(annual_rain)){ table_rain[i, 2]<- annual_rain[[i]][1] }
table_rainfall <- table_rain %>% dplyr::select(-date)%>% t() %>% log()%>% zscore()
#'
#' MATRIX DATA 
load("data/matrix_tess_herbivore_ponderate.RData") # HERBIVORES 
load("data/matrix_tess_hyena_ponderate.RData") # HYENAS
#'
#'
#'
#'ARRANGE DATA (run the line depending on if the analysis is being run for herbivores or hyenas)
#--------------------------------------
# put na for caterpillar 2013
herbivore_matrix[1,42] <- NA
hyena_matrix[1,42] <- NA
# remove umkawhazaan(hyaena dens are present)
sp_data <- herbivore_matrix[-17,]# - umkhawuazaan
# or
sp_data <- hyena_matrix[-17,]
# modificiation of the datat
hm<-sp_data %>% +0.0001%>% log() 
#'
#'
#'FOR THE ANALYSIS
#--------------------------------------
# get number of year and waterhole for the models 
nwt<-nrow(sp_data)
nyear <- ncol(sp_data)


######################### First model with constant U
modeluconstant <- list(
  Z =  matrix( 1, nrow=nwt, ncol=1), 
  A = "equal", 
  D = "equal", 
  d = table_rainfall)

# fit MARSS model
fituconstant <- MARSS(hm, model = modeluconstant, method='kem') # AIC is 

#get parameters values
para_u_cst<-MARSSparamCIs(fituconstant, method="parametric", nboot)#estimation

# Get density estimates
esti_herb_cst<-tsSmooth(fituconstant , interval =  "confidence")
colnames(esti_herb_cst)<- c("rownames", "time", "est", "se", "conf_low", "conf_up" )

#PLOT ESTIMATES
## when estimate are not exp
esti_herb_cst %>% 
  ggplot(aes(time, est)) + 
  geom_ribbon(aes(ymin = conf_low,
                  ymax = conf_up),    # shadowing cnf intervals
              fill = "steelblue2") + 
  geom_line(color = "firebrick",
            size=1) 

# after back transformation
esti_herb_cst %>% 
  ggplot(aes(time, exp(est) )) + 
  geom_ribbon(aes(ymin = exp(conf_low),
                  ymax = exp(conf_up)),    # shadowing cnf intervals
              fill = "steelblue2") + 
  geom_line(color = "firebrick", size=1) +
  geom_point( aes(x=time, y=apply((sp_data )  ,2, median, na.rm=TRUE)))



################################ Time varying 
Ut <- array(list(0), dim = c(1, 1 , nyear)) # create the U matrix that depends on time and fills it 
for(i in 1: nyear){ 
  val <- paste( "U" , i) %>% str_remove_all(" ")
  Ut[,,i]<- list(val) 
}

modelut <- list(
  Z =  matrix( 1, nwt, ncol=1), 
  U = Ut,#process error   #time varying u
  A = "equal",
  D = "equal", 
  d = table_rainfall) #obs error)

# fit MARSS model
fitut <- MARSS(hm, model = modelut, method='BFGS') #

# get parameters values 
para_ut<-MARSSparamCIs(fitut, method = "parametric", nboot = 1000)

#density estimates
esti_herb_ut<-tsSmooth(fitut , interval =  "confidence")
colnames(esti_herb_ut)<- c("rownames", "time", "est", "se", "conf_low", "conf_up" )

# plot estimates on log scale
esti_herb_ut%>% 
  ggplot(aes(time, est)) + 
  geom_ribbon(aes(ymin = conf_low,
                  ymax = conf_up),    # shadowing cnf intervals
              fill = "steelblue2") + 
  geom_line(color = "firebrick",
            size=1) 

# after back transformation
esti_herb_ut %>% 
  ggplot(aes(time, exp(est) )) + 
  geom_ribbon(aes(ymin = exp(conf_low),
                  ymax = exp(conf_up)),    # shadowing cnf intervals
              fill = "steelblue2") + 
  geom_line(color = "firebrick", size=1) +
  geom_point( aes(x=time, y=apply((sp_data + 0.00001)  ,2, median, na.rm=TRUE)))


##################### Third model with a period varying U (4 periods)
U1 <- "U1" %>%as.matrix() 
U2 <- "U2" %>% as.matrix()#period 2
U3 <- "U3" %>% as.matrix()#period 3
U4 <- "U4" %>% as.matrix() # last period
Up <- array(U1, dim = c(dim(U1), nyear )) # construit la matrice B qui depend du temps en la remplissant de B1
Up[16:21] <- U2 # from 1987 to 1992
Up[22:37] <- U3 #from 1993 to 2008
Up[38:nyear]<-U4 # from 2009 to 2020


modeluperiod <- list(
  Z =  matrix( 1, nwt, ncol=1), 
  #time varying u
  U = Up,#process error
  A = "equal",
  D = "equal", 
  d = table_rainfall) #obs error)

# fit MARSS model
fituperiod <- MARSS(hm, model = modeluperiod, method='BFGS') ##  AICc is 3405.033

# get parameter estimates 
para_per<-MARSSparamCIs(fituperiod, method="parametric", nboot = 1000)

# density estimates
esti_herb_per<-tsSmooth(fituperiod , interval =  "confidence")
colnames(esti_herb_per)<- c("rownames", "time", "est", "se", "conf_low", "conf_up" )

# plot it on the log scale 
esti_herb_per%>% 
  ggplot(aes(time, est)) + 
  geom_ribbon(aes(ymin = conf_low,
                  ymax = conf_up),    # shadowing cnf intervals
              fill = "steelblue2") + 
  geom_line(color = "firebrick",
            size=1)

# after back transformation
esti_herb_per %>% 
  ggplot(aes(time, exp(est) )) + 
  geom_ribbon(aes(ymin = exp(conf_low),
                  ymax = exp(conf_up)),    # shadowing cnf intervals
              fill = "steelblue2") + 
  geom_line(color = "firebrick", size=1) +
  geom_point( aes(x=time, y=apply((sp_data + 0.0001)  ,2, median, na.rm=TRUE)))


## SAve parameters values*
#-----------------------------
para_per_herb <- para_per
#save(para_per, file = "parameters_herbivores_period.RData")
para_per_hyen <- para_per
#save(para_per_hyen, file = "parameters_hyena_period.RData")


parameters_ci <- list(herb= para_per_herb, hyen = para_per_hyen)
#save(parameters_ci, file = "results/parameters_periodmodel.RData")



#################################################################################################################

#### SAVE MODELS
#herbivores 
herbivore_models_newrain <- list(herb_cst = fituconstant, herb_ut = fitut, herb_per = fituperiod)
herbivore_estimates_newrain <- list( esti_herb_cst, esti_herb_ut, esti_herb_per)
herbivore_params <- list( para_u_cst, para_ut, para_per)

#save(herbivore_models_newrain, file = "results/herbivore_models_newrain.RData")
#save(herbivore_estimates_newrain, file = "results/herbivore_estimates_newrain.RData")
#save(herbivore_params, file = "results_ponderate/uniq_cov/herbivore_params_cum_Rdiff.RData")

## hyenas 
hyenas_models_newrain <- list(hyen_cst = fituconstant, hyen_ut = fitut, hyen_per = fituperiod)
hyena_estimates_newrain <- list( esti_hyen_cst = esti_herb_cst, esti_hyen_ut = esti_herb_ut, esti_hyen_per = esti_herb_per)
hyena_params <- list(para_u_cst, para_ut, para_per)

#save(hyenas_models_newrain, file = "results_ponderate/uniq_cov/hyena_models_newrain.RData")
#save(hyena_estimates_newrain, file = "results_ponderate/uniq_cov/hyena_estimates_newrain.RData")
#save(hyena_params, file = "results_ponderate/uniq_cov/hyena_params_cum_Rdiff.RData")



#######################################################" CODE to check residuals 
MARSSresiduals(fituconstant)$model.residuals
plot(residuals(fituconstant))
plot(MARSSresiduals(fituconstant)$model.residuals)
plot(MARSSresiduals(fituconstant))
model.residuals(fituconstant)

