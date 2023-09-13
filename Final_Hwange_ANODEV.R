## Final file of Hwange Anodev

#'load packages 
#-------------------------
library(funtimes)
library(tidyverse)
library(MARSS)
require(zoo)
#'
#'
#'Load data
#------------------------
## Density estimates of the model
load("results/herbivores_estimates_newrain.RData")
load("results/hyena_estimates_newrain.RData")
## Model results : 
load("results/herbivore_models_newrain.RData")
load("results/hyena_models_newrain.RData")
## Arrange data : get shorter names 
herbivore_models <- herbivore_models_newrain
hyenas_models <- hyenas_models_newrain
#'
#'
#'
#' A : Exctract Log Lik Values 
#---------------------------------
#herbivore_models
herbivore_models$herb_cst$logLik
herbivore_models$herb_ut$logLik
herbivore_models$herb_per$logLik
#hyenas_models
hyenas_models$hyen_cst$logLik
hyenas_models$hyen_ut$logLik
hyenas_models$hyen_per$logLik

#' B : Exctract AICc Values 
#---------------------------------
# herbivore_models
herbivore_models$herb_cst$AICc
herbivore_models$herb_ut$AICc
herbivore_models$herb_per$AICc
#hyenas_models$
hyenas_models$hyen_cst$AICc
hyenas_models$hyen_ut$AICc
hyenas_models$hyen_per$AICc


##########################################################################################################################################################################
### C - ANODEV --
##########################################################################################################################################################################

#get number of parameters 
npar_constant <-6 #(1 A, 1x0, 1Q, 17R, 1U, 1D)
npar_period <- 9 #(1 A, 1x0, 1Q, 17R, 4U, 1D)
npar_timevar <- 54 #(1 A, 1x0, 1Q, 17R, 49U, 1D)
# create a list with the number of parameters
par<- list(npar_constant=npar_constant,npar_period=npar_period, npar_timevar=npar_timevar )

# Get n1 and n2 to do the ANODEVs
n1 <- npar_period - npar_constant
n2 <- npar_timevar - npar_period # required to calculate ANODEVs
# stock this in the list
ns <- list(n1=n1, n2=n2)


###############################
#### For herbivores
###############################

# get deviance : -2log(likelihood)
dev_cst_herb<- -2 * herbivore_models$herb_cst$logLik
dev_per_herb<- -2 * herbivore_models$herb_per$logLik
dev_timevar_herb <- -2 * herbivore_models$herb_ut$logLik

devs_herb <- list(dev_cst_herb=dev_cst_herb, dev_per_herb=dev_per_herb,dev_timevar_herb=dev_timevar_herb )

X_herb <- ((dev_cst_herb - dev_per_herb) / n1) / ((dev_per_herb-dev_timevar_herb)/n2)
f_herb<-1-pf(X_herb,n1,n2)
r_herb<-(dev_cst_herb - dev_per_herb) / (dev_cst_herb - dev_timevar_herb)

#list for the results for preys
results_anodev_herb <- list(devs_herb = devs_herb, X_herb=X_herb, f_herb=f_herb,r_herb=r_herb) 

###############################
#### For hyenas 
###############################

# get deviance : -2log(likelihood)
dev_cst_hyen<- -2 * hyenas_models$hyen_cst$logLik
dev_per_hyen<- -2 * hyenas_models$hyen_per$logLik
dev_timevar_hyen <- -2 * hyenas_models$hyen_ut$logLik

devs_hyen <- list(dev_cst_hyen= dev_cst_hyen, dev_per_hyen=dev_per_hyen, dev_timevar_hyen=dev_timevar_hyen )
# get anodev results 
n1 <- npar_period - npar_constant
n2 <- npar_timevar - npar_period


X_hyen <- ((dev_cst_hyen - dev_per_hyen) / n1) / ((dev_per_hyen-dev_timevar_hyen)/n2)
f_hyen<-1-pf(X_hyen,n1,n2)
r_hyen<-(dev_cst_hyen - dev_per_hyen) / (dev_cst_hyen - dev_timevar_hyen)

# list of the results for hyanea
results_anodev_hyen <- list(devs_hyen = devs_hyen, X_hyen=X_hyen, f_hyen=f_hyen,r_hyen=r_hyen) 

# create a list with all the results
results_anodevs<- list(par=par, ns=ns,  results_anodev_herb=results_anodev_herb, results_anodev_hyen=results_anodev_hyen)


save(results_anodevs, file = "results/results_anodevs")




