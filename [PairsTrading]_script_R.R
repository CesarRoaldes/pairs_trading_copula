library(tidyverse)
library(ggplot2)
library(copula)
library(VineCopula)

setwd("C:/Users/cesar/Desktop/partage_windows/FINAL Copule/code/output/")


################
#   IMPORTATION DES DONNES
################

importation_top_trading <- function(nb){
  a = 1
  for (i in 1:nb) {
    nam <- paste0("pf",nb,"_n",a)
    donnees <- read_csv(paste0("pf",nb,"_n",a,".csv"))
    assign(nam, donnees, envir = parent.frame())
    a <- a+1
  }
}

importation_top_trading(5)
importation_top_trading(20)
importation_top_trading(100)

importation_top_formation <- function(nb){
  a = 1
  for (i in 1:nb) {
    nam <- paste0("pf", nb, "_formation_n",a)
    donnees <- read_csv(paste0("pf", nb, "_formation_n",a,".csv"))
    assign(nam, donnees, envir = parent.frame())
    a <- a+1
  }
}

importation_top_formation(5)
importation_top_formation(20)
importation_top_formation(100)


################
#   MISE EN PLACE DE LA STRATEGIE
################

copula_strategie <- function(nb){
  a = 1
  for(i in 1:nb) { 
    nam1 <- paste0("pf", nb, "_formation_n",a)
    donnees_formation <- get(nam1)
    
    date_index = donnees_formation[,1]
    
    # Calcul des rendements
    Rx = (pull(donnees_formation[,2])-lag(pull(donnees_formation[,2]), 1))/lag(pull(donnees_formation[,2]), 1)
    Ry = (pull(donnees_formation[,3])-lag(pull(donnees_formation[,3]), 1))/lag(pull(donnees_formation[,3]), 1)
    
    df_formation = cbind.data.frame(Rx, Ry)
    df_formation = drop_na(df_formation)
    
    
    #Estimation empirique des distributions marginales des returns : Fx(Rx) et Fy(Ry)
    Fx_est = ecdf(Rx)
    Fy_est = ecdf(Ry)
    
    #Estimation de la copule :
    #1- transformation quantile de nos rendements
    u <- pobs(as.matrix(cbind(df_formation$Rx,df_formation$Ry)))[,1]
    v <- pobs(as.matrix(cbind(df_formation$Rx,df_formation$Ry)))[,2]
    #2- à l'aide de la fonction BicCopSelect on estime la meilleure copule
    cop_pair <- BiCopSelect(u,v,familyset=NA)
    famille = cop_pair$family
    param = cop_pair$par
    param2 = cop_pair$par2
    
    # Trading periodes :
    nam2 <- paste0("pf",nb,"_n",a)
    donnees_trading <- get(nam2)
    Rx = (pull(donnees_trading[,2])-lag(pull(donnees_trading[,2]), 1))/lag(pull(donnees_trading[,2]), 1)
    Ry = (pull(donnees_trading[,3])-lag(pull(donnees_trading[,3]), 1))/lag(pull(donnees_trading[,3]), 1)
    donnees_trading = donnees_trading %>% mutate(Fx = Fx_est(Rx)) %>% 
      mutate(Fy = Fy_est(Ry)) %>%
      mutate(MI_X = BiCopHfunc2(Fx, Fy, famille, par=param, par2=param2)) %>%
      mutate(MI_Y = BiCopHfunc1(Fx, Fy, famille, par=param, par2=param2))
    
    assign(nam2, donnees_trading)
    write.csv(donnees_trading, paste0(nam2, ".csv"))
    a <- a+1
  }
}

copula_strategie(5)
copula_strategie(20)
copula_strategie(100)

