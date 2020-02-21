library(tidyverse)
library(ggplot2)
library(copula)
library(VineCopula)

setwd("E:/FINAL Copule/code/output")

importation_top_formation <- function(nb){
  a = 1
  for (i in 1:nb) {
    nam <- paste0("pf", nb, "_formation_n",a)
    donnees <- read_csv(paste0("pf", nb, "_formation_n",a,".csv"))
    assign(nam, donnees, envir = parent.frame())
    a <- a+1
  }
}

importation_top_trading <- function(nb){
  a = 1
  for (i in 1:nb) {
    nam <- paste0("pf",nb,"_n",a)
    donnees <- read_csv(paste0("pf",nb,"_n",a,".csv"))
    assign(nam, donnees, envir = parent.frame())
    a <- a+1
  }
}

importation_top_trading(100)
importation_top_formation(100)

pf100_formation_n96

r_x = pf100_formation_n96$SIRI/lag(pf100_formation_n96$SIRI, 1)-1
r_y = pf100_formation_n96$KEM/lag(pf100_formation_n96$KEM, 1)-1
r_x = r_x[-1]
r_y = r_y[-1]

ks.test(x = r_x, y = pnorm)
ks.test(x = r_y, y = pnorm)
ks.test(x = r_y, y = r_x)

out1 <- capture.output(ks.test(x = r_x, y = pnorm))
out2 <- capture.output(ks.test(x = r_y, y = pnorm))
out3 <- capture.output(ks.test(x = r_y, y = r_x))
cat(out1, file="KS_test_SIRI-norm.txt", sep="\n", append=TRUE)
cat(out2, file="KS_test_KEM-norm.txt", sep="\n", append=TRUE)
cat(out3, file="KS_test_SIRI-KEM.txt", sep="\n", append=TRUE)

u <- pobs(as.matrix(cbind(r_x,r_y)))[,1]
v <- pobs(as.matrix(cbind(r_x,r_y)))[,2]

plot(r_x, r_y, xlim = c(min(r_x), max(r_x)), ylim = c(min(r_y), max(r_y)), xlab="Rendements de SIRI", ylab="Rendements de KEM", pch = 20) 
plot(u, v, xlim = c(0, 1), ylim = c(0, 1), xlab="Quantiles des rendements de SIRI", ylab="Quantiles des rendements de KEM", pch = 20) 


copule <- BiCopSelect(u,v,familyset=NA)
famille = copule$family
param = copule$par
param2 = copule$par2

plot(copule, xlab="u", ylab="v")
contour(copule)
summary(copule)

out_summary <- capture.output(summary(copule))
cat(out_summary, file="summary_copule_SIRI-KEM.txt", sep="\n", append=TRUE)


BiCopEst(u, v, family=2, method = "mle")


plot(BiCopHfunc2(u, v, famille, par=param, par2=param2))

plot(BiCopPar2Tau(family=2, par=copule$par, par2 = copule$par2, obj = as.matrix(cbind(r_x,r_y)), check.pars = TRUE))














