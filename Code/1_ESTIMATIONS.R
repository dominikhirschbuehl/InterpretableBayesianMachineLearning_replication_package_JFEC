#############
rm(list=ls())
#############

#require(dbarts)
library(dbarts)
library(invgamma)
library(zoo)
library(dplyr)
library(foreach)
library(doParallel)
library(doSNOW)
library(dplyr)
library(magrittr)
library(gridExtra)
library(ggplot2)
library(plyr)
library(ggrepel)
library(ggpubr)
library(forecast)
library(DescTools)

#################################
## 0. PATHS 
#################################

directory <- "~/1_RESEARCH/Financial_JRCSalzburg/Empirical/00_JFEC_[AcceptedPaper]/"
data_path <- "~/1_RESEARCH/Financial_JRCSalzburg/Empirical/00_JFEC_[AcceptedPaper]/Data/OUTPUT/" 
save_path <- "~/1_RESEARCH/Financial_JRCSalzburg/Empirical/00_JFEC_[AcceptedPaper]/Estimations/"

# Functions
source(paste0(directory,"Code/FUNCTIONS.R")) 

#################################
## 0. SETTINGS
#################################

# Sample considered
start_month        <- "2015-12"
end_month          <- "2022-12"

# Demean Returns
demean_option      <- "TRUE"

# BART Estimations Settings
nsave              <- 20000  #number of saved draws
nburn              <- 20000  #number of burn-ins

h          <- 1           # horizon (yt+h)
Q.q        <- 1           # number of latent factors
set.Z      <- 2           # Select the z which we would like to vary
set.sector <- 10          # Select the sector which we would like to vary 
set.grid.Z <- 250         #Number of grid points for which we compute the scenarios
num.trees  <- 350         # number of trees in BART

cgm.exp    <- 2           # CGM prior that penalizes complex trees
cgm.level  <- 0.95
sd.mu      <- 2


#################################
## 1. ARRANGING DATA
#################################

####
# 1.1 Loading Financial Data and put in a good format
####

load(file=paste0(data_path, "Financial_Data_2015D-2022D.RData"))
Financial_Data <- DATA$Financial_Data

R       <- as.matrix(DATA$R[,-1])
X_CC    <- as.matrix(DATA$X$CC)
X_ND    <- as.matrix(DATA$X$ND)
F       <- as.matrix(DATA$F[,-1])
Z       <- as.matrix(DATA$Z[,-3])  # CO2 + sector
Z[,1]   <- log(Z[,1])

rownames(R)    <- DATA$R$date
rownames(X_CC) <- DATA$X$date
rownames(X_ND) <- DATA$X$date
rownames(F)    <- DATA$F$date
rownames(Z)    <- DATA$Z$compident

####
# 1.2 WINSORIZE EXCESS RETURNS
####

results <- list()

R_rep <- matrix(NA, ncol=ncol(R), nrow=nrow(R))
for (i in c(1:nrow(R))){
  series       <- R[i,]
  R_rep[i,]    <- Winsorize(series, val = quantile(series, probs = c(0.01, 0.99), na.rm = FALSE))
}
colnames(R_rep) <- colnames(R)
rownames(R_rep) <- rownames(R)

R <- R_rep  

####
# 1.3 DEMEANING RETURNS
####

if (demean_option == "TRUE"){
  R <- scale(R, center = TRUE, scale = FALSE)
}


#################################
## 2. ESTIMATIONS BART MODEL 
#################################

#######
## 2.1 Climate Change (FF+Momentum)
#######

### Estimation
set.seed(12345)
start.time <- Sys.time()
DS_CC_M  <-   Big_BART(R,X_CC,F,Z,
                       nsave,nburn,num.trees,set.sector,set.grid.Z,set.Z,cgm.exp,cgm.level,sd.mu,Q.q)
time_diff_CC_M <- difftime(Sys.time(), start.time, units='mins')
print(time_diff_CC_M)

#######
## 2.2 Natural Disaster (FF+Momentum+COVID dummy)
#######

### Estimation
set.seed(12345)
start.time <- Sys.time()
DS_ND_d_M  <-   Big_BART(R,X_ND,F,Z,
                         nsave,nburn,num.trees,set.sector,set.grid.Z,set.Z,cgm.exp,cgm.level,sd.mu,Q.q)
time_diff_ND_d_M <- difftime(Sys.time(), start.time, units='mins')
print(time_diff_ND_d_M)

#################################
## 3. SAVE RESULTS
#################################

ANALYSIS           <- list(DS_CC_M,DS_ND_d_M)
names(ANALYSIS)    <- c("DS_CC_M","DS_ND_d_M")
save(ANALYSIS,      file=paste0(save_path,"RESULTS_CC_ND_",nsave,".RData"))  


