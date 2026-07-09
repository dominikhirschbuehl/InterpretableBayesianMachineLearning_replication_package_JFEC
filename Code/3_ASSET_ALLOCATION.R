#############
rm(list=ls())
#############
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(DescTools)
library(PeerPerformance)
#################################################################################
# Auxiliary Functions
#################################################################################

get.MinumumVarianceWeights <- function(lambda.f,Phi,lambda.q,Omega,VarF,VarX,nR){
  Sigma<- (lambda.f%*%VarF)%*%t(lambda.f)+ VarX*(Phi)%*%t(Phi) + lambda.q%*%t(lambda.q) + diag(Omega)
  ones<-as.matrix(rep(1,nR))
  suminvSigma<- t(solve(Sigma,ones))
  wp<- suminvSigma/sum(suminvSigma)
  return(wp)
}

get.MinumumVarianceWeightsLin <- function(lambda.f,lambda.q,Omega,VarF,nR){
  Sigma<- (lambda.f%*%VarF)%*%t(lambda.f) + lambda.q%*%t(lambda.q) + diag(Omega)
  ones<-as.matrix(rep(1,nR))
  suminvSigma<- t(solve(Sigma,ones))
  wp<- suminvSigma/sum(suminvSigma)
  return(wp)
}

DRisk<-function(x){
  mean(x^2*(x<=0))
}

###############################################################################
# Libraries
#################################################################################

library(invgamma)
library(zoo)
library(dplyr)
library(magrittr)
library(plyr)

#################################
## 0. PATHS 
#################################
localpath<-"C:/Users/loren/Dropbox/ClimateRisk/Accepted/00_JFEC_[AcceptedPaper]/00_JFEC_[AcceptedPaper]/"
directory <- paste0(localpath)
data_path <- paste0(localpath,"Data/OUTPUT/") 
save_path <- paste0(localpath,"Estimations/")


#################################
## 0. SETTINGS
#################################

# Sample considered
start_month        <- "2015-12"
end_month          <- "2022-12"

# Demean Returns
demean_option      <- "TRUE"

# Quantile Selection News indicator
quantilesN_option  <- "FALSE"
quantiles          <- c(0.10, 0.90) 


#################################
## 1. ARRANGING DATA
#################################

####
# 1.1 Loading Financial Data and put in a good format
####

load(file=paste0(data_path, "Financial_Data_2015D-2022D.RData"))
Financial_Data <- DATA$Financial_Data

R0       <- as.matrix(DATA$R[,-1])
R        <-R0
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


VarF<-var(F)
VarX<-as.numeric(var(X_ND))
XX<-cbind(as.matrix(X_ND),F)
nX<-ncol(XX)
VarXX<-var(XX)
nR<-ncol(R)


#################################
## 2. Allocations
#################################


###
#2.1 Equally Weighted Allocation
###

wEW<-matrix(1/nR,nrow=nR,ncol=1)


###
#2.2 Linear  OLS Allocation
###

#Linear OLS factor model Esimation
if (demean_option == "TRUE"){
  R <- scale(R, center = TRUE, scale = FALSE)
}
start.time <- Sys.time()
 DS_NDlin_OLS  <-   lm(R~0+XX)
 time_diff_NDlin_OLS <- difftime(Sys.time(), start.time, units='mins')
 print(time_diff_NDlin_OLS)


lambda.f<-t(coef(DS_NDlin_OLS))
Omega<-diag(var(resid(DS_NDlin_OLS)))
wpOLS <-t(get.MinumumVarianceWeightsLin(lambda.f,matrix(0,c(nR,1)),Omega,VarXX,nR))

###
#2.3 BART  Allocations
###
load(paste0(save_path,"RESULTS_CC_ND_20000.RData"))
attach(ANALYSIS$DS_ND_d_M)
nrep<- nrow(Phi.store)
wp<-matrix(NA,nrow=nrep,ncol=nR)
Deltawp<-matrix(NA,nrow=nrep,ncol=nR)
for(rep in 1:nrep){
  Phi<-drop(Phi.store[rep,,])
  lambda.f<-drop(lambda.f.store[rep,,])
  lambda.q<-drop(lambda.q.store[rep,,])
  Omega<-drop(Omega.store[rep,])
  wp[rep,] <-get.MinumumVarianceWeights(lambda.f,Phi,lambda.q,Omega,VarF,VarX,nR)
  print(rep/nrep)
}
detach(ANALYSIS$DS_ND_d_M)

wMedianw<-apply(wp,2,median)
wMedianwnof<-apply(wp,2,median)









#################################
#4 Portfolio Returns
##################################
#Use R0 non winsorized returns
rp<-NULL
rp$NDBartmdianw<-R0%*%wMedianw
rp$NDLinOLSw<-R0%*%wpOLS
rp$EW<- R0%*%wEW

###
#4.1 Portfolio Performance Table (Table 2)
###

pTable<- rbind(as.data.frame(lapply(rp,mean)),as.data.frame(lapply(rp,sd))
               ,as.data.frame(lapply(rp,function(x){(mean(x))/sd(x)})), as.data.frame(lapply(rp,function(x){(mean(x))/sqrt(DRisk(x))})))

rownames(pTable)<-c("Mean", "S.D.","Sharpe Ratio" ,"Sortino Ratio" )
print(pTable)


###
#4.2 Cumulative Return Plot (figure 8)
###
crp<- data.frame(BART=cumprod(1+rp$NDBartmdianw),OLS=cumprod(1+rp$NDLinOLSw),EW=cumprod(1+rp$EW),date=as.Date(paste0(DATA$R$date,"-01")))
library(xts)
xts_crp <- xts(crp[, c("BART", "OLS", "EW")], order.by = crp$date)
cumretplot<-plot(xts_crp, main="Cumulative Portfolio Returns", col=2:4,lty=1:3)
addLegend("topleft",names(xts_crp),fill = 2:5,
          bty="n")
pdf(file = paste0("CumRet.pdf"),paper='special')
print(cumretplot)
dev.off()

###
#4.3 Sectorial Allocation (Table 3)
###

names_sec       <- c("Manufacturing - 1","Manufacturing - 2","Manufacturing - 3","Information","Retail","Utilities","Others","Mining and Oil","Transportation","Insurance")

#Allocation with BART Model
Z_av    <- DATA$Z   
Z_av[,1]<- log(Z_av[,1])
intervals_w       <- apply(wp,     c(2), function(x) quantile(x, c(0.16, 0.5, 0.84)))
intervals_w       <- t(intervals_w)
colnames(intervals_w)     <- c("Q_16" , "Q_50" , "Q_84")
intervals       <- data.frame(compident = Z_av$compident,intervals_w)
intervals                <- intervals[order(intervals$Q_50),] 
intervals                <- left_join(intervals,Z_av,by="compident")
intervals$sector_name    <- names_sec[intervals$sector]
sectorial_avg            <- intervals %>%  group_by(sector_name) %>% dplyr::summarise(BART = sum(Q_50, na.rm = TRUE))
intervals                <- intervals %>%  left_join(sectorial_avg, by = "sector_name") %>% arrange(sector)


#Allocation with EW
intervals_w       <- matrix(wEW, nrow = length(wEW), ncol = 3)
colnames(intervals_w)     <- c("Q_16" , "Q_50" , "Q_84")
intervals       <- data.frame(compident = Z_av$compident,intervals_w)
intervals                <- intervals[order(intervals$Q_50),] 
intervals                <- left_join(intervals,Z_av,by="compident")
intervals$sector_name    <- names_sec[intervals$sector]
sectorial_avg_EW            <- intervals %>%  group_by(sector_name) %>% dplyr::summarise(EW = sum(Q_50, na.rm = TRUE))


#Allocation with OLS
intervals_w       <- matrix(wpOLS, nrow = length(wEW), ncol = 3)
colnames(intervals_w)     <- c("Q_16" , "Q_50" , "Q_84")
intervals       <- data.frame(compident = Z_av$compident,intervals_w)
intervals                <- intervals[order(intervals$Q_50),] 
intervals                <- left_join(intervals,Z_av,by="compident")
intervals$sector_name    <- names_sec[intervals$sector]
sectorial_avg_OLS            <- intervals %>%  group_by(sector_name) %>% dplyr::summarise(OLS = sum(Q_50, na.rm = TRUE))




#Average Sector Allocation Table

WTable<- sectorial_avg
WTable<-WTable%>%left_join(sectorial_avg_OLS, by = "sector_name") 
WTable<-WTable%>%left_join(sectorial_avg_EW, by = "sector_name") 

print(WTable)



