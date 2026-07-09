############################################################################################################################################################
############################################################################################################################################################
# Cleaning file 
############################################################################################################################################################
############################################################################################################################################################

#############
rm(list=ls())
#############

library(zoo)
library(dplyr)
library(tidyr)
library(lubridate)

###############
# 0. SETTINGS
###############

set.Z     <- 2
data_path <- "~/1_RESEARCH/Financial_JRCSalzburg/Empirical/00_JFEC_[AcceptedPaper]/" 

# Generate Folders
dir.create(paste0(data_path,"Graphs/"))
dir.create(paste0(data_path,"Estimations/"))

###############
# 1. DATA 
###############

start_date <- "2015-12-01" 
end_date   <- "2022-12-01"

######
## 1.1 FINANCIAL DATA
######

Financial_Data              <- read.csv(paste0(data_path,"Data/INPUT/USFiveFactors_2022_incl_ins.csv"))
Financial_Data$year         <- as.Date(strptime(as.character(Financial_Data$year), "%m/%d/%Y"))
Financial_Data              <- Financial_Data %>% filter(year>=start_date) %>% filter(year<=end_date)
Financial_Data$compident    <- as.character(Financial_Data$compident)
Financial_Data              <- Financial_Data %>% mutate(year=substr(Financial_Data$year,1,7)) %>% dplyr::rename(date = year)


Financial_Data <- Financial_Data %>% dplyr::select(c("date","compident","name","ff_10",
                                                     "EXCRET",
                                                     "MKTRF_FF5","SMB_FF5","HML_FF5","RMW_FF5","CMA_FF5","MoM_FF",
                                                     "CO2EquivScope1",
                                                     "CO2tonetsales","netsales","NAICS_Subsector_Code","NAICS_Sector_Code")) 
# Defining sectors 
Financial_Data$NAICS_Sector_Code <- as.character(Financial_Data$NAICS_Sector_Code)
Financial_Data$z_sector          <- rep(NA,nrow(Financial_Data))
Financial_Data$sicsectorown      <- rep(NA,nrow(Financial_Data))

Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "31-33" & Financial_Data$NAICS_Subsector_Code < 319 ]   <- 1
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "31-33" & Financial_Data$NAICS_Subsector_Code > 319  & Financial_Data$NAICS_Subsector_Code < 328 ] <- 2 
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "31-33" & Financial_Data$NAICS_Subsector_Code > 327  & Financial_Data$NAICS_Subsector_Code < 340 ] <- 3
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "23"]     <- 3 #12 old  #3
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "51"]     <- 4
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "44-45"]  <- 5
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "72"]     <- 5 #5
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "48-49"]  <- 9 #7
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "53"]     <- 5 #8 # old5
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "42"]     <- 5 #14
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "22"]     <- 6
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "56"]     <- 7
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "62"]     <- 7#11
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "81"]     <- 7#13
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "71"]     <- 7#15
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "54"]     <- 7 #4
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "21"]     <- 8
Financial_Data$z_sector[Financial_Data$NAICS_Sector_Code == "52"]     <- 10
# 
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "31-33" & Financial_Data$NAICS_Subsector_Code < 319 ]   <- "Manif_1"                                               # Manufacturing 1 Food
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "31-33" & Financial_Data$NAICS_Subsector_Code > 319  & Financial_Data$NAICS_Subsector_Code < 328 ] <- "Manif_2"    # Manufacturing 2 Wood and paper
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "31-33" & Financial_Data$NAICS_Subsector_Code > 327  & Financial_Data$NAICS_Subsector_Code < 340 ] <- "Manif_3"    # Manufacturing 3 Iron and steel
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "23"]     <- "Manif_3"  #"Constr"     # Constr in Manif3
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "51"]     <- "Infor" # information    # Information
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "44-45"]  <- "Retal"                  # Retail trade
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "72"]     <- "Retal" #"Food"          # Accomodation and food services to Retail
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "48-49"]  <- "Trans"                  # Transportation
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "53"]     <- "Retal" #"RealEs"        # Real Estate to Retail
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "42"]     <- "Retal" #"WSales"        # Wholesale
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "22"]     <- "Utils"                  # Utilities
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "56"]     <- "Admin"                  # Other
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "62"]     <- "Admin" #"Healt"         # Other
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "81"]     <- "Admin" #"OServis"       # Other
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "71"]     <- "Admin" #"Recre"         # Other
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "54"]     <- "Admin" # OtherServices  # Other
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "21"]     <- "MinOilGas"              # Mining Oil Gas
Financial_Data$sicsectorown[Financial_Data$NAICS_Sector_Code == "52"]     <- "Insur"                  # Insurance

# https://www.naics.com/search/ 
# Code	Sector Title	
# 11	Agriculture, Forestry, Fishing and Hunting
# 21	Mining	
# 22	Utilities	
# 23	Construction	
# 31-33	Manufacturing	
# 42	Wholesale Trade	
# 44-45	Retail Trade	
# 48-49	Transportation and Warehousing	
# 51	Information	
# 52	Finance and Insurance
# 53	Real Estate Rental and Leasing	
# 54	Professional, Scientific, and Technical Services	
# 55	Management of Companies and Enterprises	
# 56	Administrative and Support and Waste… Services	
# 61	Educational Services	
# 62	Health Care and Social Assistance	
# 71	Arts, Entertainment, and Recreation	
# 72	Accommodation and Food Services
# 81	Other Services (except Public Administration)	
# 92	Public Administration	

# Include companies that are available from 2015D to 2022D
Financial_Data <- Financial_Data %>%
  group_by(compident) %>%
  filter(n() == n_distinct(Financial_Data$date)) %>%
  ungroup()


######
## 1.2 NEWS 
######

NEWS_raw0 <- read.csv(paste0(data_path,"Data/INPUT/DNA_sentiment_US.csv"))
NEWS_raw  <- NEWS_raw0 %>% mutate(date=substr(NEWS_raw0$date,1,7)) %>% group_by(date,topic) %>% summarize(mean_sent = mean(sentiment, na.rm = T),
                                                                                                         sum_sent  = sum(sentiment, na.rm = T),
                                                                                                         volume    = n()) 
NEWS <- NEWS_raw %>%
  select(date, topic, mean_sent) %>%
  filter(topic %in% c("climate change", "natural disaster")) %>%
  spread(topic, mean_sent) %>% dplyr::rename(CC="climate change") %>% dplyr::rename(ND="natural disaster") %>%  na.locf
NEWS       <- NEWS %>% mutate(date=paste0(date,"-01")) %>% mutate(date = as.Date(date)) %>% filter(date > as.Date(start_date)-months(2)) %>% filter(date<=end_date) 
NEWS$dummy <- 0
NEWS$dummy[which(NEWS$date=="2020-05-01"):which(NEWS$date=="2020-12-01")] <- 1
NEWS$CC   <- c(NA,resid(lm(CC ~ lag(CC,1),          data=NEWS)))
NEWS$ND   <- c(NA,resid(lm(ND ~ lag(ND,1) + dummy,  data=NEWS)))
NEWS      <- NEWS %>% filter(date>=start_date) %>% mutate(date=substr(date,1,7))
NEWS      <- NEWS %>% select(date,CC,ND)
X         <- NEWS

###############
# 2. DEFINE VARIABLES
###############

######
## 2.1 EFFECT MODIFIERS
######

# 2.1.1 CO2/NET_Sales *1000000

CO2 <- Financial_Data %>%  dplyr::select(date, compident, CO2EquivScope1) %>% dplyr::group_by(compident) %>% dplyr::filter_at(vars(-compident), all_vars(!is.na(.))) %>%
  dplyr::summarize(mean_CO2 = mean(CO2EquivScope1, na.rm = TRUE), co22=n()) %>% dplyr::ungroup()
CO2 <- subset(CO2, compident != "c881") # this company has only 0: it is eliminated

NS  <- Financial_Data %>%  dplyr::select(date, compident, netsales)       %>% dplyr::group_by(compident) %>% dplyr::filter_at(vars(-compident), all_vars(!is.na(.))) %>%
  dplyr::summarize(mean_NS  = mean(netsales, na.rm = TRUE), nets=n()) %>% dplyr::ungroup()

EM  <- left_join(CO2,NS,by="compident")

EM  <- EM %>% mutate(EM=mean_CO2/mean_NS*1000000)

# Maximum and Minimum values for the Carbon Intensity Scores (CIS) values appearing at session "3.2 Financial and CO2 data"
CIS_min <- summary(EM$EM)[1]
CIS_max <- summary(EM$EM)[6]

# 2.1.2 Sectors
sec        <- Financial_Data %>% dplyr::select(c(compident,z_sector)) %>% distinct()

# 2.1.3 Join two Effect Modifiers
Z          <- left_join(EM,sec,by="compident") %>% dplyr::select(EM,z_sector,compident) %>% dplyr::rename(sector=z_sector)


######
## 2.2 EXCESS RETURNS
######

R_0  <- Financial_Data %>% 
  select(date, compident, EXCRET) %>%
  spread(key = compident, value = EXCRET) 

R_0  <- R_0 %>%  mutate_all(~ na.locf(., fromLast = TRUE)) # if the first value is NA copy the second as first

compident_s <- Z$compident
R           <- R_0 %>% select(date, all_of(compident_s))


######
## 2.3 CONTROLS (Fama and French factors+Momentum)
######

F  <- Financial_Data %>% select(c(date,MKTRF_FF5,SMB_FF5,HML_FF5,RMW_FF5,CMA_FF5,MoM_FF)) %>% distinct()

###############
# 3. CREATE DATASETS
###############

DATA        <- list(R, F, Z, X, Financial_Data)
names(DATA) <- c("R","F","Z","X","Financial_Data")

save(DATA, file=paste0(data_path, "Data/OUTPUT/Financial_Data_2015D-2022D.RData"))


#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
