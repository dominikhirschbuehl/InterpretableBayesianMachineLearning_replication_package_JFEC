#############
rm(list=ls())
#############

library(zoo)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(tidyr)

# You select the analysis for eithr CLIMATE CHANGE or NATURAL DISASTERS: names_j
# "DS_CC_M"  : Climate Change
# "DS_ND_d_M": Natural Disaster
names_j   <- "DS_ND_d_M" 

#################################
## 0. SETTINGS
#################################

directory       <- "~/1_RESEARCH/Financial_JRCSalzburg/Empirical/00_JFEC_[AcceptedPaper]/" # "~/B1_Salzburg/JFEC_replication_file_climate_finance/"
estim_path      <- paste0(directory,"Estimations/")
data_path       <- paste0(directory,"Data/")

save_graph_path <- paste0(directory,"Graphs/")

# Load Data
NEWS_raw0 <- read.csv(paste0(data_path,"INPUT/DNA_sentiment_US.csv"))
load(paste0(data_path,"OUTPUT/Financial_Data_2015D-2022D.RData"))
load(paste0(estim_path,"RESULTS_CC_ND_20000.RData"))

# Sectors names
names_sec       <- c("Manufacturing - 1","Manufacturing - 2","Manufacturing - 3","Information","Retail","Utilities","Others","Mining and Oil","Transportation","Insurance")

# Settings for the Graphs
size_text_xy    <- 20  # 20
size_text_leg   <- 18  # 14
size_text_leg1  <- 1   # 1
size_text_xy_overall    <- 40  # 20
size_text_leg_overall   <- 30  # 14
size_text_leg1_overall  <- 1   # 1
chart_width     <- 18
chart_height    <- 10 
chart_width_overall     <- 16
chart_height_overall    <- 10
size_text_axis_LambdaOverall <- 40
size_text_axis_labels  <- 23
size_text_axis_labels_phi <- size_text_axis_labels+6
ymin_value <- -0.8
ymax_value <- 0.5
ymin_value_dib <- 0
ymax_value_dib <- 0.025
ymin_value <- -0.8
ymax_value <- 0.5
ymin_value_dib <- 0
ymax_value_dib <- 0.025

  
all_vars      <- ls()
matching_vars <- all_vars[grep("^ANALYSIS", all_vars)]
ANALYSIS0     <- get(matching_vars)
rm(list = matching_vars, envir = .GlobalEnv)
  
Financial_Data <- DATA$Financial_Data
  
Z       <- DATA$Z
Z[,1]   <- log(Z[,1])
  
# Select Natural Disaster or Climate Change Analysis

ANALYSIS  <- ANALYSIS0[[names_j]]
repl      <- nrow(ANALYSIS$Phi.store)
    
##########################################################
# 1.  ...some preparation...
##########################################################

phi                      <- ANALYSIS$Phi.store
intervals_phi            <- apply(phi,     c(2,3), function(x) quantile(x, c(0.16, 0.5, 0.84)))
intervals_phi            <- t(intervals_phi[,,1])
colnames(intervals_phi)  <- c("Q_16" , "Q_50" , "Q_84")
intervals_phi            <- data.frame(compident = Z$compident,intervals_phi)
intervals                <- intervals_phi[order(intervals_phi$Q_50), ] 
intervals                <- left_join(intervals,Z,by="compident")
intervals$sector_name    <- names_sec[intervals$sector]
sectorial_avg            <- intervals %>%  group_by(sector_name) %>% summarise(avg = mean(Q_50, na.rm = TRUE))
intervals                <- intervals %>%  left_join(sectorial_avg, by = "sector_name") %>% arrange(sector)

#########
# 1.1 Comment of Figure 3 at the section "4.1 Firm-level impact of climate change and natural disasters"
#########
counting       <- intervals %>% mutate(negative_sign = ifelse(Q_16 < 0 & Q_50 < 0 & Q_84 < 0,  1, 0))
counting       <- counting  %>% mutate(positive_sign = ifelse(Q_16 > 0 & Q_50 > 0 & Q_84 > 0, +1, 0))
negative_cases <- nrow(counting[counting$negative_sign == 1, ])
positive_cases <- nrow(counting[counting$positive_sign == 1, ])
negative_cases
positive_cases

#########
# 1.2 Carbon Intensity Scores (CIS)
#########
CO2 <- Financial_Data %>%  select(date, compident, CO2EquivScope1) %>% group_by(compident) %>% filter_at(vars(-compident), all_vars(!is.na(.))) %>%
  summarize(mean_CO2 = mean(CO2EquivScope1, na.rm = TRUE)) %>% ungroup()
CO2 <- subset(CO2, compident != "c881") # this company has only 0: I'll eliminate it

NS  <- Financial_Data %>% select(date, compident, netsales)       %>% group_by(compident) %>% filter_at(vars(-compident), all_vars(!is.na(.))) %>%
  summarize(mean_NS  = mean(netsales, na.rm = TRUE)) %>% ungroup()
EM  <- left_join(CO2,NS,by="compident")
EM  <- EM %>% mutate(EM=mean_CO2/mean_NS*1000000)

#######
# 1.2.1 Maximum and Minimum values for the Carbon Intensity Scores (CIS) values appearing at session "3.2 Financial and CO2 data"
#######
CIS_min <- summary(EM$EM)[1]
CIS_max <- summary(EM$EM)[6]

# log-CIS
EM  <- EM %>% mutate(logEM = log(EM))

#######
# 1.2.2 Classify companies in three groups: low, medium and high emitters
#######
 
values <- summary(EM$logEM)
min_q  <- values[2]  # 1st Quartile
max_q  <- values[5]  # 3rd Quartile
    
intervals1 <- intervals %>%
      mutate(EM_group = cut(EM, breaks = c(-Inf, min_q, max_q, Inf), labels = c("Low", "Medium", "High")))

##########################################################
# 2.  GENERATE FIGURES
##########################################################

###########################
# 2.1 FIGURE 2 
###########################
start_date <- "2015-01-01" 
end_date   <- "2022-12-01"


NEWS_raw  <- NEWS_raw0 %>% 
  group_by(date,topic) %>% 
  summarize(mean_sent = mean(sentiment, na.rm = T),
            sum_sent  = sum(sentiment, na.rm = T)) 


roll_m <- 90
NEWS <- NEWS_raw %>% data.frame() %>%
  select(date, topic, mean_sent) %>%
  filter(topic %in% c("climate change", "natural disaster", "green economy")) %>%
  spread(topic, mean_sent) %>% dplyr::rename(CC="climate change") %>% dplyr::rename(ND="natural disaster") %>% dplyr::rename(GE="green economy") %>% na.locf
NEWS$date   <- as.Date(NEWS$date)
NEWS        <- data.frame(NEWS)
NEWS$rollCC <- zoo::rollmean(NEWS$CC, k = roll_m, fill = NA, align="right")
NEWS$rollND <- zoo::rollmean(NEWS$ND, k = roll_m, fill = NA, align="right")
NEWS$rollGE <- zoo::rollmean(NEWS$GE, k = roll_m, fill = NA, align="right")
NEWS_daily  <- NEWS %>% filter(date>=start_date)
NEWS_monthly   <- NEWS %>% filter(date>=start_date) %>% mutate(date=substr(date,1,7))
NEWS_monthly   <- NEWS_monthly %>% mutate(date=paste0(date,"-01")) %>% mutate(date = as.Date(date)) 

# Create dummy for natural disaster
dummy         <- rep(0,nrow(NEWS_daily))
dummy[which(NEWS_daily$date=="2020-05-01"):which(NEWS_daily$date=="2020-12-31")] <- 1
tempND        <- summary(lm(NEWS_daily$rollND ~ dummy))
NEWS_daily$rollND_d <- as.matrix(tempND$residuals)


myggtheme <- theme(panel.grid.major.x = element_blank() ,
                   panel.grid.major.y = element_line( size=.01, color="lightgrey"), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "lightgrey"),
                   legend.position = "bottom")

## CC
list_events <- as.Date(
  c(
    #"2012-12-01",  #"2012-12-01",  # (1)  Doha UN Climate Change
    "2013-02-01",  #"2013-02-12",  # (2)  Obama's State Union Speech: confirm commitment to fight climate change
    "2014-05-01",  #"2014-05-05",  # (3)  3rd National Climate Assessment US (2)
    "2014-11-01",  #"2014-11-04",  # (4)  Democratic party loses control of senate (mid-term elections). News implication for climate policy
    "2015-08-01",  #"2015-08-03",  # (5)  President Barack Obama introduces the Clean Power Plan in the United States to reduce air pollution, including greenhouse gas emissions.[12]
    "2015-12-01",  #"2015-12-12",  # (6)  Paris Agreement (3)
    "2016-11-01",  #"2016-11-07",  # (7)  COP22 Marrakech, Marocco (4) and Trump win Elections
    "2017-06-01",  #"2017-06-01",  # (8)  Trump withdrawal (5)
    "2017-11-01",  #"2017-11-06",  # (9)  COP23 Bonn, Germany (6)
    #"2018-09-01",  #"2018-09-19",  # (10) 12.3% Artic Sea Minimum; Melting glaciers; Permafrost thawing Ocean acidification
    "2018-10-01",  #"2018-10-08",  # (11) publication of the IPCC Special Report on Global Warming of 1.5 °C predicting dire environmental consequences if global warming is allowed to rise above
    "2018-12-01",  #"2018-12-03",  # (12) COP24 Katowice, Poland   (7)
    #"2019-04-01",  #"2019-04-28",  # (13) Democratic Representatives Alexandria Ocasio-Cortez and Ed Markey introduce a resolution in the U.S. House of Representatives calling for a Green New Deal, causing little immediate change in policy but attracting considerable attention as an issue in the 2020 United States elections.[14]
    #"2019-09-01",  #                 (14) UN Secretary-General António Guterres convenes the 2019 UN Climate Action Summit in an attempt to pressure countries to commit to greenhouse gas reductions of 45 percent by 2030 and carbon neutrality by 2050, but the talks are hindered by the absence of the two largest carbon emitters China and the United States.[9]
    "2019-12-01",  #                 (15) COP 25 Madrid The European Commission issues a European Green Deal to reduce Europe to climate neutrality by 2050.[15]
    "2021-01-01",  #                 (17) Joe Biden signs an executive order for the United States to rejoin the Paris Agreement
    #"2021-08-01",  #"2021-08-07",  # (18) August 7, 2021: publication of the IPCC Sixth Assessment Report predicting that 1.5 °C warming within the next two decades is likely at current emissions levels and calls for drastic action to prevent further catastrophic warming.[9][16]
    "2021-11-01",  #                 (19) October 31-November 12, 2021: The 26th COP is held in Edinburgh, United Kingdom, after being postponed due to the COVID-19 pandemic It had been set to be the first COP to include a commitment to phase out coal power stations, but this was lessened at last minute.[17]
    "2022-11-01")
)  

news_events_CC=data.frame(date=list_events,   
                          event=seq(1:length(list_events)),
                          name = c(
                            "Obama's State Union",
                            "US National Climate Assessment",
                            "Mid-term elections",
                            "Clean Power Plan",
                            "Paris Agreement",
                            "COP22 Marrakech and Trump win elections",
                            "Trump withdrawal",
                            "COP23 Bonn",
                            "IPCC Special report on global warming",
                            "COP24 Katowice",
                            "COP25 Madrid",
                            "Biden rejoins Paris agreement",
                            "COP 26 Edinburgh",
                            "COP 27 Sharm El Sheikh"
                          ))


NEWS_daily$CC_smooth <- zoo::rollmean(NEWS_daily$CC, k=90, fill=NA, align="right")

NEWS_daily %>%
  left_join(news_events_CC %>% 
              mutate(date = floor_date(date, "month")) %>%
              filter(!event %in% c(8,10)), 
            by="date") %>%
  ggplot(aes(x=date, y=CC_smooth)) +
  geom_line() +
  myggtheme +
  ggrepel::geom_label_repel(aes(x=date, y=CC_smooth, label = name), nudge_x = 1, size = 3,  box.padding = 0.5, max.overlaps = Inf) +
  ylab("Climate change sentiment")

ggsave( paste0(save_graph_path, "ts_cc.pdf"), height= 5, width=10)
ggsave( paste0(save_graph_path, "Figure2_a.pdf"), height= 5, width=10)


## ND
list_events <- as.Date(
  c(
    "2012-08-01",  # (1)  Hurricane Sandy
    "2013-09-01",  # (2) "2013-09-11",  # (1)  Colorado floods
    "2014-11-01",  # (3) "2014-11-13",  # (1)  North American winter storm
    "2015-08-01",  # (4) "2015-08-15",  # (2)  Okanogan Complex fire - wildfire
    "2016-01-01",  # (5) "2016-01-23",  # (3)  U.S. Blizard
    "2017-09-01",  # (6)  Hurricane Maria and Irma
    "2018-07-01",  # (7) "2018-07-15",  # (6)  California wildfire
    "2019-09-01",  # (8) "2019-09-17",  # Tropical storm Imelda
    "2020-08-01",  # (9) "2020-08-26",  # (7)  Hurricane Laura and wildfire season
    "2021-01-01",  # (10)"2021-01-31",  # (8)  Nor'easter winter storm
    "2021-08-01",  # (11)"2021-08-26",  # (10)  Hurricane Ida
    "2022-03-01"))# (12)"2022-03-05")),  # (11)  Tornado outbreak


news_events_ND=data.frame(date=list_events,   
                          event=seq(1:length(list_events)),
                          name = c(
                            "Hurricane Sandy",
                            "Colorado floods",
                            "North American winter storm",
                            "Okanogan Complex wildfire",
                            "US Blizard",
                            "Hurricane Maria and Irma",
                            "California wildfire",
                            "Tropical storm Imelda",
                            "Hurricane Laura and wildfire season",
                            "Nor'easter winter storm",
                            "Hurricane Ida",
                            "Tornado outbreak"
                          ))


NEWS_daily$ND_smooth <- zoo::rollmean(NEWS_daily$ND, k=90, fill=NA, align="right")

NEWS_daily %>%
  left_join(news_events_ND %>% 
              mutate(date = floor_date(date, "month")) %>%
              filter(!event %in% c(8,10)), 
            by="date") %>%
  ggplot(aes(x=date, y=ND_smooth)) +
  geom_line() +
  myggtheme +
  ggrepel::geom_label_repel(aes(x=date, y=ND_smooth, label = name), nudge_x = 1, size = 3,  box.padding = 0.5, max.overlaps = Inf) +
  ylab("Natural disaster sentiment")

ggsave(paste0(save_graph_path, "ts_nd.pdf"), height= 5, width=10)
ggsave(paste0(save_graph_path, "Figure2_b.pdf"), height= 5, width=10)


###########################
# 2.2 FIGURE 3
###########################

intervals1     <- intervals1[order(intervals1$Q_50),]
intervals1$xx  <- 1:nrow(intervals1)

graph_phi <- ggplot(intervals1, aes(x = xx)) +
  geom_point(aes(y = Q_50, color = EM_group), size = 4) +
  geom_point(aes(y = Q_16, color = EM_group)) +
  geom_point(aes(y = Q_84, color = EM_group)) +
  scale_color_manual(values = c("green", "gray", "brown")) + theme_bw() + theme(axis.text = element_text(size = size_text_xy_overall))      # + theme(legend.position = "none") 
graph_phi_overall = graph_phi + geom_hline(yintercept=0, linetype = "dashed", 
                                             color="red", size=0.5)+
  labs(x = "Individual Companies", y = expression(italic(phi))) +
  theme(legend.title = element_blank(), 
        #legend.text = element_text(size = 30), 
        legend.key.size = unit(size_text_leg1_overall, "cm"),
        axis.text = element_text(size = size_text_axis_LambdaOverall),
        axis.title.x = element_text(size = size_text_axis_labels),  # Aumenta la dimensione del titolo dell'asse x
        axis.title.y = element_text(size = size_text_axis_labels_phi),# family = "serif", face = "italic"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ ylim(ymin_value, ymax_value)+ theme(legend.position = "none") 


if(names_j=="DS_CC_M"){
  pdf(file = paste0(save_graph_path,"Figure3_","L",".pdf"),width=chart_width,height=chart_height,paper='special')
  print(graph_phi_overall)
  dev.off()}
if(names_j=="DS_ND_d_M"){
  pdf(file = paste0(save_graph_path,"Figure3_","R",".pdf"),width=chart_width,height=chart_height,paper='special')
  print(graph_phi_overall)
  dev.off()}


###########################
# 2.3 FIGURE 4 & 5
###########################

intervals1 <- intervals %>%
  mutate(EM_group = cut(EM, breaks = c(-Inf, min_q, max_q, Inf), labels = c("Low", "Medium", "High")))

intervals1 <- intervals1 %>%
  group_by(sector_name) %>%
  arrange(Q_50, .by_group = TRUE) %>%
  mutate(xx_sector = row_number()) %>%
  ungroup()

intervals1$xx <- intervals1$xx_sector 
intervals1$xxx <- 1

graph_phi <- ggplot(intervals1, aes(x = xx)) +
      geom_point(aes(y = Q_50, color = EM_group), size = 4) +
      geom_point(aes(y = Q_16, color = EM_group)) +
      geom_point(aes(y = Q_84, color = EM_group)) +
      scale_color_manual(values = c("green", "gray", "brown")) #+
    #geom_segment(data = subset(sector_avgs1),
    #             aes(x = xx, y = `16%`, xend = xx, yend = `84%`),
    #             color = "black", linetype = "dashed")
graph_phi = graph_phi + scale_color_manual(values = c("green", "gray","brown")) + theme_bw()
graph_phi = graph_phi + geom_hline(yintercept=0, linetype = "dashed", color="red", size=0.5)+
      xlab(NULL) + ylab(NULL)+theme(legend.title = element_blank(), 
                                    legend.text = element_text(size = size_text_leg), legend.key.size = unit(size_text_leg1, "cm"))
graph_phi = graph_phi + geom_line(aes( x=xx, y=avg ),size=0.5,show.legend = FALSE, color  ="blue",linetype = "dashed")+xlab(NULL) +ylab(NULL)+theme(legend.title = element_blank())
graph_phi_sectors = graph_phi + facet_wrap(~sector_name, ncol = 5, nrow = 2,   scales = "free_x" )+
      labs(x = "Individual Companies", y = expression(italic(phi))) +
      theme(strip.text.x = element_text(size = size_text_xy), 
            strip.text.y = element_text(size = size_text_xy),
            axis.text = element_text(size = size_text_xy+2),
            axis.title.x = element_text(size = size_text_xy+3),  # Aumenta la dimensione del titolo dell'asse x
            axis.title.y = element_text(size = size_text_axis_labels_phi+3), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) + theme(legend.position = "none") #+ ylim(ymin_value, ymax_value)

if(names_j=="DS_CC_M"){
  pdf(file = paste0(save_graph_path,"Figure4.pdf"),width=chart_width,height=chart_height,paper='special')
  print(graph_phi_sectors)
  dev.off()}
if(names_j=="DS_ND_d_M"){
  pdf(file = paste0(save_graph_path,"Figure5.pdf"),width=chart_width,height=chart_height,paper='special')
  print(graph_phi_sectors)
  dev.off()}


##########################################################
# 2.4 FIGURE 6
##########################################################

count_store <- ANALYSIS$count.store
  
rept     <- dim(count_store)[1]
eff_modif<- dim(count_store)[3]
  
df     <- NULL
for (i in seq_len(eff_modif)){
  df <-  rbind(df, as.matrix(count_store[1:rept,1,i]))
}
  
df <- data.frame(
  em=factor(rep(c("CO2/sales", "sector"), each=rept)),
  split=df)
  
mu <- ddply(df, "em", summarise, grp.mean=mean(split))
  
count_store_graph <- ggplot(df, aes(x=split, color=em, fill=em)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
    geom_density(alpha=0.6)+
    geom_vline(data=mu, aes(xintercept=grp.mean, color=em),
               linetype="dashed")+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+xlab(NULL) +ylab(NULL)  + theme_bw()+
    theme(legend.title = element_blank()) + 
    theme(legend.text = element_text(size = size_text_leg)) +
    
    theme(legend.key.size = unit(size_text_leg1, "cm")) + labs(x = "Occurrence in tree", y = "Density")+
    theme(axis.text = element_text(size = size_text_xy),
          axis.title.x = element_text(size = size_text_xy+3),  # Aumenta la dimensione del titolo dell'asse x
          axis.title.y = element_text(size = size_text_xy+3),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) #+ ylim(ymin_value_dib, ymax_value_dib)

if(names_j=="DS_CC_M"){
  pdf(file = paste0(save_graph_path,"Figure6.pdf"),width=16,height=12,paper='special')
  print(count_store_graph)
  dev.off()}

##########################################################
# 2.5 FIGURE 7
##########################################################

scenarios             <- ANALYSIS$phi.scen.store
lambda_scen           <- scenarios

# Setting the minimum and maximum of the scenario grid as specified in section "4.3 Counterfactual analysis"
set.z <- seq(base::min(Z[,1]), base::max(Z[,1]), length.out=250)

lambda_scenario_store <- data.frame()
for (j in c(1:10)){
  lambda_scenario            <- apply(lambda_scen[,,j],2,function(x) quantile(x, c(0.16, 0.5, 0.84)))
  lambda_scenario            <- as.data.frame(t(lambda_scenario))
  lambda_scenario$sector     <- rep(j, nrow(lambda_scenario))
  lambda_scenario$s_names    <- rep(names_sec[j], nrow(lambda_scenario))
  max_EM                     <- filter(Z, sector == j) %>% select(EM) %>% max
  min_EM                     <- filter(Z, sector == j) %>% select(EM) %>% min
  lambda_scenario$position_max_EM <- which.min(abs(set.z - unique(max_EM)))
  lambda_scenario$position_min_EM <- which.min(abs(set.z - unique(min_EM)))
  lambda_scenario$EM         <- set.z
  lambda_scenario_store      <- rbind(lambda_scenario_store, lambda_scenario)
}
lambda_scenario_store$xx         <- 1:nrow(lambda_scenario)

graph_scen = ggplot(lambda_scenario_store) + geom_line(aes(x=xx, y=`50%`), colour="blue", show.legend = TRUE,size=0.8) 
graph_scen = graph_scen  + geom_line(aes(x=xx, y=`16%` , color  = "blue"), linetype = "dashed", show.legend = FALSE,size=0.5) #+ ylab("Fluctuation test") + xlab(" ")
graph_scen = graph_scen  + geom_line(aes(x=xx, y=`84%` , color  = "blue"), linetype = "dashed", show.legend = FALSE,size=0.5) #+ ylab("Fluctuation test") + xlab(" ")
graph_scen = graph_scen  + geom_rect(aes(xmin = position_min_EM, xmax = position_max_EM, ymin = -Inf, ymax = Inf), fill = "grey80", alpha = 0.0125) +labs(y = "", x = "") 
graph_scen = graph_scen  + geom_vline(aes(xintercept = position_max_EM), color = "grey80", linetype = "dashed", size = 0.5) + geom_vline(aes(xintercept = position_min_EM), color = "grey80", linetype = "dashed", size = 0.5)
graph_scen = graph_scen  + labs(x = "Carbon Intensity Score", y = expression(paste(bold(E) , "(", phi[], ") = ", mu(z))))  +  scale_x_continuous(breaks = c(62,125,187,250),
                                                                                                                                                 label = as.character(round(c(set.z[62],set.z[125],set.z[187],set.z[250]),0))) #c("25","50","75","15.8"))
graph_scen = graph_scen  + theme_bw() + theme(axis.text = element_text(size = size_text_xy), axis.title = element_text(size = size_text_xy), plot.title = element_text(size = size_text_xy), legend.position = "none")
graph_scen = graph_scen  + theme(axis.title.y = element_text(size = size_text_axis_labels_phi-3))
graph_scen = graph_scen  + geom_hline(yintercept=0, linetype = "dashed", color="red", size=0.5)
graph_scen = graph_scen  + facet_wrap(~s_names, ncol = 5, nrow = 2, scales = "free_x")+theme(strip.text.x = element_text(size = size_text_xy), strip.text.y = element_text(size = size_text_xy),axis.text = element_text(size = size_text_xy)) #+ ylim(ymin_value, ymax_value)
graph_scen = graph_scen  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

if(names_j=="DS_ND_d_M"){
  pdf(file = paste0(save_graph_path,"Figure7.pdf"),width=16,height=12,paper='special')
  print(graph_scen)
  dev.off()}

##########################################################
# 3.  GENERATE TABLES
##########################################################

##########
# 3.1 Table 1
##########
sect_comp    <- Z      %>% select(sector, compident) %>%  mutate(Company_Index = row_number(), .before = 1)
sect_comp    <- sect_comp %>% mutate(sector_name = names_sec[sector])
phi          <- ANALYSIS$Phi.store
Phi_rev      <- phi[,,1]

df_means <- as.data.frame(t(Phi_rev)) %>%
  mutate(Company_Index = row_number()) %>%
  inner_join(sect_comp %>% select(Company_Index, sector_name), by = "Company_Index") %>%
  group_by(sector_name) %>%
  summarise(across(starts_with("V"), mean)) %>%
  pivot_longer(-sector_name, names_to = "draw", values_to = "mean_val") %>%
  pivot_wider(names_from = sector_name, values_from = mean_val) %>%
  select(-draw)

Table_1 <- df_means %>%
  pivot_longer(cols = everything(),            
               names_to = "sector", 
               values_to = "draw_value") %>%
  group_by(sector) %>%                         
  summarise(                                  
    mean   = mean(draw_value),
    median = median(draw_value),
    q16    = quantile(draw_value, 0.16),
    q84    = quantile(draw_value, 0.84)
  ) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
Table_1 <- Table_1 %>% mutate(across(c(mean, median, q16, q84), ~ sprintf("%.4f", .)))
