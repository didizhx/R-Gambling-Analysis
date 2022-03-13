setwd("C:/Users/mma/Downloads/Semester 1/BUSINESS ANALYTICS TOOLS OPEN SOURCE/Group Project/Group Project Final/")

#LOADING REQUIRED PACKAGES
if (!require(dplyr)) install.packages('dplyr')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lubridate)) install.packages('lubridate')
if (!require(rfm)) install.packages("rfm")
if (!require(DT)) install.packages("DT")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(XLConnect))install.packages("XLConnect")

library(dplyr)
library(tidyverse)
library(lubridate)
library(rfm)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(sf) 
library(readxl)
library(XLConnect)
library(plotly)

#IMPORTING DATA (DATASET)
data <- load(file = "DataGroupAssignment.Rdata")

#IMPORTING DATA (APPENDIX)
appendix_excel <- "Appendices Group Assignment.xlsx"

#Reading Data
products <- read_excel(path = appendix_excel, sheet = "Appendix 1")
countries <- read_excel(path = appendix_excel, sheet = "Appendix 2")
languages <- read_excel(path = appendix_excel, sheet = "Appendix 3")
applications <- read_excel(path = appendix_excel, sheet = "Appendix 4")

######################## DATA CLEANING ########################

# DATASET Demographics

#FIXING DATES IN Demographics
Demographics$RegDate <- as.integer(format(ymd(Demographics$RegDate), "%Y%m%d"))
Demographics$FirstPay <- as.integer(format(ymd(Demographics$FirstPay), "%Y%m%d"))
Demographics$FirstAct <- as.integer(format(ymd(Demographics$FirstAct), "%Y%m%d"))
Demographics$FirstSp <- as.integer(format(ymd(Demographics$FirstSp), "%Y%m%d"))
Demographics$FirstCa <- as.integer(format(ymd(Demographics$FirstCa), "%Y%m%d"))
Demographics$FirstGa <- as.integer(format(ymd(Demographics$FirstGa), "%Y%m%d"))
Demographics$FirstPo <- as.integer(format(ymd(Demographics$FirstPo), "%Y%m%d"))

# JOIN DEMOGRAPHICS WITH APPENDIX TABLES
joined_table<- left_join(Demographics,languages, by=NULL)
joined_table <- left_join(joined_table, countries, by = NULL)
joined_table <- left_join(joined_table, applications, by = NULL)

#Filtering for registration dates from Feb 1, 2005 to Feb 27, 2005
joined_table <- joined_table %>% filter(joined_table$RegDate >= 20050201 & joined_table$RegDate <= 20050227)

#remove NA values from FirstAct
joined_table <- joined_table[!(is.na(joined_table$FirstAct)),]

#Calculating distances in days between registration date and first plays
joined_table$dif_RDFP <- ymd(joined_table$FirstPay) - ymd(joined_table$RegDate)
joined_table$dif_RDFA <- ymd(joined_table$FirstAct) - ymd(joined_table$RegDate)
joined_table$dif_RDFS <- ymd(joined_table$FirstSp) - ymd(joined_table$RegDate)
joined_table$dif_RDFC <- ymd(joined_table$FirstCa) - ymd(joined_table$RegDate)
joined_table$dif_RDFG <- ymd(joined_table$FirstGa) - ymd(joined_table$RegDate)
joined_table$dif_RDFPo <- ymd(joined_table$FirstPo) - ymd(joined_table$RegDate)

#################################################################################

                       #### DATASET POKER CHIPS ####

# EXTRACTING DATE, Month, Day, Weekdays and Time from TransDateTime
PokerChipConversions$Date <- ymd(format(as.POSIXct(PokerChipConversions$TransDateTime,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d"))
PokerChipConversions$TransMonth <- month(as.POSIXlt(PokerChipConversions$Date, format="%Y/%m/%d"))
PokerChipConversions$TransDay <- day(as.POSIXlt(PokerChipConversions$Date, format="%Y/%m/%d"))
PokerChipConversions$TransWeekdays <- weekdays(as.POSIXlt(PokerChipConversions$Date, format="%Y/%m/%d"))
PokerChipConversions$Time <- format(as.POSIXct(PokerChipConversions$TransDateTime,format="%Y-%m-%d %H:%M:%S"),"%H")

#mapping transaction types to names
PokerChipConversions$TransType <- recode(PokerChipConversions$TransType,"124" = "Buy", "24" = "Sell")

# poker chip cleaned group by and summary statistics
PokerChipCleaned <-
  PokerChipConversions %>% group_by(UserID, TransType) %>%
  dplyr::summarise(MinTransDate = min(Date), MaxTransDate = max(Date),
                   TransactionsAmount= sum(TransAmount),
                   AverageTransactionAmount = mean(TransAmount),
                   MaxTransactionAmount = max(TransAmount),
                   MinTransactionAmount = min(TransAmount),
                   NumberOfTransactions = n(),
                   DaysOfPlay = n_distinct(Date))

#extracting column names from pokerchipcleaned
ColumnsNamesPC <- colnames(PokerChipCleaned)[3:ncol(PokerChipCleaned)]
ColumnsNamesPC
PokerChipCleaned <- pivot_wider(PokerChipCleaned,names_from = TransType, values_from = ColumnsNamesPC)

#Creating new variable for net balance per user
PokerChipCleaned <- PokerChipCleaned %>% mutate(NetAmount = TransactionsAmount_Sell - TransactionsAmount_Buy)
colnames(PokerChipCleaned)

#Adding 3 to each col to specify that it is for Product #3(Poker Boss Media)
colnames(PokerChipCleaned)[2:ncol(PokerChipCleaned)] <- paste(colnames(PokerChipCleaned)[2:ncol(PokerChipCleaned)],"3", sep = "_")

#check for repeated values in new table
sum(duplicated(PokerChipCleaned$UserID))

#remove Days of Play Sell column
PokerChipCleaned <- dplyr::select(PokerChipCleaned,-c("DaysOfPlay_Sell_3"))
colnames(PokerChipCleaned)

#cHANGING TYPE TO INT 
PokerChipCleaned$MinTransDate_Buy_3 <- as.integer(PokerChipCleaned$MinTransDate_Buy_3)
PokerChipCleaned$MinTransDate_Sell_3 <- as.integer(PokerChipCleaned$MinTransDate_Sell_3)
PokerChipCleaned$MaxTransDate_Buy_3 <- as.integer(PokerChipCleaned$MaxTransDate_Buy_3)
PokerChipCleaned$MaxTransDate_Sell_3 <- as.integer(PokerChipCleaned$MaxTransDate_Sell_3)

# #REPLACING NULL WITH 0
PokerChipCleaned[is.na(PokerChipCleaned)] <- 0
colnames(PokerChipCleaned)
PokerChipCleaned

#################################################################################

                               ### UDA ###

# #FIXING DATE IN UDA
UDA <- UserDailyAggregation %>% group_by(UserID)
UDA$Date <- ymd(UDA$Date)

# Neglect accounting system correction

UDA$Stakes< -case_when(UDA$Stakes < 0 ~ 0, TRUE ~ UDA$Stakes) 
UDA$Winnings<-case_when(UDA$Winnings < 0 ~ 0, TRUE ~ UDA$Winnings) 
UDA$Bets <- case_when(UDA$Bets < 0 ~ 0, TRUE ~ UDA$Bets) 

#GROUPBY
UDA_clean <- UDA %>%
  group_by(UserID, ProductID) %>% 
  dplyr::summarize(MinTransDate = min(Date), MaxTransDate = max(Date),
                   total_trans = n(),
                   total_stakes = round(sum(Stakes),2),
                   total_winnings = round(sum(Winnings),2),
                   total_net = round(sum(Winnings)-sum(Stakes),2),
                   total_bets = round(sum(Bets),2))
UDA_clean

#WIDER
UDA_wider = pivot_wider(UDA_clean,id_cols = UserID, names_from = ProductID , values_from = colnames(UDA_clean)[3:9])

#####################################

# MERGING DATA TO FORM DATAMART 

#MERGE DATA 
datamart<- left_join(joined_table,PokerChipCleaned, by=NULL)

datamart <- left_join(datamart,UDA_wider, by=NULL)

datamart$MinTransDate_1 <- as.integer(datamart$MinTransDate_1)

# (remember to check/change the column number each time when there are any changes in variables!)
datamart$recency <- apply(datamart[ , c("MaxTransDate_Buy_3","MaxTransDate_1","MaxTransDate_2","MaxTransDate_8",
                                        "MaxTransDate_6","MaxTransDate_7","MaxTransDate_4","MaxTransDate_5")], MARGIN =  1, FUN = max, na.rm = T)
datamart$firstbuydate <- apply(datamart[ , c("MinTransDate_Buy_3","MinTransDate_1","MinTransDate_2","MinTransDate_4",
                                             "MinTransDate_5","MinTransDate_6","MinTransDate_7","MinTransDate_8")], MARGIN =  1, FUN = min, na.rm = T)

datamart$total_trans <- rowSums(datamart[ , c("DaysOfPlay_Buy_3","total_trans_1","total_trans_2","total_trans_4",
                                              "total_trans_5","total_trans_6","total_trans_7","total_trans_8")], na.rm=TRUE)
datamart$total_stakes <- rowSums(datamart[ , c("TransactionsAmount_Buy_3","total_stakes_1","total_stakes_2","total_stakes_4",
                                               "total_stakes_5","total_stakes_6","total_stakes_7","total_stakes_8")], na.rm=TRUE)
datamart$total_winnings <- rowSums(datamart[ , c("TransactionsAmount_Sell_3","total_winnings_1","total_winnings_2","total_winnings_4",
                                                 "total_winnings_5","total_winnings_6","total_winnings_7","total_winnings_8")], na.rm=TRUE)
datamart$total_net <- rowSums(datamart[,c("NetAmount_3","total_net_1","total_net_2","total_net_4",
                                          "total_net_5","total_net_6","total_net_7","total_net_8")], na.rm=TRUE)
datamart$total_bets <- rowSums(datamart[ , c("NumberOfTransactions_Buy_3","total_bets_1","total_bets_2","total_bets_4",
                                             "total_bets_5","total_bets_6","total_bets_7","total_bets_8")], na.rm=TRUE)

datamart$firstbuydate <- format(ymd(datamart$firstbuydate), "%Y%m%d")

datamart <- data.frame(datamart)

################################################################################

#dropping 1 NA gender value 

datamart <- datamart %>% drop_na(Gender)

################################################################################
#avoid scientific number format
format(datamart, scientific = FALSE)

#Encode gender variable 
datamart$Gender <- recode(datamart$Gender, "0" = "Female", "1"="Male")

################################################################################
#pre-processing for RFM plots

analysis_date <- lubridate::as_date("2005-09-30")

datamart$recency_new <- as.numeric(analysis_date - as.Date(datamart$recency, "%Y-%m-%d"))
datamart$recency_new[is.na(datamart$recency_new)] <- 0

rfm_score <- rfm_table_customer(data = datamart,customer_id = UserID,
                                n_transactions =total_bets,recency_days = recency_new,
                                total_revenue = total_stakes, analysis_date = analysis_date,
                                recency_bins = 5,frequency_bins =5, monetary_bins = 5)

#Pre-processing for pie chart based on customer loyalty division

dataLoyalty <- datamart %>%
  mutate(Loyalty = case_when(
    between(datamart$total_stakes, 50000, 2247200 ) ~ "Platinum (>=50000)",
    between(datamart$total_stakes, 20000, 50000) ~ "Gold (20000 - 50000)",
    between(datamart$total_stakes, 500, 20000) ~ "Silver (500 - 20000)",
    between(datamart$total_stakes, 0, 500) ~ "Others (<=500)"))

df <- dataLoyalty %>% group_by(Loyalty) %>%
  dplyr::summarise(count = n()) %>% dplyr::arrange(desc(count))

customer_loyalty <- df %>% plot_ly(labels = ~Loyalty, values = ~count, type = 'pie') %>% 
  layout(title = "Customers Loyalty Division based on Total Stakes Amount",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


######################  Country Maps ######################

#getting map data
mapdata <- map_data("world")
mapdata <- mapdata %>% rename(Country.Name = region)
mapdata$CountryName <- as.character(mapdata$CountryName)

datamart_country <- datamart %>% group_by(Country.Name) %>%
  summarise(nbr_users = n(),
            Totbets = sum(total_bets),
            totspent = sum(total_stakes),
            totwinning = sum(total_winnings))

country_df <- left_join(datamart_country,mapdata, by = "Country.Name")

####################### TOTAL USERS PER COUNTRY ####################
country_df <- country_df %>% filter (!is.na(country_df$nbr_users))
map1 <- ggplot(country_df,aes (x= long , y = lat, group = group )) +
  geom_polygon(aes (fill= nbr_users),color = "black")  +labs(title = "Countires by Number of  Users", fill = "Number of Users") +
  theme_void()
map1

####################### TOTAL BETS PER COUNTRY ####################

country_df <- country_df %>% filter (!is.na(country_df$Totbets))
map3 <- ggplot(country_df,aes (x= long , y = lat, group = group)) +
  geom_polygon(aes (fill= Totbets/1000),color = "black")+
  scale_fill_viridis_c(option = "plasma",trans = 'sqrt')+labs(title = "Countires by Number of Bets", fill = "Total Bets in Thousands") +
  theme_void()
map3


####################### AMOUNT SPENT PER COUNTRY ####################

country_df <- country_df %>% filter (!is.na(country_df$totspent))
map2 <- ggplot(country_df,aes (x= long , y = lat, group = group )) +
  geom_polygon(aes (fill= totspent/1000000),color = "black") +
  scale_fill_viridis_b(option = "plasma",trans = 'sqrt')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  theme_void() +
  labs(title = "Countries by Total Amount Spent", fill = "Total Spent in Millions")
map2

####################### TOTAL Winnings PER COUNTRY ####################
country_df <- country_df %>% filter (!is.na(country_df$totwinning))
map4 <- ggplot(country_df,aes (x= long , y = lat, group = group )) +
  geom_polygon(aes (fill= totwinning/1000000),color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  theme_void() +
  labs(title = "Countries by Total Amount Spent", fill = "Total Winnings in Millions")
map4

############  FILTERING COUNTRY ###########


 ## subset of country data to get top 10 countries ##
country_filtered <- data.frame(country_filtered)

country_filtered <- datamart %>% group_by(Country.Name) %>%
  summarise(nbr_users = n(),
            country_total_stakes = sum(total_stakes),
            country_total_winnings = sum(total_winnings),
            country_total_bets = sum(total_bets))

write.csv(datamart, file = "datamart.csv")
write.csv(PokerChipConversions, file = "PokerChipConversions.csv")

                                                    ### TOP 10 COUNTRIES BY NUMBER OF USERS ###

t10countriesnbr <- top_n(country_filtered, 10, nbr_users)
t10countriesusers <- ggplot(t10countriesnbr, aes(x=reorder(Country.Name,-nbr_users), y= nbr_users,fill=Country.Name )) + 
  geom_col( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "TOP 10 Countries By No.Of.Users",x = "Countries", y= "Number of Users")
t10countriesusers

                                                      ### TOP 10 COUNTRIES BY TOTAL BETS ###

t10countriesB <- top_n(country_filtered, 10, country_total_bets)
t10countriesbets <- ggplot(t10countriesB, aes(x=reorder(Country.Name,-country_total_bets), y= country_total_bets/1000000,fill=Country.Name )) + 
  geom_col( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "TOP 10 Countries By Bets(Millions)",x = "Countries", y= "Total Number of Bets (Millions)")
t10countriesbets + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


                                                    ##### TOP 10 COUNTRIES BY TOTAL STAKES #####

t10countriesS <- top_n(country_filtered, 10, country_total_stakes)

t10countriesstakes <- ggplot(t10countriesS, aes(x=reorder(Country.Name,-country_total_stakes), y= country_total_stakes/1000000,fill=Country.Name )) + 
  geom_col( ) +
  scale_fill_hue(c = 40) +
  scale_y_continuous(breaks=seq(0, 60,5)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "TOP 10 Countries By Stakes(Millions)",x = "Countries", y= "Total stakes (Millions)")
t10countriesstakes #+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


                                                    ### TOP 10 COUNTRIES BY TOTAL WINNINGS  ###

top_10_countriesW <- top_n(country_filtered, 10, country_total_winnings)
top_10_countriesW <- top_10_countriesW[order(top_10_countries2$country_total_winnings, decreasing = TRUE),]

t10countrieswinnings <- ggplot(top_10_countriesW, aes(x= reorder(Country.Name, -country_total_winnings), y= country_total_winnings/1000000),fill=Country.Name) + 
  geom_col( ) +
  scale_fill_hue(c = 50) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "Top 10 Countries By Winnings(Millions)",x = "Countries", y= "Total stakes Winnings(Millions)")
t10countrieswinnings + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


################################ END OF COUNTRIES ########################


                                                        ##### FILTERING APPLICATIONS #####

                                                 ## subset of Application data ##
App_filtered <- datamart %>% group_by(Application.Description) %>%
  summarise(App_total_users = n(),
            App_total_stakes = sum(total_stakes),
            App_total_winnings = sum(total_winnings),
            App_total_bets = sum(total_bets))
App_filtered

                                                     ##### TOP 5 APPS By TOTAL USERS #####

t5Appsnbr <- top_n(App_filtered, 5, App_total_users)
t5Appsusers <- ggplot(t5Appsnbr, aes(x=reorder(Application.Description,-App_total_users), y= App_total_users,fill=Application.Description)) + 
  geom_col( ) +
  coord_flip() +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "TOP 5 Applications By No.Of.Users",x = "Applications", y= "Number of Users")
t5Appsusers
                                                     ##### TOP 5 APPS By TOTAL BETS #####

t5AppsB <- top_n(App_filtered, 5, App_total_bets)
t5Appsbets <- ggplot(t5AppsB, aes(x=reorder(Application.Description,-App_total_bets), y= App_total_bets/1000000,fill=Application.Description)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "TOP 5 Apps By Bets(Millions)",x = "Applications", y= "Total Number of Bets(Millions)")+
  scale_fill_manual("Teams",values = c('#F0B27A', '#F4D03F','#EC7063','#07B9CC','#ABEBC6'))
t5Appsbets + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


                                                        ##### TOP 5 APPS BY TOTAL STAKES #####

t5AppsS <- top_n(App_filtered, 5, App_total_stakes)
t5Appsstakes <- ggplot(t5AppsS, aes(x=reorder(Application.Description,-App_total_stakes), y= App_total_stakes/1000000,fill=Application.Description)) + 
  geom_col( ) +
  coord_flip() +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "TOP 5 Apps By Stakes(Millions)",x = "Applications", y= "Total stakes(Millions)")+
  scale_fill_manual("Teams",values = c('#F0B27A', '#5DADE2','#F9E79F','#E74C3C','#CACFD2'))
t5Appsstakes + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


                                                        ##### TOP 5 APPS BY TOTAL WINNINGS #####

t5AppsW <- top_n(App_filtered, 5, App_total_winnings)
t5AppsWinnings <- ggplot(t5AppsW, aes(x=reorder(Application.Description,-App_total_winnings), y= App_total_winnings/1000000,fill=Application.Description)) + 
  geom_col( ) +
  coord_flip() +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "TOP 5 Apps By Winnings(Millions)",x = "Applications", y= "Total stakes(Millions)")
t5AppsWinnings + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

############# END OF APPS ###########

############################################################ POKER ANALYSIS ############################################################

                                                         ####### Analysis by Week Days  #######

pokerweekdays <- PokerChipConversions %>% group_by(TransWeekdays) %>%
  summarise(nbr_transactions = n())

pokerweekdays <- pokerweekdays %>% drop_na(TransWeekdays)

pokerweekdaysplot <- ggplot(pokerweekdays, aes(x=reorder(TransWeekdays,-nbr_transactions), y= nbr_transactions,fill=TransWeekdays )) + 
  geom_col(width = 0.5) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  labs(title = "Poker Transactions By Weekday",x = "Weekday", y= "Total Transactions")+
  scale_fill_manual("Teams",values = c('#F0B27A', '#5DADE2','#F9E79F','#E74C3C','#CACFD2','#34495E','5E202C'))
pokerweekdaysplot + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



                                                         ####### Analysis By Month ##########

pokermonth <- PokerChipConversions %>% group_by(TransMonth,TransType) %>%
  summarise(nbr_transactions = n())

pokermonth <- pokermonth %>% drop_na(TransMonth)

pokermonth <- pokermonth %>% mutate(Month = month.name[TransMonth])

pokermonth <- subset(pokermonth, Month != 'October')

pokermonthplot <- ggplot(pokermonth, aes(x = reorder(Month,-nbr_transactions), y = nbr_transactions, fill = TransType)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(breaks=seq(0, 30000,5000)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.ticks.y=element_blank()) +
  scale_fill_manual("Teams",values = c('#F0B27A', '#5DADE2'))+
labs(title = "Poker Transactions By Month",x = "Month", y= "Total Transactions")
pokermonthplot

########################################################  CUSTOMER ANALYSIS #####################################################



custanalysis1 <- datamart[order(datamart$total_stakes, decreasing = TRUE),]

top10Customers<- top_n(custanalysis1, 10, total_stakes)

top10Customers <- top10Customers[c("UserID","RegDate","Gender","total_stakes")]

########################################################### SHINY APP ############################################################

#Building Shiny Application 

library(shiny)
library(ggplot2)
library(rfm)

if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("DBI")) install.packages("DBI"); library("DBI")
if(!require("leaflet")) install.packages("leaflet"); library("leaflet")
if(!require("flexdashboard")) install.packages("flexdashboard"); library("flexdashboard")
if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")


library(shiny)
library(shinydashboard)
library(DT)


ui <-navbarPage("Gamblers analysis", 
                 theme = shinythemes::shinytheme("darkly"),
                navbarMenu('Top Ranking',
                 tabPanel("TOP 10 Country",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "TOPNplot", label = "TOP 10 Countries By:",
                                           choices = c(
                                             "Number of Users" = "t1",
                                             "Total Stakes" = "t2",
                                             "Total Winnings" = "t3",
                                             "Total Bets" = "t4")
                              )),
                            mainPanel(h3("TOP 10 by Country",align = "center"),
                                      plotOutput("TOPN")))),
                 tabPanel("TOP 5 Application",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "TOPNAppplot", label = "TOP 5 Applications By:",
                                           choices = c(
                                             "Number of Users" = "a1",
                                             "Total Stakes" = "a2",
                                             "Total Winnings" = "a3",
                                             "Total Bets" = "a4")
                              )),
                            mainPanel(h3("TOP 5 by Application",align = "center"),
                                      plotOutput("TOPNApp"))))),
                 navbarMenu("Customer Profiling",
                            tabPanel("Users Distribution",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons(inputId = "radio1", label = "Number of gamblers by:", 
                                                      choices = c("Application" = "Application.Description",
                                                                  "Gender" = "Gender", "Language" = "`Language.Description`")
                                         )),
                                       mainPanel(   h3("Distribution of users by selected variable",align = "center"),
                                                    plotOutput('plot1')  ))),
                            
                            tabPanel("User Map",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons(inputId = "radioMaps", label = "Map by:", 
                                                      choices = c("Total Users per Country" = "m1",
                                                                  "Total Bets Per Country" = "m2", 
                                                                  "Amount Spent Per Country" = "m3",
                                                                  "Total Winnings Per Country" = "m4")
                                         )),
                                     
                                     mainPanel (
                                                plotOutput('plot2')))),
                            tabPanel("Customer Loyalty",
                                     mainPanel(plotlyOutput("CSPlot"))
                                     )),
                 
                 tabPanel("RFM Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "RFMPlot", label = "RFM Analysis:", 
                                           choices = c(
                                             "Recency vs Monetary" = "p1", 
                                             "Frequency vs Monetary" = "p2", 
                                             "Recency vs Frequency" = "p3",
                                             "RFM Histogram" = "p4",
                                             "RFM Heatmap" = "p5",
                                             "RFM Bar Chart" = "p6")
                              )),
                            mainPanel(h3("RFM Analysis",align = "center"),
                                      plotOutput("RFM")))),
                 
                 tabPanel("PokerAnalysis",
                          mainPanel(h3("Number of unique days played"), 
                                    plotOutput('Poker1'),
                                    h3("Number of unique days played vs number of buy transactions"),
                                    plotOutput("Poker2"),
                                    h3("Net Win/Loss vs Number of Bets (Buys)"),
                                    plotOutput("Poker5"),
                                    h3("Number of Poker Transactions Per Month"),
                                    plotOutput("Poker3"),
                                    h3("Number of Poker Transactions Per Weekday"),
                                    plotOutput("Poker4"),
                                    )),

                 tabPanel("OtherGames",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "StakesVsBets", label = "Select Y axis",
                                          choices = c("Stakes Amount" = "stakes",
                                                      "Number of Bets" = "bets")),
                              radioButtons(inputId = "Game", label = "Total Winnings vs Total Stakes for:", 
                                           choices = c("Sports book fixed-odd" = "1",
                                                       "Sports book live-action" = "2",
                                                       "Casino BossMedia" = "4",
                                                       "Supertoto" = "5",
                                                       "Games VS" = "6",
                                                       "Games bwin" = "7",
                                                       "Casino Chartwell" = "8"   
                                           ))),     
                            mainPanel(h3("Total Winnings per Game type and number of Stakes/Bets",align = "center"),
                                      plotOutput('BetsVSWin')))
                 ))


server <- function(input, output){
  
  output$plot1 <- renderPlot({
    ggplot( datamart,
            aes_string(x = input$radio1, fill = input$radio1))+
      geom_bar() + coord_flip()+ theme(legend.position="none") 
  })
  output$plot2 <- renderPlot({
    if (input$radioMaps == "m1")  {print(map1)}
    if (input$radioMaps == "m2")  {print(map3)}
    if (input$radioMaps == "m3")  {print(map2)}
    if (input$radioMaps == "m4")  {print(map4)}
  })
  output$Poker1 <- renderPlot({
    ggplot(data = datamart, aes(x = Gender, y = DaysOfPlay_Buy_3, fill = Gender))+geom_boxplot()+
      labs(y= "Number of Days Played")
  })
  output$Poker2 <- renderPlot({
    ggplot(data=datamart, aes(x = DaysOfPlay_Buy_3, y = NumberOfTransactions_Buy_3, color = Gender))+
      geom_point()+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
      labs(x="Number of Days Played", y= "Number of Transactions")
  })
  output$Poker3 <- renderPlot({
    ggplot(pokermonth, aes(x = Month, y = nbr_transactions, fill = TransType)) +
      geom_bar(stat = "identity", position = "dodge")+
      scale_y_continuous(breaks=seq(0, 30000,5000)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
            axis.ticks.y=element_blank()) +
      scale_fill_manual("Teams",values = c('#F0B27A', '#5DADE2'))
    labs(title = "Poker Transactions By Month",x = "Month", y= "Total Transactions")
    pokermonthplot
  })
  output$Poker4 <- renderPlot({
    ggplot(pokerweekdays, aes(x=reorder(TransWeekdays,-nbr_transactions), y= nbr_transactions,fill=TransWeekdays )) + 
      geom_col(width = 0.5) +
      scale_fill_hue(c = 40) +
      theme(legend.position="none") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
            axis.ticks.y=element_blank()) +
      labs(title = "Poker Transactions By week Days)",x = "Week Day", y= "Total Transactions")+
      scale_fill_manual("Teams",values = c('#F0B27A', '#5DADE2','#F9E79F','#E74C3C','#CACFD2','#34495E','5E202C'))
    pokerweekdaysplot + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  })
  
  output$BetsVSWin <- renderPlot({
    TSVarName <- paste("total",input$StakesVsBets,input$Game,sep="_")
    TWVarName <- paste("total_winnings_",input$Game,sep="")
    Gender <- unique(datamart$Gender)
    
    gamesDict <- c("1" = "Sports book fixed-odd",
      "2" = "Sports book live-action",
      "4" =  "Casino BossMedia" ,
      "5" =  "Supertoto",
      "6" =  "Games VS",
      "7" = "Games bwin",
      "8" =  "Casino Chartwell" )
    
    require(scales)
    ggplot(data=datamart, aes_string(x = TSVarName, y = TWVarName, color = "Gender"))+
      geom_point()+geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
      scale_x_continuous(labels = comma) + labs(x = paste("Total",input$StakesVsBets, gamesDict[input$Game], sep = " "), 
                                                y = paste("Total Winnings",gamesDict[input$Game], sep=" ")) +
      if(input$StakesVsBets == "stakes"){geom_abline(slope=1)}})
  
  output$RFM <- renderPlot({
    if (input$RFMPlot == "p1")  {print(rfm_rm_plot(rfm_score))}   
    if (input$RFMPlot == "p2")  {print(rfm_fm_plot(rfm_score))}
    if (input$RFMPlot == "p3")  {print(rfm_rf_plot(rfm_score))}  
    if (input$RFMPlot == "p4")  {print(rfm_histograms(rfm_score))}
    if (input$RFMPlot == "p5")  {print(rfm_heatmap(rfm_score))} 
    if (input$RFMPlot == "p6")  {print(rfm_bar_chart(rfm_score))}
    
  })
  output$TOPN <- renderPlot({
    if (input$TOPNplot == "t1")  {print(t10countriesusers)}
    if (input$TOPNplot == "t4")  {print(t10countriesbets)}
    if (input$TOPNplot == "t2")  {print(t10countriesstakes)}
    if (input$TOPNplot == "t3")  {print(t10countrieswinnings)}
  })
  output$TOPNApp <- renderPlot({
    if (input$TOPNAppplot == "a1")  {print(t5Appsusers)}
    if (input$TOPNAppplot == "a4")  {print(t5Appsbets)}
    if (input$TOPNAppplot == "a2")  {print(t5Appsstakes)}
    if (input$TOPNAppplot == "a3")  {print(t5AppsWinnings)}
  })
 output$CSPlot <- renderPlotly ({
   customer_loyalty
 })
 output$Poker5 <- renderPlot({
   DataPoker <- datamart %>% 
     filter(NumberOfTransactions_Buy_3 <= 1500, between(NetAmount_3, -20000,20000))
   
   ggplot(data=DataPoker, aes(x=NumberOfTransactions_Buy_3, y=NetAmount_3, color = Gender)) +
     geom_point()+  geom_smooth() + labs(x = "Net Win/Loss", y = "Number of Bets")
   
 })
}

shinyApp(ui=ui, server = server)
