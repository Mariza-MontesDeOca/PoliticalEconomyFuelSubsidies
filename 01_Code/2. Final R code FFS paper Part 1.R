#### _________________________________________________________________________________####
####     Diff in Diff Callaway and Sant'Anna multiple treatment                      ####
#### _________________________________________________________________________________####
# This code can only be run from section 3. with the data. We use proprietary data for the political,
# economic variables.

### ___________________________________________________________________________________________####
###  1.  Gasoline subsidies and taxes in Mexico: Figure 1                                       ####
### ___________________________________________________________________________________________####
#### 1.1 Importing Libraries ####
# set working directory
gc()
rm(list = ls())
setwd("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices")
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(expss)
library(ggplot2)
library("siebanxicor")
library(lubridate)
#### 1.2 Importing fuel prices in Mexico ####

#loading the data from 1990 to 1994
setwd("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices")
price_90_94 <- read.csv("precios de gasolina Magna 90-94.csv")
str(price_90_94)
price_90_94$magna <- price_90_94$regular_price/1000 #cambiando de viejos pesos a nuevos pesos
price_90_94$day <- rep(1,60)
#pasting
price_90_94$date <- as.Date(paste(price_90_94$year, price_90_94$month, price_90_94$day, sep="/"), "%Y/%m/%d")
#ready! 
price_90_94$premium <- rep(NA,60)
price_90_94$diesel <- rep(NA,60)
#seleccionar las mismas columnas 
price_90_94 <- dplyr::select(price_90_94, date, magna, premium, diesel)

#loading the data from 1995 to 1999
price_95_99 <- as.data.frame(read_excel("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/precios_gasolinas_1995_1999.xls",
                                        skip=7))
price_95_99 <- price_95_99[-c(1,7:8),]
names(price_95_99)[names(price_95_99) == '...1'] <- 'fuel'
price_95_99 <- as.data.frame(t(price_95_99))
#change first row to be the header
price_95_99$date <- rownames(price_95_99)
price_95_99 <-  dplyr::select(price_95_99, "date", "magna"="2","premium"="3","diesel"="4")
price_95_99 <- price_95_99[-1,]
#dealing with dates
price_95_99$month <- substr(price_95_99$date,1,3)
price_95_99$year <- substr(price_95_99$date,5,9)
#Replacing text with values
price_95_99$month <- gsub("Ene","01", price_95_99$month)                  
price_95_99$month <- gsub("Feb","02", price_95_99$month)                  
price_95_99$month <- gsub("Mar","03", price_95_99$month)                  
price_95_99$month <- gsub("Abr","04", price_95_99$month)                  
price_95_99$month <- gsub("May","05", price_95_99$month)                  
price_95_99$month <- gsub("Jun","06", price_95_99$month)                  
price_95_99$month <- gsub("Jul","07", price_95_99$month)                  
price_95_99$month <- gsub("Ago","08", price_95_99$month)                  
price_95_99$month <- gsub("Sep","09", price_95_99$month)                  
price_95_99$month <- gsub("Oct","10", price_95_99$month)                  
price_95_99$month <- gsub("Nov","11", price_95_99$month)                  
price_95_99$month <- gsub("Dic","12", price_95_99$month) 
price_95_99$day <- rep(1,60)
#pasting
price_95_99$date <- as.Date(paste(price_95_99$year, price_95_99$month, price_95_99$day, sep="/"), "%Y/%m/%d")
price_95_99 <- dplyr::select(price_95_99, date, magna, premium, diesel)
#ready! 


#loading the data from 2000 to 2010
price_00_10 <- as.data.frame(read_excel("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/precios_gasolinas_2000_2010.xls",
                                        skip=7))
price_00_10 <- price_00_10[-c(1,7:8),]
names(price_00_10)[names(price_00_10) == '...1'] <- 'fuel'
price_00_10 <- as.data.frame(t(price_00_10))
#change first row to be the header
price_00_10$date <- rownames(price_00_10)
price_00_10 <-  dplyr::select(price_00_10, "date", "magna"="2","premium"="3","diesel"="4")
price_00_10 <- price_00_10[-1,]
#dealing with dates
price_00_10$month <- substr(price_00_10$date,1,3)
price_00_10$year <- substr(price_00_10$date,5,9)
#Replacing text with values
price_00_10$month <- gsub("Ene","01", price_00_10$month)                  
price_00_10$month <- gsub("Feb","02", price_00_10$month)                  
price_00_10$month <- gsub("Mar","03", price_00_10$month)                  
price_00_10$month <- gsub("Abr","04", price_00_10$month)                  
price_00_10$month <- gsub("May","05", price_00_10$month)                  
price_00_10$month <- gsub("Jun","06", price_00_10$month)                  
price_00_10$month <- gsub("Jul","07", price_00_10$month)                  
price_00_10$month <- gsub("Ago","08", price_00_10$month)                  
price_00_10$month <- gsub("Sep","09", price_00_10$month)                  
price_00_10$month <- gsub("Oct","10", price_00_10$month)                  
price_00_10$month <- gsub("Nov","11", price_00_10$month)                  
price_00_10$month <- gsub("Dic","12", price_00_10$month) 
price_00_10$day <- rep(1,120)
#pasting
price_00_10$date <- as.Date(paste(price_00_10$year, price_00_10$month, price_00_10$day, sep="/"), "%Y/%m/%d")
price_00_10 <- dplyr::select(price_00_10, date, magna, premium, diesel)
#ready! 

#loading the data from 2010 to 2015
price_10_15 <- as.data.frame(read_excel("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/precios_gasolinas_2010_2015.xls",
                                        skip=7))
price_10_15 <- price_10_15[-c(1,7:8),]
names(price_10_15)[names(price_10_15) == '...1'] <- 'fuel'
price_10_15 <- as.data.frame(t(price_10_15))
#change first row to be the header
price_10_15$date <- rownames(price_10_15)
price_10_15 <-  dplyr::select(price_10_15, "date", "magna"="2","premium"="3","diesel"="4")
price_10_15 <- price_10_15[-1,]
#dealing with dates
price_10_15$month <- substr(price_10_15$date,1,3)
price_10_15$year <- substr(price_10_15$date,5,9)
#Replacing text with values
price_10_15$month <- gsub("Ene","01", price_10_15$month)                  
price_10_15$month <- gsub("Feb","02", price_10_15$month)                  
price_10_15$month <- gsub("Mar","03", price_10_15$month)                  
price_10_15$month <- gsub("Abr","04", price_10_15$month)                  
price_10_15$month <- gsub("May","05", price_10_15$month)                  
price_10_15$month <- gsub("Jun","06", price_10_15$month)                  
price_10_15$month <- gsub("Jul","07", price_10_15$month)                  
price_10_15$month <- gsub("Ago","08", price_10_15$month)                  
price_10_15$month <- gsub("Sep","09", price_10_15$month)                  
price_10_15$month <- gsub("Oct","10", price_10_15$month)                  
price_10_15$month <- gsub("Nov","11", price_10_15$month)                  
price_10_15$month <- gsub("Dic","12", price_10_15$month) 
price_10_15$day <- rep(1,72)
#pasting
price_10_15$date <- as.Date(paste(price_10_15$year, price_10_15$month, price_10_15$day, sep="/"), "%Y/%m/%d")
price_10_15 <- dplyr::select(price_10_15, date, magna, premium, diesel)
#ready! 

#loading the data of 2016
price_16 <- as.data.frame(read_excel("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/precios_gasolinas_2016.xlsx"))
price_16 <- price_16[-1,-c(5:7)]
price_16$year <- substr(price_16$Date, 1,4)
price_16$month <- substr(price_16$Date, 6,7)
price_16$day <- rep(1,12)
price_16$date <- as.Date(paste(price_16$year, price_16$month, price_16$day, sep="/"), "%Y/%m/%d")
price_16 <- dplyr::select(price_16, date, magna=Magna, premium=Premium, diesel=Diesel)

#loading the data from 2017 to 2024
price_17_24 <- as.data.frame(read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/precios_gasolina_diario.csv"))

price_17_24 <- dplyr::select(price_17_24, date=fecha, "magna"="gasolina_regular", "premium"="gasolina_premium", "diesel"="diesel")

price_17_24$year <- substr(price_17_24$date, 1,4)

price_17_24$month <- substr(price_17_24$date, 6,7)

price_17_24$day <- substr(price_17_24$date, 9,10)

price_17_24$date <- as.Date(paste(price_17_24$year, price_17_24$month, price_17_24$day, sep="/"), "%Y/%m/%d") 

library(lubridate)
price_17_24 <- price_17_24 %>% group_by(date = lubridate::floor_date(date,"month")) %>% summarize(magna=mean(magna),
                                                                                                  premium=mean(premium),
                                                                                                  diesel=mean(diesel))
#rbinding the price data
prices <- rbind(price_90_94, price_95_99, price_00_10, price_10_15, price_16, price_17_24)
rm(price_90_94, price_95_99, price_00_10, price_10_15, price_16, price_17_24)

#### 1.3 Importing reference prices (US prices) ####
### Obtaining reference price
url2<-"https://www.eia.gov/dnav/pet/hist_xls/EER_EPMRU_PF4_RGC_DPGd.xls"
destfile2<-"/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/Us_gulf_coast_regular_spot_fob_daily_1986_2021.xls"
download.file(url2,destfile2)
usprice <- as.data.frame(read_excel("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/Us_gulf_coast_regular_spot_fob_daily_1986_2021.xls",
                                    sheet=2, skip=2))
usprice<-dplyr::select(usprice, date="Date",
                       us_gulf_price="U.S. Gulf Coast Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon)")
usprice$year <- substr(usprice$date, 1,4)
usprice$month <- substr(usprice$date, 6,7)
usprice$day <- substr(usprice$date, 9,10)
usprice     <- usprice %>% 
  dplyr::mutate(year=as.numeric(year)) %>% 
  dplyr::filter(year>= 1990)
usprice$date <- as.Date(paste(usprice$year, usprice$month, usprice$day, sep="/"), "%Y/%m/%d")
?floor_date
library(lubridate)
usprice <- usprice %>% 
  group_by(date = lubridate::floor_date(date,"month")) %>% 
  summarize(us_gulf_price=mean(us_gulf_price))
prices<-left_join(prices, usprice)
rm(usprice)
# write_csv(prices, "prices_mexico_us_2024.csv", append=TRUE, col_names = TRUE)

#### 1.4 Importing THE MX USD exchange rates ####
###Downloading the exchange rate from Banxico
#install.packages("siebanxicor")
library("siebanxicor")
#Obtain your token here: https://www.banxico.org.mx/SieAPIRest/service/v1/token
#Replace the token in "SetToken" below
setToken("ae753417aeaa633bbf384dc438e78bf3731760fed00d2d6366ce9ada0009b1cb")
idSeries<-c("SF17908")
exchange_rate<-as.data.frame(getSeriesData("SF17908", '1990-01-01', '2024-12-01'))
colnames(exchange_rate)
exchange_rate$date <- as.Date(exchange_rate$SF17908.date, format="%Y/%m/%d")
exchange_rate<-dplyr::select(exchange_rate, date, exchange_rate="SF17908.value" )
# write_csv(exchange_rate, "/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/exchage_rate.csv", append=TRUE, col_names = TRUE)
prices<- dplyr::left_join(prices, exchange_rate)

#Doing operations in the dataset 
prices$us_gulf_price_mxn_cif <- (((prices$us_gulf_price*prices$exchange_rate)/3.78541)*1.09)*1.24

prices<-apply_labels(prices,
                     magna="Price low-octane MXN/l",
                     premium="Price high-octane MXN/l",
                     diesel="Price diesel MXN/l",
                     us_gulf_price="US Gulf Coast Regular Spot FOB USD/Gallon",
                     exchange_rate="Pesos per USD",
                     us_gulf_price_mxn_cif="US Gulf Coast Regular Spot CIF MXN/l")

# write_csv(prices, "/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/prices_mexico_us_labelled_V2.csv",col_names = TRUE)


#### 1.5 Mexico price gap plot ####
prices =read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/prices_mexico_us_labelled_V2.csv")

###Doing the plot
prices<-as.data.frame(prices)
str(prices)
prices[prices == 0] <- NA
str(prices)
prices$diff<-as.numeric(prices$magna) - prices$us_gulf_price_mxn_cif
prices <-prices %>% mutate_if(is.character, as.numeric)
#Plot!
salmon3
slategray3
ggplot(data = prices,aes(date,magna))+
  geom_ribbon(data=prices[prices$diff>=0 & prices$date<=as.Date("2005-07-15", "%Y-%m-%d"), ], #the first green part
              aes(ymax=magna, ymin=us_gulf_price_mxn_cif), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff<0 & prices$date>=as.Date("2005-08-01", "%Y-%m-%d") & #the first red part
                            prices$date<=as.Date("2005-10-01", "%Y-%m-%d"), ],
              aes(ymin=magna, ymax=us_gulf_price_mxn_cif, fill='lightsalmon'), alpha=0.4, show.legend = F)  +
  
  geom_ribbon(data=prices[prices$diff>=0 & prices$date<=as.Date("2006-02-15", "%Y-%m-%d") &
                            prices$date>as.Date("2005-11-01", "%Y-%m-%d"), ], #the second green part
              aes(ymax=magna, ymin=us_gulf_price_mxn_cif), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff<0 & prices$date>=as.Date("2006-03-01", "%Y-%m-%d") & #the second red part
                            prices$date<=as.Date("2006-08-01", "%Y-%m-%d"), ],
              aes(ymin=magna, ymax=us_gulf_price_mxn_cif, fill='lightsalmon'), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff>=0 & prices$date>=as.Date("2006-09-01", "%Y-%m-%d") &
                            prices$date<=as.Date("2007-02-01", "%Y-%m-%d"), ],
              aes(ymax=magna, ymin=us_gulf_price_mxn_cif), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff<0 & prices$date>=as.Date("2007-01-01", "%Y-%m-%d") & #the third red part
                            prices$date<=as.Date("2009-01-01", "%Y-%m-%d"), ],
              aes(ymin=magna, ymax=us_gulf_price_mxn_cif, fill='lightsalmon'), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff>=0 & prices$date>=as.Date("2008-11-01", "%Y-%m-%d") & # green 
                            prices$date<=as.Date("2009-04-01", "%Y-%m-%d"), ],
              aes(ymax=magna, ymin=us_gulf_price_mxn_cif), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff<0 & prices$date>=as.Date("2009-01-01", "%Y-%m-%d") &
                            prices$date<=as.Date("2013-08-30", "%Y-%m-%d"), ],#the fourth red part
              aes(ymin=magna, ymax=us_gulf_price_mxn_cif, fill='lightsalmon'), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff>=0 & prices$date>=as.Date("2014-01-01", "%Y-%m-%d") &
                            prices$date<=as.Date("2022-02-01", "%Y-%m-%d"), ], 
              aes(ymax=magna, ymin=us_gulf_price_mxn_cif), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff<0 & prices$date>=as.Date("2022-02-01", "%Y-%m-%d") &
                            prices$date<=as.Date("2022-09-30", "%Y-%m-%d"), ],#the fourth red part
              aes(ymin=magna, ymax=us_gulf_price_mxn_cif, fill='lightsalmon'), alpha=0.4, show.legend = F) +
  
  geom_ribbon(data=prices[prices$diff>=0 & prices$date>=as.Date("2022-08-01", "%Y-%m-%d") &
                            prices$date<=as.Date("2024-12-01", "%Y-%m-%d"), ], 
              aes(ymax=magna, ymin=us_gulf_price_mxn_cif), alpha=0.4, show.legend = F) +
  geom_segment(aes(as.Date("2005/07/15", "%Y/%m/%d") , y = 0, 
                   xend = as.Date("2005/07/15", "%Y/%m/%d"), yend =29), colour="black", linetype=2)+ 
  geom_segment(aes(as.Date("2009/12/12", "%Y/%m/%d") , y = 0, 
                   xend = as.Date("2009/12/12", "%Y/%m/%d"), yend =27.5), colour="black", linetype=2)+ 
  geom_segment(aes(as.Date("2014/02/15", "%Y/%m/%d") , y = 0, 
                   xend = as.Date("2014/02/15", "%Y/%m/%d"), yend =26), colour="black", linetype=2)+ 
  geom_segment(aes(as.Date("2017/02/15", "%Y/%m/%d") , y = 0, 
                   xend = as.Date("2017/02/15", "%Y/%m/%d"), yend =24.5), colour="black", linetype=2)+ 
  geom_segment(aes(as.Date("2020/03/15", "%Y/%m/%d") , y = 0, 
                   xend = as.Date("2020/03/15", "%Y/%m/%d"), yend =23.7), colour="black", linetype=2)+ 
  geom_segment(aes(as.Date("2022/03/15", "%Y/%m/%d") , y = 0, 
                   xend = as.Date("2022/03/15", "%Y/%m/%d"), yend =22), colour="black", linetype=2)+ 
  geom_text(aes(x=as.Date("1998-10-15", "%Y-%m-%d"), label="Taxes", y=28.5), colour="darkgray", angle=0, size=3.2) +
  geom_text(aes(x=as.Date("2007-07-15", "%Y-%m-%d"), label="Subsidy growth", y=28.5), colour="darkgray", angle=0, size=3.2) +
  geom_text(aes(x=as.Date("2012/06/01", "%Y/%m/%d"), label="Subsidy phase-out", y=27), colour="darkgray", angle=0, size=3.2) +
  geom_text(aes(x=as.Date("2016/07/30", "%Y/%m/%d"), label="Carbon + excise tax", y=25.2), colour="darkgray", angle=0, size=3.2) +
  geom_text(aes(x=as.Date("2018/06/15", "%Y/%m/%d"), label="Market lib.", y=24), colour="darkgray", angle=0, size=3.2) +
  geom_text(aes(x=as.Date("2021/03/01", "%Y/%m/%d"), label="Lockdown", y=23), colour="darkgray", angle=0, size=1.8) +
  geom_text(aes(x=as.Date("2021/03/01", "%Y/%m/%d"), label=" + recession", y=22.5), colour="darkgray", angle=0, size=1.8) +
  geom_text(aes(x=as.Date("2023/04/30", "%Y/%m/%d"), label="War in", y=4), colour="darkgray", angle=0, size=3.2) +
  geom_text(aes(x=as.Date("2023/04/30", "%Y/%m/%d"), label="Ukraine", y=3), colour="darkgray", angle=0, size=3.2) +
  geom_line(aes(y = magna, color = "gray"), size=2) +
  geom_line(aes(y = us_gulf_price_mxn_cif, color = "black"), size=1.3)+
  theme_classic() +
  labs(y="Prices in MXN/l",
       x="Date",
       color="Prices") +
  theme(legend.position="bottom")+
  scale_color_manual(labels = c( "Reference price (U.S. Gulf)", "Domestic gasoline price"), values = c("black", "royalblue4")) +
  scale_x_date(date_labels="%Y",date_breaks  ="2 year",
               limits = c(as.Date("1990-01-01"), as.Date("2025-12-01")),
               expand = c(0, 0))


?geom_point

ggsave(path="/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/10_Figures",
       filename="Implicit_subsidy_2024_12.png", width=10, height=5)


### ___________________________________________________________________________________________####
###  2.  Gasoline subsidies and taxes in Bolivia: Figure 2                                       ####
### ___________________________________________________________________________________________####
#### 2.1. Importing libraries ####
# set working directory
gc()
rm(list = ls())
setwd("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/")
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(expss)
library(ggplot2)
source("05_Scripts/transforming_date.R")

#### 2.2. Importing OLADE'S data####

#Important: prices from OLADE for Bol are in (US$/bbl)
bolivia               <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_bolivia.xlsx",sheet=2,skip=296))
bolivia               <- bolivia[c(234:539),] #data set of the other fuels
colnames(bolivia)     <- bolivia[1,]
bolivia               <- bolivia[-1,]
colnames(bolivia)[1]  <- "date"
bolivia               <- bolivia %>% dplyr::select(date, lo_oct_usd_olade="Gasolina Regular" )
#Setting date in date format 
bolivia$date        <- transforming_date(bolivia$date)
bolivia$year        <- substr(bolivia$date, 6,9)
bolivia$month       <- substr(bolivia$date, 1,2)
bolivia$day         <- "01"
bolivia$date        <- as.Date(paste(bolivia$year, bolivia$month, bolivia$day, sep="/"), "%Y/%m/%d")
#There are some holes in the Bolivia data for prices, since 20017 04, therefore we make two cuts
bolivia_full = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2022/11/01"), "months"))
#left joining
bolivia              = left_join(bolivia_full, bolivia)
rm(bolivia_full)
cols.num = c("lo_oct_usd_olade","year", "month", "day")
bolivia[cols.num]    <- sapply(bolivia[cols.num],as.numeric)
sapply(bolivia, class)
#selecting variables 
bolivia              <- bolivia %>%  dplyr::select(date,lo_oct_usd_olade)

#### 2.3. Converting to local currency, importing OLADE exchange rate####
#we have usd/bbl, we need first to convert to BOL/bbl
#Loading OLADE exchange rate
exchange              <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=347))
exchange              <- exchange[c(1:333),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"

exchange$date        <- transforming_date(exchange$date)
exchange$year        <- substr(exchange$date, 6,9)
exchange$month       <- substr(exchange$date, 1,2)
exchange$day         <- "01"
exchange$date        <- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange             = exchange %>% drop_na(date)

bolivia              <- left_join(bolivia, exchange %>% dplyr::select(date, exchange_olade), by="date")

rm(exchange)
bolivia[,2:3]    <- sapply(bolivia[,2:3],as.numeric)



#### 2.4. Importingand preparing another exchange rate from the central Bank of Bolivia ####
df_tmp           <- readxl::read_xlsx("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/exchange_Rate_BOB_USD_BoliviaCentralBank.xlsx")

#Replacing text with values
df_tmp$month    <- gsub("Enero","01", df_tmp$month)                  
df_tmp$month    <- gsub("Febrero","02", df_tmp$month)                  
df_tmp$month    <- gsub("Marzo","03", df_tmp$month)                  
df_tmp$month    <- gsub("Abril","04", df_tmp$month)                  
df_tmp$month    <- gsub("Mayo","05", df_tmp$month)                  
df_tmp$month    <- gsub("Junio","06", df_tmp$month)                  
df_tmp$month    <- gsub("Julio","07", df_tmp$month)                  
df_tmp$month    <- gsub("Agosto","08", df_tmp$month)                  
df_tmp$month    <- gsub("Septiembre","09", df_tmp$month)                  
df_tmp$month    <- gsub("Octubre","10", df_tmp$month)                  
df_tmp$month    <- gsub("Noviembre","11", df_tmp$month)                  
df_tmp$month    <- gsub("Diciembre","12", df_tmp$month) 
#Creating the date variable
df_tmp$date     <- as.Date(paste(df_tmp$year, df_tmp$month, df_tmp$day, sep="/"), "%Y/%m/%d")
#Calculating the average per month
library(lubridate)

df_tmp        <- df_tmp %>% 
  group_by(date = lubridate::floor_date(date,"month")) %>%  #this is similar to group by month, but we keep the date format
  summarize(exchange_rate_CB=mean(exchange_rate))

#Left_joining
bolivia      <- left_join(bolivia, df_tmp)

rm(df_tmp)


#### 2.4. Transforming to local currency####
#Coalesce the two datasets, given priority to the CB one
bolivia=bolivia %>% mutate(exchange_both=coalesce(exchange_rate_CB, exchange_olade))

bolivia$lo_oct_local_olade = bolivia$lo_oct_usd_olade*bolivia$exchange_both

bolivia$lo_oct_local_olade_Xlast = bolivia$lo_oct_usd_olade*6.86


#Here both variables are in BBL, EITHER USD/BBL OR BOLIVIANOS/BBL. TO CONVERT TO LITERS WE DIVIDE BY 158.987
bolivia = bolivia %>% dplyr::mutate(lo_oct_usd_olade=lo_oct_usd_olade/158.987,
                                    lo_oct_local_olade=lo_oct_local_olade/158.987,
                                    lo_oct_local_olade_Xlast=lo_oct_local_olade_Xlast/158.987)


#### 2.5. Importing govt reports data (self-collected)####
#obtaining the prices collected on my own and replacing NAs with these prices 
price.bol.own       = as.data.frame(read_csv("03_Data_Country by country/Bolivia/master_data_bolivia.csv")) %>% 
  dplyr::select(date, gasoline_price_low)

bolivia_full        = data.frame(date=seq(as.Date("2005/01/01"), as.Date("2022/11/01"), "months"))

price.bol.own       = left_join(bolivia_full,price.bol.own)

rm(bolivia_full)

price.bol.own       = price.bol.own %>% 
  dplyr::mutate(gasoline_price_low=ifelse(date>=as.Date("2018-09-01"),3.74,
                                          gasoline_price_low))
#THE PRICE HERE IS IN Bs/L, 
bolivia=full_join(bolivia, price.bol.own)

### VERY IMPORTANT STEP
bolivia[276,c("lo_oct_local_olade", "lo_oct_local_olade_Xlast")] <- NA #deleting the observed prices, these will be replaced with the announcement
#planned prices during the planned reform %>%  of Q4 2010
bolivia=bolivia %>% mutate(lo_oct_local_olade=coalesce(lo_oct_local_olade, gasoline_price_low))
bolivia=bolivia %>% mutate(lo_oct_local_olade_Xlast=coalesce(lo_oct_local_olade_Xlast, gasoline_price_low))

bolivia = bolivia %>% dplyr::select(date, lo_oct_local_olade, lo_oct_local_olade_Xlast, exchange_both)

rm(price.bol.own)

#OLADE SEEMS TO TAKE 6.97 FOR ALL YEARS


#### 2.6  Importing reference price ####
### Obtaining reference price
url2<-"https://www.eia.gov/dnav/pet/hist_xls/EER_EPMRU_PF4_RGC_DPGd.xls"
destfile2<-"/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/Us_gulf_coast_regular_spot_fob_daily_1986_2021.xls"
download.file(url2,destfile2)
usprice       <- as.data.frame(read_excel("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/Us_gulf_coast_regular_spot_fob_daily_1986_2021.xls",
                                          sheet=2, skip=2))
usprice       <-dplyr::select(usprice, date="Date",
                              us_gulf_price="U.S. Gulf Coast Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon)")
usprice$year  <- substr(usprice$date, 1,4)
usprice$month <- substr(usprice$date, 6,7)
usprice$day   <- substr(usprice$date, 9,10)
usprice       <- usprice %>% 
  dplyr::mutate(year=as.numeric(year)) %>% 
  dplyr::filter(year>= 1990)
usprice$date  <- as.Date(paste(usprice$year, usprice$month, usprice$day, sep="/"), "%Y/%m/%d")

library(lubridate)

usprice       <- usprice %>% 
  group_by(date = lubridate::floor_date(date,"month")) %>% 
  summarize(us_gulf_price=mean(us_gulf_price))

bolivia      <-left_join(bolivia, usprice)

rm(usprice)


#Doing operations in the dataset 
bolivia$us_gulf_price_bol_cif <- (((bolivia$us_gulf_price*bolivia$exchange_both)/3.78541)*1.09)*1.24
bolivia$us_gulf_price_bol_cif_ex_last <- ((bolivia$us_gulf_price*6.89/3.78541)*1.09)*1.24

bolivia<-apply_labels(bolivia,
                      lo_oct_local_olade="Price low-octane Bolivianos/l",
                      exchange_both="Bolivianos per USD",
                      us_gulf_price_bol_cif="US Gulf Coast Regular Spot CIF Bolivianos/l",
                      us_gulf_price_bol_cif_ex_last="US Gulf Coast Regular Spot CIF Bolivianos/l")

# write_csv(bolivia, "/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/01_prices/bolivia_us_labelled2.csv",col_names = TRUE, append=TRUE)

#### 2.7  Bolivia price gap plot ####
###Doing the plot
bolivia               <- as.data.frame(bolivia)

str(bolivia)
bolivia[bolivia == 0] <- NA
str(bolivia)
bolivia$diff          <- as.numeric(bolivia$lo_oct_local_olade) - bolivia$us_gulf_price_bol_cif
bolivia               <- bolivia %>% mutate_if(is.character, as.numeric) %>%  filter(date>"1990-01-01")
#Plot!
salmon3
slategray3
ggplot(data = bolivia, aes(date,lo_oct_local_olade))+
  geom_ribbon(data=bolivia[bolivia$diff>=0 & bolivia$date<=as.Date("2004-04-01", "%Y-%m-%d") , ],                   #first gray period
              aes(ymin=lo_oct_local_olade, ymax=us_gulf_price_bol_cif), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff<=0 & bolivia$date>=as.Date("2004-05-01", "%Y-%m-%d")&                       #now red period
                             bolivia$date<=as.Date("2008-12-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif,fill='lightsalmon'), alpha=0.4, show.legend = F)  + 
  geom_ribbon(data=bolivia[bolivia$diff>=0 & bolivia$date>=as.Date("2008-10-01", "%Y-%m-%d")&                       #now gray again
                             bolivia$date<=as.Date("2009-04-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff<=0 & bolivia$date>=as.Date("2009-05-01", "%Y-%m-%d")&                       #now red again
                             bolivia$date<=as.Date("2015-01-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif,fill='lightsalmon'), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff<=0 & bolivia$date>=as.Date("2015-02-01", "%Y-%m-%d")&                       #now red again
                             bolivia$date<=as.Date("2015-09-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif,fill='lightsalmon'), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff>=0 & bolivia$date>=as.Date("2015-09-01", "%Y-%m-%d")&                       #now gray again
                             bolivia$date<=as.Date("2016-12-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff<=0 & bolivia$date>=as.Date("2017-08-01", "%Y-%m-%d")&                       #now red again
                             bolivia$date<=as.Date("2018-12-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif,fill='lightsalmon'), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff>=0 & bolivia$date>=as.Date("2018-06-01", "%Y-%m-%d")&                       #now gray again
                             bolivia$date<=as.Date("2019-06-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff<=0 & bolivia$date>=as.Date("2019-03-01", "%Y-%m-%d")&                       #now red again
                             bolivia$date<=as.Date("2020-01-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif,fill='lightsalmon'), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff>=0 & bolivia$date>=as.Date("2020-01-01", "%Y-%m-%d")&                       #now gray again
                             bolivia$date<=as.Date("2021-06-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif), alpha=0.4, show.legend = F)  +
  geom_ribbon(data=bolivia[bolivia$diff<=0 & bolivia$date>=as.Date("2021-03-01", "%Y-%m-%d")&                       #now red again
                             bolivia$date<=as.Date("2022-11-01", "%Y-%m-%d") , ], 
              aes(ymin=lo_oct_local_olade,ymax=us_gulf_price_bol_cif,fill='lightsalmon'), alpha=0.4, show.legend = F)  +
  geom_line(aes(y = lo_oct_local_olade, color = "gray"), size=2) +
  geom_line(aes(y = us_gulf_price_bol_cif, color = "black"), size=1.3)+
  theme_classic() +
  labs(y="Precios BOB/l",
       x="Date",
       color="Bolivia") +
  theme(legend.position="bottom")+
  scale_color_manual(labels = c("Reference price (U.S. Gulf)","Domestic gasoline price"), values = c("royalblue4", "black")) +
  scale_x_date(date_labels="%Y",date_breaks  ="2 year")


?geom_point
# ggsave(path="/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/10_Figures",
#        filename="Implicit_subsidy_2022_11_08_bolivia.png", width=10, height=5)
# 



### ___________________________________________________________________________________________####
###  3.  Subsidy removal and presidential approval: Figure 3 YET TO BE ADDED                   ####
### ___________________________________________________________________________________________####

### ___________________________________________________________________________________________####
###  4.  Preparing and saving the regression  data                                            ####
### ___________________________________________________________________________________________####
#### 4.1 Load the libraries ####
rm(list=ls())
gc()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "lmtest", "xtable", "did"))
getwd()
setwd("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/")
data = read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv")
unique(data$country)
#### 4.2 Select the relevant variables for the SDID ####
data = dplyr::select(data, id=countryno, country, quarterly, ln_approval_smoothed,
                     ln_gdp_growth_imf,
                     ln_cpi, duration, duration.sqr,
                     SocioeconomicConditions_B, InternalConflict_D, 
                     Corruption_F, LawOrder_I, EthnicTensions_J ,
                     BureaucracyQuality_L,
                     ConsumerConfidence ,Poverty,
                     Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                     RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                     RiskDebtService,
                     EconomicRiskRating, FinancialRiskRating
)

data = data %>% filter(!country %in% c("Dominican Republic"))

unique(data$country)
#### 4.3 Transform the year and month into a zoo object ####
data = mutate(data, date = zoo::as.yearqtr(quarterly))
#### 4.4 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)


#### 4.5 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="Bolivia" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
# data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% mutate(first.treat=ifelse(is.na(first.treat), 0, first.treat))

data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% select(id, country, date.numeric, first.treat, ln_approval_smoothed, ln_gdp_growth_imf,
                       ln_cpi, duration, duration.sqr,
                       SocioeconomicConditions_B, InternalConflict_D, 
                       Corruption_F, LawOrder_I, EthnicTensions_J ,
                       BureaucracyQuality_L,
                       ConsumerConfidence ,Poverty,
                       Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                       RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                       RiskDebtService, EconomicRiskRating, FinancialRiskRating)
data = data %>% mutate(ln_Corruption_F=log(Corruption_F),
                       ln_SocioeconomicConditions_B=log(SocioeconomicConditions_B),
                       ln_InternalConflict_D=log(InternalConflict_D),
                       ln_LawOrder_I=log(LawOrder_I),
                       ln_BureaucracyQuality_L=log(BureaucracyQuality_L))
data = data %>% mutate(id=as.numeric(id))

data = data %>%  filter(date.numeric>2007 & date.numeric<2012.75)

unique(data$country)
 

### _____________________________________________________________________________________________####
###  5.  Ploting visual examination trends: Figure 4 Mexico                                     ####
### ___________________________________________________________________________________________####
#### 5.1 Load the libraries ####
rm(list=ls())

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "did"))
getwd()

#### 5.2 Load the data ####
data   <-   read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv") %>% 
  mutate(presidentno=dplyr::group_indices(.,presidents)) %>%  #creating president id 
  select(id=countryno, quarterly, presidentno,country,approval_smoothed,presidents,lo_oct_local_olade,
         ln_approval_smoothed,
         gdp_growth_imf, ip_imf, unemp_imf, unemp_growth_imf, cpi_imf, cpi_growth_imf,
         duration, duration.sqr, honey, 
         lo_oct_real,
         lo_oct_usd_olade, 
         SocioeconomicConditions_B, InternalConflict_D, 
         Corruption_F, LawOrder_I, EthnicTensions_J ,
         BureaucracyQuality_L,
         ConsumerConfidence ,Poverty,
         Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
         RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
         RiskDebtService,
         EconomicRiskRating, FinancialRiskRating
  ) #selecting vars


data = data %>% filter(!country %in% c("Dominican Republic"))

data   <-   data %>%
  group_by(presidentno) %>%
  mutate(mean_approval = mean(approval_smoothed, na.rm=T),
         demeaned = approval_smoothed - mean_approval) %>%  ungroup()

#### 5.3 Calculate rolling mean of all variables ####
data= data %>% group_by(country) %>%  mutate(approval_roll=c(NA,NA, rollmean(approval_smoothed, k=3)))
data= data %>% group_by(country) %>%  mutate(SocioeconomicConditions_B_roll=c(NA,NA, rollmean(SocioeconomicConditions_B, k=3)))
data= data %>% group_by(country) %>%  mutate(Corruption_F_roll=c(NA,NA, rollmean(Corruption_F, k=3)))
data= data %>% group_by(country) %>%  mutate(InternalConflict_D_roll=c(NA,NA, rollmean(InternalConflict_D, k=3)))
data= data %>% group_by(country) %>%  mutate(LawOrder_I_roll=c(NA,NA, rollmean(LawOrder_I, k=3)))
data= data %>% group_by(country) %>%  mutate(BureaucracyQuality_L_roll=c(NA,NA, rollmean(BureaucracyQuality_L, k=3)))


#### 5.4 Transform the year and month into a zoo object ####
data = dplyr::mutate(data, date = zoo::as.yearqtr(quarterly))
#### 5.5 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)
#### 5.6 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="Bolivia" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% mutate(id=as.numeric(id))
#data = data %>%  filter(date.numeric>2006 & date.numeric<2012.75)


mx = data %>% 
  filter(country!="Bolivia") %>% 
  mutate(group=ifelse(country=="Mexico", "treat", "control"),
         treat=ifelse(group=="treat", 1,0),
         exp=ifelse(date.numeric>=2009.75, 1, 0)) %>% 
  select(country, id, date.numeric, first.treat,quarterly,
         group, treat, exp,
         demeaned, approval_smoothed, ln_approval_smoothed,
         duration , duration.sqr ,
         SocioeconomicConditions_B ,InternalConflict_D   ,
         Corruption_F ,  LawOrder_I , 
         BureaucracyQuality_L,
         approval_roll,SocioeconomicConditions_B_roll, Corruption_F_roll,
         InternalConflict_D_roll, LawOrder_I_roll, BureaucracyQuality_L_roll) %>% 
  mutate(quarterly=lubridate::yq(quarterly),
         quarterly=as.yearqtr(quarterly))

str(mx$quarterly)

mx2 = mx %>%
  group_by(treat,quarterly) %>%
  summarize(approval = mean(approval_smoothed),
            demeaned = mean(demeaned),
            ln_approval=mean(ln_approval_smoothed),
            approval_roll=mean(approval_roll))


#### 5.9 Parallel trends plot with ln_approval, complete period ####
ggplot(mx2  %>% filter(quarterly<2012.75), aes(y=ln_approval,x=quarterly, group=as.factor(treat), 
                                               color=as.factor(treat))) +
  geom_point() + 
  geom_line()+
  theme_bw() +
  geom_vline(xintercept=2009.75,linetype="dashed", color = "red") +
  ylim(3,5)+
  theme_classic() +
  scale_color_manual(name="Treat", labels=c("Controls","Mexico"),
                     values=c("#121257","gray") )+
  labs(y="Approval ratings (log)", x="Date")+
  theme(axis.text.x=element_text(angle=-90))+
  scale_x_continuous(breaks=round(seq(min(as.numeric(mx2$quarterly)),max(as.numeric(mx2$quarterly)), by=0.25)))+
  annotate("rect", xmin=2007.25, xmax=2012.75, ymin=3, ymax=5, alpha=.1)+
  geom_text(aes(x=2012.3, label="DiD reg", y=4.95), colour="darkgray", angle=0, size=3)+
  geom_text(aes(x=2012.3, label="period", y=4.88), colour="darkgray", angle=0, size=3)+
  geom_text(aes(x=2009.1, label="Treatment", y=3.2), colour="red",  size=3)

# ggsave("10_Figures/Parallel_Trends_Mexico_ln_approval_complete_period_DomRepOut.png", width=10, height=5)




### _________________________________________________________________________________####
###  6.  Generating a DiD plot first for Bolivia ####
#### _________________________________________________________________________________####
#### 6.1 Load the libraries ####
rm(list=ls())

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "did"))
getwd()

rm(list=ls())
#### 6.2 Load the data ####
data   <-   read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv") %>% 
  mutate(presidentno=dplyr::group_indices(.,presidents)) %>%  #creating president id 
  select(id=countryno, quarterly, presidentno,country,approval_smoothed,presidents,lo_oct_local_olade,
         ln_approval_smoothed,
         gdp_growth_imf, ip_imf, unemp_imf, unemp_growth_imf, cpi_imf, cpi_growth_imf,
         duration, duration.sqr, honey, 
         lo_oct_real,
         lo_oct_usd_olade, 
         SocioeconomicConditions_B, InternalConflict_D, 
         Corruption_F, LawOrder_I, EthnicTensions_J ,
         BureaucracyQuality_L,
         ConsumerConfidence ,Poverty,
         Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
         RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
         RiskDebtService,
         EconomicRiskRating, FinancialRiskRating
  ) #selecting vars

data = data %>% filter(!country %in% c("Dominican Republic"))

data   <-   data %>%
  group_by(presidentno) %>%
  mutate(mean_approval = mean(approval_smoothed, na.rm=T),
         demeaned = approval_smoothed - mean_approval) %>%  ungroup()
#### 6.3 Calculate rolling mean of all variables ####
data= data %>% group_by(country) %>%  mutate(approval_roll=c(NA,NA, rollmean(approval_smoothed, k=3)))
data= data %>% group_by(country) %>%  mutate(SocioeconomicConditions_B_roll=c(NA,NA, rollmean(SocioeconomicConditions_B, k=3)))
data= data %>% group_by(country) %>%  mutate(Corruption_F_roll=c(NA,NA, rollmean(Corruption_F, k=3)))
data= data %>% group_by(country) %>%  mutate(InternalConflict_D_roll=c(NA,NA, rollmean(InternalConflict_D, k=3)))
data= data %>% group_by(country) %>%  mutate(LawOrder_I_roll=c(NA,NA, rollmean(LawOrder_I, k=3)))
data= data %>% group_by(country) %>%  mutate(BureaucracyQuality_L_roll=c(NA,NA, rollmean(BureaucracyQuality_L, k=3)))

#### 6.4 Transform the year and month into a zoo object ####
data = dplyr::mutate(data, date = zoo::as.yearqtr(quarterly))
#### 6.5 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)

#### 6.6 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="Bolivia" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% mutate(id=as.numeric(id))
#data = data %>%  filter(date.numeric>2006 & date.numeric<2012.75)



mx = data %>% 
  filter(country!="Mexico") %>% 
  mutate(group=ifelse(country=="Bolivia", "treat", "control"),
         treat=ifelse(group=="treat", 1,0),
         exp=ifelse(date.numeric>=2010.75, 1, 0)) %>% 
  select(country, id, date.numeric, first.treat, quarterly,
         group, treat, exp,
         approval_smoothed,demeaned,ln_approval_smoothed,
         Corruption_F , duration , duration.sqr ,
         SocioeconomicConditions_B ,InternalConflict_D   ,
         Corruption_F ,  LawOrder_I , 
         BureaucracyQuality_L,
         approval_roll,SocioeconomicConditions_B_roll, Corruption_F_roll,
         InternalConflict_D_roll, LawOrder_I_roll, BureaucracyQuality_L_roll) %>% 	
  mutate(quarterly=lubridate::yq(quarterly),	
         quarterly=as.yearqtr(quarterly))

mx2 = mx %>%	
  group_by(treat,quarterly) %>%	
  summarize(approval = mean(approval_smoothed),	
            demeaned = mean(demeaned),	
            ln_approval=mean(ln_approval_smoothed),
            approval_roll=mean(approval_roll))


#### 6.9 Parallel trends plot with ln_approval, complete period ####
ggplot(mx2 %>% filter(quarterly<2012.75), aes(y=ln_approval,x=quarterly, group=as.factor(treat), 
                                              color=as.factor(treat))) +
  geom_line() + 
  geom_point() + 
  theme_bw() +
  geom_vline(xintercept=2010.75,linetype="dashed", color = "red") +
  ylim(2,5)+
  theme_classic() +
  scale_color_manual(name="Treat", labels=c("Controls","Bolivia"),
                     values=c("#121257","gray") )+
  labs(y="Approval ratings (log)", x="Date")+
  theme(axis.text.x=element_text(angle=-90))+
  scale_x_continuous(breaks=round(seq(min(as.numeric(mx2$quarterly)),max(as.numeric(mx2$quarterly)), by=0.25)))+
  annotate("rect", xmin=2007.25, xmax=2012.75, ymin=2, ymax=5, alpha=.1)+
  geom_text(aes(x=2012.11, label="DiD reg", y=4.94), colour="darkgray", angle=0, size=3)+
  geom_text(aes(x=2012.11, label="period", y=4.81), colour="darkgray", angle=0, size=3)+
  geom_text(aes(x=2010.00, label="Treatment", y=2.1), colour="red",  size=3)
# ggsave("10_Figures/Parallel_Trends_Bolivia_ln_approval_complete_period_DomRepOut.png", width=10, height=5)

#### 6.10 Parallel trends plot with ln_approval, complete period ####
ggplot(mx2 %>% filter(quarterly<2012.75), aes(y=approval_roll,x=quarterly, group=as.factor(treat), 
                                              color=as.factor(treat))) +
  geom_line() + 
  geom_point() + 
  theme_bw() +
  geom_vline(xintercept=2010.75,linetype="dashed", color = "red") +
  ylim(15,80)+
  theme_classic() +
  scale_color_manual(name="Treat", labels=c("Controls","Bolivia"),
                     values=c("#121257","gray") )+
  labs(y="Approval rolling mean (k=3)", x="Date")+
  theme(axis.text.x=element_text(angle=-90))+
  scale_x_continuous(breaks=round(seq(min(as.numeric(mx2$quarterly)),max(as.numeric(mx2$quarterly)), by=0.25)))+
  annotate("rect", xmin=2007.25, xmax=2012.75, ymin=15, ymax=80, alpha=.1)+
  geom_text(aes(x=2012.2, label="DiD reg", y=78), colour="darkgray", angle=0, size=3)+
  geom_text(aes(x=2012.2, label="period", y=76), colour="darkgray", angle=0, size=3)+
  geom_text(aes(x=2009.4, label="Treatment", y=18), colour="red",  size=3)
# ggsave("10_Figures/Parallel_Trends_Bolivia_approval_roll_complete_period_DomRepOut.png", width=10, height=5)





### ___________________________________________________________________________________________####
###  7.  Descriptive statistics: Tables 2 and 3                                                ####
### ___________________________________________________________________________________________####
#### 7.1 Descriptives table by country: Table 2  ####
#### 1.1 Load the libraries ####
rm(list=ls())
gc()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "lmtest", "xtable", "did"))
getwd()
setwd("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/")
data = read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv")
unique(data$country)
#### 1.2 Select the relevant variables for the SDID ####
data = dplyr::select(data, id=countryno, country, quarterly, ln_approval_smoothed,
                     ln_gdp_growth_imf,
                     ln_cpi, duration, duration.sqr,
                     SocioeconomicConditions_B, InternalConflict_D, 
                     Corruption_F, LawOrder_I, EthnicTensions_J ,
                     BureaucracyQuality_L,
                     ConsumerConfidence ,Poverty,
                     Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                     RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                     RiskDebtService,
                     EconomicRiskRating, FinancialRiskRating
)

data = data %>% filter(!country %in% c("Dominican Republic"))

unique(data$country)
#### 1.3 Transform the year and month into a zoo object ####
data = mutate(data, date = zoo::as.yearqtr(quarterly))
#### 1.4 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)


#### 1.5 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="Bolivia" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
# data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% mutate(first.treat=ifelse(is.na(first.treat), 0, first.treat))

data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% select(id, country, date.numeric, first.treat, ln_approval_smoothed, ln_gdp_growth_imf,
                       ln_cpi, duration, duration.sqr,
                       SocioeconomicConditions_B, InternalConflict_D, 
                       Corruption_F, LawOrder_I, EthnicTensions_J ,
                       BureaucracyQuality_L,
                       ConsumerConfidence ,Poverty,
                       Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                       RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                       RiskDebtService, EconomicRiskRating, FinancialRiskRating)
data = data %>% mutate(ln_Corruption_F=log(Corruption_F),
                       ln_SocioeconomicConditions_B=log(SocioeconomicConditions_B),
                       ln_InternalConflict_D=log(InternalConflict_D),
                       ln_LawOrder_I=log(LawOrder_I),
                       ln_BureaucracyQuality_L=log(BureaucracyQuality_L))
data = data %>% mutate(id=as.numeric(id))

data = data %>%  filter(date.numeric>2007 & date.numeric<2012.75)

unique(data$country)

data = data %>% mutate(countries=case_when(country=="Mexico" ~"Mexico",
                                           country=="Bolivia" ~"Bolivia",
                                           TRUE ~ "Controls"))

descriptives = data %>% group_by(countries) %>% 
  dplyr::summarise(mean_approval=mean(exp(ln_approval_smoothed), na.rm=T),
                   mean_duration=mean(duration, na.rm=T),
                   mean_durationsqr=mean(duration.sqr, na.rm = T),
                   mean_corruption=mean(exp(ln_Corruption_F), na.rm=T),
                   mean_socioeconomic=mean(exp(ln_SocioeconomicConditions_B), na.rm=T),
                   mean_internalconflict=mean(exp(ln_InternalConflict_D), na.rm=T),
                   mean_laworder=mean(exp(ln_LawOrder_I), na.rm=T),
                   mean_bureaucracy=mean(exp(ln_BureaucracyQuality_L), na.rm=T),
                   variance_approval=var(exp(ln_approval_smoothed), na.rm=T),
                   variance_duration=var(duration, na.rm=T),
                   variance_durationsqr=var(duration.sqr, na.rm = T),
                   variance_corruption=var(exp(ln_Corruption_F), na.rm=T),
                   variance_socioeconomic=var(exp(ln_SocioeconomicConditions_B), na.rm=T),
                   variance_internalconflict=var(exp(ln_InternalConflict_D), na.rm=T),
                   variance_laworder=var(exp(ln_LawOrder_I), na.rm=T),
                   variance_bureaucracy=var(exp(ln_BureaucracyQuality_L), na.rm=T),
                   min_approval=min(exp(ln_approval_smoothed), na.rm=T),
                   min_duration=min(duration, na.rm=T),
                   min_durationsqr=min(duration.sqr, na.rm = T),
                   min_corruption=min(exp(ln_Corruption_F), na.rm=T),
                   min_socioeconomic=min(exp(ln_SocioeconomicConditions_B), na.rm=T),
                   min_internalconflict=min(exp(ln_InternalConflict_D), na.rm=T),
                   min_laworder=min(exp(ln_LawOrder_I), na.rm=T),
                   min_bureaucracy=min(exp(ln_BureaucracyQuality_L), na.rm=T),
                   max_approval=max(exp(ln_approval_smoothed), na.rm=T),
                   max_duration=max(duration, na.rm=T),
                   max_durationsqr=max(duration.sqr, na.rm = T),
                   max_corruption=max(exp(ln_Corruption_F), na.rm=T),
                   max_socioeconomic=max(exp(ln_SocioeconomicConditions_B), na.rm=T),
                   max_internalconflict=max(exp(ln_InternalConflict_D), na.rm=T),
                   max_laworder=max(exp(ln_LawOrder_I), na.rm=T),
                   max_bureaucracy=max(exp(ln_BureaucracyQuality_L), na.rm=T)) 

descriptives_long = descriptives %>% 
  pivot_longer(!c(countries),
               names_to=c("statistic", "variable"),
               names_sep="_",
               values_to="score")

descriptives_wide= descriptives_long %>% 
  pivot_wider(
    names_from=statistic,
    values_from=score) %>% 
  rename(Country=countries,
         Variable=variable,
         Mean=mean,
         Variance=variance,
         Max=max,
         Min=min)

### replacing variable names to make them prety
descriptives_wide$Variable <- gsub("approval","Presidential approval",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("duration","Term length",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("durationsqr","Term length squared",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("corruption","Corruption",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("socioeconomic","Socioeconomic conditions",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("internalconflict","Internal conflict ",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("laworder","Law and Order",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("bureaucracy","Bureaucracy Quality",descriptives_wide$Variable)
descriptives_wide$Variable <- gsub("Term lengthsqr","Term length squared",descriptives_wide$Variable)

#rbindind full sample and country-type sample
descriptives_final= rbind(descriptives_wide)

order=c("Full sample","Bolivia","Mexico","Controls")
descriptives_final$Country <- factor(descriptives_final$Country, levels=order)
#latex table
print(xtable(descriptives_final,
             caption="Descriptive statistics"), include.rownames = F)

?xtable

#### 7.2 Descriptives table DiD before-after with-out treatment: Table 3  ####
#### 1.1 Load the libraries ####
rm(list=ls())
gc()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "lmtest", "xtable", "did"))
getwd()

data = read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv")
unique(data$country)
#### 1.2 Select the relevant variables for the SDID ####
data = dplyr::select(data, id=countryno, country, quarterly, ln_approval_smoothed,
                     ln_gdp_growth_imf,
                     ln_cpi, duration, duration.sqr,
                     SocioeconomicConditions_B, InternalConflict_D, 
                     Corruption_F, LawOrder_I, EthnicTensions_J ,
                     BureaucracyQuality_L,
                     ConsumerConfidence ,Poverty,
                     Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                     RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                     RiskDebtService,
                     EconomicRiskRating, FinancialRiskRating
)

data = data %>% filter(!country %in% c("Dominican Republic"))

unique(data$country)
#### 1.3 Transform the year and month into a zoo object ####
data = mutate(data, date = zoo::as.yearqtr(quarterly))
#### 1.4 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)


#### 1.5 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="Bolivia" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
# data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% mutate(first.treat=ifelse(is.na(first.treat), 0, first.treat))

data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% select(id, country, date.numeric, first.treat, ln_approval_smoothed, ln_gdp_growth_imf,
                       ln_cpi, duration, duration.sqr,
                       SocioeconomicConditions_B, InternalConflict_D, 
                       Corruption_F, LawOrder_I, EthnicTensions_J ,
                       BureaucracyQuality_L,
                       ConsumerConfidence ,Poverty,
                       Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                       RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                       RiskDebtService, EconomicRiskRating, FinancialRiskRating)
data = data %>% mutate(ln_Corruption_F=log(Corruption_F),
                       ln_SocioeconomicConditions_B=log(SocioeconomicConditions_B),
                       ln_InternalConflict_D=log(InternalConflict_D),
                       ln_LawOrder_I=log(LawOrder_I),
                       ln_BureaucracyQuality_L=log(BureaucracyQuality_L))
data = data %>% mutate(id=as.numeric(id))

data = data %>%  filter(date.numeric>2007 & date.numeric<2012.75)

unique(data$country)



data = data %>% mutate(countries=case_when(country=="Mexico" ~"Mexico",
                                           country=="Bolivia" ~"Bolivia",
                                           TRUE ~ "Controls"))

descriptivesbeforemx = data %>% filter(date.numeric<= as.yearqtr("2009-12-30", format="%Y-%m-%d")) %>% 
  group_by(countries) %>% 
  dplyr::summarise(mean_approval_before=mean(exp(ln_approval_smoothed), na.rm=T)) %>% 
  filter(countries==c("Mexico", "Controls"))

descriptivesaftermx = data %>% filter(date.numeric> as.yearqtr("2009-12-30", format="%Y-%m-%d")) %>% 
  group_by(countries) %>% 
  dplyr::summarise(mean_approval_after=mean(exp(ln_approval_smoothed), na.rm=T))%>% 
  filter(countries==c("Mexico", "Controls"))

descriptivesbeforebol = data %>% filter(date.numeric<= as.yearqtr("2010-12-26", format="%Y-%m-%d")) %>% 
  group_by(countries) %>% 
  dplyr::summarise(mean_approval_before=mean(exp(ln_approval_smoothed), na.rm=T))%>% 
  dplyr::filter(countries!="Mexico")

descriptivesafterbol = data %>% filter(date.numeric> as.yearqtr("2010-12-26", format="%Y-%m-%d")) %>% 
  group_by(countries) %>% 
  dplyr::summarise(mean_approval_after=mean(exp(ln_approval_smoothed), na.rm=T))%>% 
  dplyr::filter(countries!="Mexico")

descriptivesbol=merge(descriptivesbeforebol, descriptivesafterbol, by="countries")
descriptivesbol$countries <- gsub("Controls","Controls (before Dec 2010)",descriptivesbol$countries)

descriptivesmx=merge(descriptivesbeforemx, descriptivesaftermx, by="countries")
descriptivesmx$countries <- gsub("Controls","Controls (before Dec 2009)",descriptivesmx$countries)

descriptives2=rbind(descriptivesbol, descriptivesmx)

#latex table
print(xtable(descriptives2), include.rownames = F)



### ___________________________________________________________________________________________####
###  8. Time-event plot - Effect of subsidy removal on presidential approval: Figure 6        ####
### _________________________________________________________________________________________####
#### 8.1 Estimate the group-time average treatment effects ####
out <- att_gt(yname = "ln_approval_smoothed",
              gname = "first.treat",
              idname = "id",
              tname = "date.numeric",
              xformla = ~ ln_Corruption_F + duration + duration.sqr +
                ln_SocioeconomicConditions_B + ln_InternalConflict_D   +
                ln_LawOrder_I+ ln_BureaucracyQuality_L,
              data = data,
              est_method = "reg",
              control_group="notyettreated",
              alp=0.10)

summary(out)
aggte(out, type="dynamic")
agg.gs <- aggte(out, type = "group")
?att_gt
ggdid(out) + 
  ylim(-1,1) +
  theme(axis.text.x = element_text(angle = 90, size=10))

out$t
#### 8.2 Getting the  group-time average treatment effects PLOT  ####
res1=tibble(group=out$group,
            time=out$t,
            coeff=out$att,
            std_err=out$se,
            t_value=coeff/std_err)%>% 
  dplyr::mutate(ci_lo=coeff-(std_err*1.96),
                ci_hi=coeff+(std_err*1.96)) %>% 
  mutate(group=case_when(group=="2009.75"~ "Mexico",
                         group=="2010.75"~ "Bolivia"),
         Time=case_when(group=="Mexico" & time < 2009.75 ~"pre",
                        group=="Mexico" & time >= 2009.75 ~"post",
                        group=="Bolivia" & time < 2010.75 ~"pre",
                        group=="Bolivia" & time >= 2010.75 ~"post"))

dat_text <- data.frame(label=c("Treatment", "Treatment"),
                       group=c("Bolivia", "Mexico"),
                       time=c(2010.00, 2011.00),
                       coeff=c(0.50,0.50))

str(res1)
a=ggplot(res1, aes(x=time, y=coeff,color=Time)) +
  geom_point() +
  facet_wrap(facets="group", nrow=5, ncol=1)+
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi, x=time), width=0.1)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  geom_vline(data=data.frame(xint=2009.64, group="Mexico"), 
             aes(xintercept=xint), linetype="dashed", color = "red") +
  geom_vline(data=data.frame(xint=2010.64, group="Bolivia"), 
             aes(xintercept=xint), linetype="dashed", color = "red")+
  theme_classic()+scale_color_manual(name="Treat", labels=c("Post","Pre"),
                                     values=c("#121257","gray") )

a
a + geom_text=data(dat_text, label=label)
?geom_errorbar


# ggsave("10_Figures/DiD_DomRep_out.png", width=10, height=7)


#### 8.3 Getting ATT ####
att = res1 %>%  filter(Time=="post") %>%  filter(coeff<0 & abs(t_value)>=1.96) %>% 
  group_by(group) %>% summarize(att=sum(coeff, na.rm=T)) %>% 
  mutate(att=case_when(group=="Mexico" ~ round(att/2,2),
                       group=="Bolivia"~ round(att/5,2)))


### ___________________________________________________________________________________________####
###  9.  Placebo United States: Figure 9,                                                     ####
### _________________________________________________________________________________________####
#### 9.1 Load the libraries ####
rm(list=ls())
gc()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "lmtest", "xtable", "did"))
getwd()

data = read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv")
unique(data$country)
#### 9.2 Select the relevant variables for the SDID ####
data = dplyr::select(data, id=countryno, country, quarterly, ln_approval_smoothed,
                     ln_gdp_growth_imf,
                     ln_cpi, duration, duration.sqr,
                     SocioeconomicConditions_B, InternalConflict_D, 
                     Corruption_F, LawOrder_I, EthnicTensions_J ,
                     BureaucracyQuality_L,
                     ConsumerConfidence ,Poverty,
                     Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                     RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                     RiskDebtService,
                     EconomicRiskRating, FinancialRiskRating
)

data = data %>% filter(!country %in% c("Dominican Republic"))

unique(data$country)
#### 9.3 Transform the year and month into a zoo object ####
data = mutate(data, date = zoo::as.yearqtr(quarterly))
#### 9.4 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)

data = data %>% filter(!country %in% c("Bolivia"))

#### 9.5 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="United States" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
# data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% mutate(first.treat=ifelse(is.na(first.treat), 0, first.treat))

data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% select(id, country, date.numeric, first.treat, ln_approval_smoothed, ln_gdp_growth_imf,
                       ln_cpi, duration, duration.sqr,
                       SocioeconomicConditions_B, InternalConflict_D, 
                       Corruption_F, LawOrder_I, EthnicTensions_J ,
                       BureaucracyQuality_L,
                       ConsumerConfidence ,Poverty,
                       Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                       RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                       RiskDebtService, EconomicRiskRating, FinancialRiskRating)
data = data %>% mutate(ln_Corruption_F=log(Corruption_F),
                       ln_SocioeconomicConditions_B=log(SocioeconomicConditions_B),
                       ln_InternalConflict_D=log(InternalConflict_D),
                       ln_LawOrder_I=log(LawOrder_I),
                       ln_BureaucracyQuality_L=log(BureaucracyQuality_L))
data = data %>% mutate(id=as.numeric(id))

data = data %>%  filter(date.numeric>2007 & date.numeric<2012.75)

unique(data$country)
#### 9.6 Estimate the group-time average treatment effects ####
out <- att_gt(yname = "ln_approval_smoothed",
              gname = "first.treat",
              idname = "id",
              tname = "date.numeric",
              xformla = ~ ln_Corruption_F + duration + duration.sqr +
                ln_SocioeconomicConditions_B + ln_InternalConflict_D   +
                ln_LawOrder_I+ ln_BureaucracyQuality_L,
              data = data,
              est_method = "reg",
              control_group="notyettreated",
              alp=0.10)

summary(out)
aggte(out, type="dynamic")
agg.gs <- aggte(out, type = "group")
?att_gt
ggdid(out) + 
  ylim(-1,1) +
  theme(axis.text.x = element_text(angle = 90, size=10))

out$t
#### 9.7 Getting the  group-time average treatment effects PLOT  ####
res1=tibble(group=out$group,
            time=out$t,
            coeff=out$att,
            std_err=out$se,
            t_value=coeff/std_err)%>% 
  dplyr::mutate(ci_lo=coeff-(std_err*1.96),
                ci_hi=coeff+(std_err*1.96)) %>% 
  mutate(group=case_when(group=="2009.75"~ "Mexico",
                         group=="2010.75"~ "United States"),
         Time=case_when(group=="Mexico" & time < 2009.75 ~"pre",
                        group=="Mexico" & time >= 2009.75 ~"post",
                        group=="United States" & time < 2010.75 ~"pre",
                        group=="United States" & time >= 2010.75 ~"post"))

dat_text <- data.frame(label=c("Treatment", "Treatment"),
                       group=c("United States", "Mexico"),
                       time=c(2010.00, 2011.00),
                       coeff=c(0.50,0.50))

str(res1)
a=ggplot(res1, aes(x=time, y=coeff,color=Time)) +
  geom_point() +
  facet_wrap(facets="group", nrow=5, ncol=1)+
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi, x=time), width=0.1)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  geom_vline(data=data.frame(xint=2009.64, group="Mexico"), 
             aes(xintercept=xint), linetype="dashed", color = "red") +
  geom_vline(data=data.frame(xint=2010.64, group="United States"), 
             aes(xintercept=xint), linetype="dashed", color = "red")+
  theme_classic()+scale_color_manual(name="Treat", labels=c("Post","Pre"),
                                     values=c("#121257","gray") )

a
a + geom_text=data(dat_text, label=label)
?geom_errorbar



#### 9.8. Filtering to get only US ####
res1 <- res1 %>% filter(group=="United States")

dat_text <- data.frame(label=c("Treatment"),
                       group=c("United States"),
                       time=c(2010.00),
                       coeff=c(0.50))

a=ggplot(res1, aes(x=time, y=coeff,color=Time)) +
  geom_point() +
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi, x=time), width=0.1)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  geom_vline(data=data.frame(xint=2010.64, group="United States"), 
             aes(xintercept=xint), linetype="dashed", color = "red")+
  theme_classic()+scale_color_manual(name="Treat", labels=c("Post","Pre"),
                                     values=c("#121257","gray") ) +
  ylim(-1, 1)

a


# ggsave("10_Figures/US_placebo.png", width=10, height=7)



#### 9.8 Getting ATT ####
att = res1 %>%  filter(Time=="post") %>%  filter(coeff<0 & abs(t_value)>=1.96) %>% 
  group_by(group) %>% summarize(att=sum(coeff, na.rm=T)) %>% 
  mutate(att=case_when(group=="Mexico" ~ round(att/2,2),
                       group=="Bolivia"~ round(att/5,2)))



## ___________________________________________________________________________________________####
#### X.  Robustness: Empirical evidence at the m                         ####
#### _________________________________________________________________________________________####
#### 2.1 Load the libraries ####
rm(list=ls())
gc()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "lmtest", "xtable", "did"))
getwd()

data = read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv")
unique(data$country)
#### 2.2 Select the relevant variables for the SDID ####
data   <-   read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv") %>% 
  mutate(presidentno=dplyr::group_indices(.,presidents)) %>%  #creating president id 
  select(id=countryno, quarterly, presidentno,country,approval_smoothed,presidents,lo_oct_local_olade,
         gdp_growth_imf, ip_imf, unemp_imf, unemp_growth_imf, cpi_imf, cpi_growth_imf,
         duration, duration.sqr, honey, 
         lo_oct_real,
         lo_oct_usd_olade, 
         SocioeconomicConditions_B, InternalConflict_D, 
         Corruption_F, LawOrder_I, EthnicTensions_J ,
         BureaucracyQuality_L,
         ConsumerConfidence ,Poverty,
         Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
         RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
         RiskDebtService,
         EconomicRiskRating, FinancialRiskRating
  ) #selecting vars

data   <-   data %>%
  group_by(presidentno) %>%
  mutate(mean_approval = mean(approval_smoothed, na.rm=T),
         demeaned = approval_smoothed - mean_approval) %>%  ungroup()

data = data %>% filter(!country %in% c("Dominican Republic"))

unique(data$country)
#### 2.3 Transform the year and month into a zoo object ####
data = mutate(data, date = zoo::as.yearqtr(quarterly))
#### 2.4 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)
#### 2.5 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="United States" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% mutate(ln_Corruption_F=log(Corruption_F),
                       ln_SocioeconomicConditions_B=log(SocioeconomicConditions_B),
                       ln_InternalConflict_D=log(InternalConflict_D),
                       ln_LawOrder_I=log(LawOrder_I),
                       ln_BureaucracyQuality_L=log(BureaucracyQuality_L))
data = data %>% mutate(id=as.numeric(id))

data = data %>%  filter(date.numeric>2007 & date.numeric<2012.75)

unique(data$country)
#### 2.6 Estimate the group-time average treatment effects ####
out <- att_gt(yname = "demeaned",
              gname = "first.treat",
              idname = "id",
              tname = "date.numeric",
              xformla = ~ Corruption_F + duration + duration.sqr +
                SocioeconomicConditions_B + InternalConflict_D   +
                LawOrder_I+ BureaucracyQuality_L,
              data = data,
              est_method = "reg",
              control_group="notyettreated",
              alp=0.10)

summary(out)

?att_gt
ggdid(out) + 
  #ylim(-1,1) +
  theme(axis.text.x = element_text(angle = 90, size=10))

out$t
#### 2.7 Getting the  group-time average treatment effects PLOT  ####
res1=tibble(group=out$group,
            time=out$t,
            coeff=out$att,
            std_err=out$se,
            t_value=coeff/std_err)%>% 
  dplyr::mutate(ci_lo=coeff-(std_err*1.96),
                ci_hi=coeff+(std_err*1.96)) %>% 
  mutate(group=case_when(group=="2009.75"~ "Mexico",
                         group=="2010.75"~ "Bolivia"),
         Time=case_when(group=="Mexico" & time < 2009.75 ~"pre",
                        group=="Mexico" & time >= 2009.75 ~"post",
                        group=="Bolivia" & time < 2010.75 ~"pre",
                        group=="Bolivia" & time >= 2010.75 ~"post"))

dat_text <- data.frame(label=c("Treatment", "Treatment"),
                       group=c("Bolivia", "Mexico"),
                       time=c(2010.00, 2011.00),
                       coeff=c(0.50,0.50))

str(res1)
a=ggplot(res1, aes(x=time, y=coeff,color=Time)) +
  geom_point() +
  facet_wrap(facets="group", nrow=5, ncol=1)+
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi, x=time), width=0.1)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  geom_vline(data=data.frame(xint=2009.64, group="Mexico"), 
             aes(xintercept=xint), linetype="dashed", color = "red") +
  geom_vline(data=data.frame(xint=2010.64, group="Bolivia"), 
             aes(xintercept=xint), linetype="dashed", color = "red")+
  theme_classic()+scale_color_manual(name="Treat", labels=c("Post","Pre"),
                                     values=c("#121257","gray") )

a
a + geom_text=data(dat_text, label=label)
jowId ?geom_errorbar


ggsave("10_Figures/DiD_Mx_withCovariates_Demeaned_approval.png", width=10, height=5)


#### 2,8 Getting ATT ####
att = res1 %>%  filter(Time=="post") %>%  filter(coeff<0 & abs(t_value)>=1.96) %>% 
  group_by(group) %>% summarize(att=sum(coeff, na.rm=T)) %>% 
  mutate(att=case_when(group=="Mexico" ~ round(att/6,2),
                       group=="Bolivia"~ round(att/5,2)))







## ___________________________________________________________________________________________####
#### X Robustness: Keeping out Dom. Rep. - RAW APPROVAL                   ####
#### _________________________________________________________________________________________####
#### X.1 Load the libraries ####
rm(list=ls())
gc()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "lmtest", "xtable", "did"))
getwd()

data = read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv")
unique(data$country)
#### X.2 Select the relevant variables for the SDID ####
data = dplyr::select(data, id=countryno, country, quarterly, ln_approval_smoothed,approval_smoothed,
                     ln_gdp_growth_imf,
                     ln_cpi, duration, duration.sqr,
                     SocioeconomicConditions_B, InternalConflict_D, 
                     Corruption_F, LawOrder_I, EthnicTensions_J ,
                     BureaucracyQuality_L,
                     ConsumerConfidence ,Poverty,
                     Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                     RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                     RiskDebtService,
                     EconomicRiskRating, FinancialRiskRating
)

data = data %>% filter(!country %in% c("Dominican Republic"))

unique(data$country)
#### X.3 Transform the year and month into a zoo object ####
data = mutate(data, date = zoo::as.yearqtr(quarterly))
#### X.4 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)
#### X.5 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="Bolivia" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d")))
data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% select(id, country, date.numeric, first.treat, ln_approval_smoothed, approval_smoothed,
                       ln_gdp_growth_imf,
                       ln_cpi, duration, duration.sqr,
                       SocioeconomicConditions_B, InternalConflict_D, 
                       Corruption_F, LawOrder_I, EthnicTensions_J ,
                       BureaucracyQuality_L,
                       ConsumerConfidence ,Poverty,
                       Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                       RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                       RiskDebtService, EconomicRiskRating, FinancialRiskRating)
data = data %>% mutate(ln_Corruption_F=log(Corruption_F),
                       ln_SocioeconomicConditions_B=log(SocioeconomicConditions_B),
                       ln_InternalConflict_D=log(InternalConflict_D),
                       ln_LawOrder_I=log(LawOrder_I),
                       ln_BureaucracyQuality_L=log(BureaucracyQuality_L))
data = data %>% mutate(id=as.numeric(id))

data = data %>%  filter(date.numeric>2007 & date.numeric<2012.75)

unique(data$country)
#### X.6 Estimate the group-time average treatment effects ####
out <- att_gt(yname = "approval_smoothed",
              gname = "first.treat",
              idname = "id",
              tname = "date.numeric",
              xformla = ~ Corruption_F + duration + duration.sqr +
                SocioeconomicConditions_B + InternalConflict_D   +
                LawOrder_I+ BureaucracyQuality_L,
              data = data,
              est_method = "reg",
              control_group="notyettreated",
              alp=0.10)

summary(out)

?att_gt
ggdid(out) + 
  ylim(-1,1) +
  theme(axis.text.x = element_text(angle = 90, size=10))

out$t
#### X.7 Getting the  group-time average treatment effects PLOT  ####
res1=tibble(group=out$group,
            time=out$t,
            coeff=out$att,
            std_err=out$se,
            t_value=coeff/std_err)%>% 
  dplyr::mutate(ci_lo=coeff-(std_err*1.96),
                ci_hi=coeff+(std_err*1.96)) %>% 
  mutate(group=case_when(group=="2009.75"~ "Mexico",
                         group=="2010.75"~ "Bolivia"),
         Time=case_when(group=="Mexico" & time < 2009.75 ~"pre",
                        group=="Mexico" & time >= 2009.75 ~"post",
                        group=="Bolivia" & time < 2010.75 ~"pre",
                        group=="Bolivia" & time >= 2010.75 ~"post"))

dat_text <- data.frame(label=c("Treatment", "Treatment"),
                       group=c("Bolivia", "Mexico"),
                       time=c(2010.00, 2011.00),
                       coeff=c(0.50,0.50))

str(res1)
a=ggplot(res1, aes(x=time, y=coeff,color=Time)) +
  geom_point() +
  facet_wrap(facets="group", nrow=5, ncol=1)+
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi, x=time), width=0.1)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  geom_vline(data=data.frame(xint=2009.64, group="Mexico"), 
             aes(xintercept=xint), linetype="dashed", color = "red") +
  geom_vline(data=data.frame(xint=2010.64, group="Bolivia"), 
             aes(xintercept=xint), linetype="dashed", color = "red")+
  theme_classic()+scale_color_manual(name="Treat", labels=c("Post","Pre"),
                                     values=c("#121257","gray") )

a
a + geom_text=data(dat_text, label=label)
?geom_errorbar


ggsave("10_Figures/DiD_Mx_withCovariates_raw_approval.png", width=10, height=5)


#### X,8 Getting ATT ####
att = res1 %>%  filter(Time=="post") %>%  filter(coeff<0 & abs(t_value)>=1.96) %>% 
  group_by(group) %>% summarize(att=sum(coeff, na.rm=T)) %>% 
  mutate(att=case_when(group=="Mexico" ~ round(att/2,2),
                       group=="Bolivia"~ round(att/5,2)))



## ___________________________________________________________________________________________####
#### X Robustness: Keeping out Dom. Rep. - MOVING AVERAGE          ####
#### _________________________________________________________________________________________####
#### X.1 Load the libraries ####
rm(list=ls())
gc()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("tidyverse", "synthdid","plm","zoo", "lmtest", "xtable", "did"))
getwd()

data = read_csv("03_Data_Country by country/Generated_data/Panel_Countries_ICRG_Covariates.csv")
unique(data$country)
#### X.2 Select the relevant variables for the SDID ####
data = dplyr::select(data, id=countryno, country, quarterly, ln_approval_smoothed,approval_smoothed,
                     ln_gdp_growth_imf,
                     ln_cpi, duration, duration.sqr,
                     SocioeconomicConditions_B, InternalConflict_D, 
                     Corruption_F, LawOrder_I, EthnicTensions_J ,
                     BureaucracyQuality_L,
                     ConsumerConfidence ,Poverty,
                     Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                     RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                     RiskDebtService,
                     EconomicRiskRating, FinancialRiskRating
)

data = data %>% filter(!country %in% c("Dominican Republic"))

unique(data$country)


#### X.3 Calculate rolling mean of all variables ####
data= data %>% group_by(country) %>%  mutate(approval_roll=c(NA,NA, rollmean(approval_smoothed, k=3)))
data= data %>% group_by(country) %>%  mutate(SocioeconomicConditions_B_roll=c(NA,NA, rollmean(SocioeconomicConditions_B, k=3)))
data= data %>% group_by(country) %>%  mutate(Corruption_F_roll=c(NA,NA, rollmean(Corruption_F, k=3)))
data= data %>% group_by(country) %>%  mutate(InternalConflict_D_roll=c(NA,NA, rollmean(InternalConflict_D, k=3)))
data= data %>% group_by(country) %>%  mutate(LawOrder_I_roll=c(NA,NA, rollmean(LawOrder_I, k=3)))
data= data %>% group_by(country) %>%  mutate(BureaucracyQuality_L_roll=c(NA,NA, rollmean(BureaucracyQuality_L, k=3)))


# View(data %>%  select(quarterly, country, duration, approval_smoothed, approval_roll))

#### X.3 Transform the year and month into a zoo object ####
data = mutate(data, date = zoo::as.yearqtr(quarterly))
#### X.4 Balance the panel  ####
data=pdata.frame(data, index=c("id", "quarterly"), drop.index=F, row.names=T)
data=make.pbalanced(data, balance.type="shared.times", index=c("countryno", "quarterly"))
is.pbalanced(data)
#### X.5 Define the first.treatment variable ####
data = data %>% dplyr::mutate(first.treat=case_when(country=="Mexico"~ as.yearqtr("2009-12-30", format="%Y-%m-%d"),
                                                    country=="Bolivia" ~ as.yearqtr("2010-12-26", format="%Y-%m-%d"),
                                                    TRUE ~ as.numeric(0)))
data = data %>% mutate(first.treat=ifelse(first.treat=="0 Q1", 0, first.treat))
data = data %>% mutate(date.numeric=as.numeric(date))
data = data %>% mutate(first.treat=as.numeric(first.treat))
data = data %>% select(id, country, date.numeric, first.treat, ln_approval_smoothed, approval_smoothed,
                       ln_gdp_growth_imf,
                       ln_cpi, duration, duration.sqr,
                       SocioeconomicConditions_B, InternalConflict_D, 
                       Corruption_F, LawOrder_I, EthnicTensions_J ,
                       BureaucracyQuality_L,
                       ConsumerConfidence ,Poverty,
                       Unemployment , CivilWar ,Terrorism, CivilDisorder , RiskGDPGrowth, 
                       RiskInflation,RiskBudgetBalance,RiskExchangeRateStability,
                       RiskDebtService, EconomicRiskRating, FinancialRiskRating,
                       approval_roll,SocioeconomicConditions_B_roll, Corruption_F_roll,
                       InternalConflict_D_roll, LawOrder_I_roll, BureaucracyQuality_L_roll)

data = data %>% mutate(ln_Corruption_F=log(Corruption_F),
                       ln_SocioeconomicConditions_B=log(SocioeconomicConditions_B),
                       ln_InternalConflict_D=log(InternalConflict_D),
                       ln_LawOrder_I=log(LawOrder_I),
                       ln_BureaucracyQuality_L=log(BureaucracyQuality_L))
data = data %>% mutate(id=as.numeric(id))

data = data %>%  filter(date.numeric>2007 & date.numeric<2012.75)

unique(data$country)
#### X.6 Estimate the group-time average treatment effects ####
out <- att_gt(yname = "approval_roll",
              gname = "first.treat",
              idname = "id",
              tname = "date.numeric",
              xformla = ~ Corruption_F_roll + duration + duration.sqr +
                SocioeconomicConditions_B_roll + InternalConflict_D_roll   +
                LawOrder_I_roll+ BureaucracyQuality_L_roll,
              data = data,
              est_method = "reg",
              control_group="notyettreated",
              alp=0.10)

summary(out)

?att_gt
ggdid(out) + 
  ylim(-1,1) +
  theme(axis.text.x = element_text(angle = 90, size=10))

out$t
#### X.7 Getting the  group-time average treatment effects PLOT  ####
res1=tibble(group=out$group,
            time=out$t,
            coeff=out$att,
            std_err=out$se,
            t_value=coeff/std_err)%>% 
  dplyr::mutate(ci_lo=coeff-(std_err*1.96),
                ci_hi=coeff+(std_err*1.96)) %>% 
  mutate(group=case_when(group=="2009.75"~ "Mexico",
                         group=="2010.75"~ "Bolivia"),
         Time=case_when(group=="Mexico" & time < 2009.75 ~"pre",
                        group=="Mexico" & time >= 2009.75 ~"post",
                        group=="Bolivia" & time < 2010.75 ~"pre",
                        group=="Bolivia" & time >= 2010.75 ~"post"))

dat_text <- data.frame(label=c("Treatment", "Treatment"),
                       group=c("Bolivia", "Mexico"),
                       time=c(2010.00, 2011.00),
                       coeff=c(0.50,0.50))

str(res1)
a=ggplot(res1, aes(x=time, y=coeff,color=Time)) +
  geom_point() +
  facet_wrap(facets="group", nrow=5, ncol=1)+
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi, x=time), width=0.1)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  geom_vline(data=data.frame(xint=2009.64, group="Mexico"), 
             aes(xintercept=xint), linetype="dashed", color = "red") +
  geom_vline(data=data.frame(xint=2010.64, group="Bolivia"), 
             aes(xintercept=xint), linetype="dashed", color = "red")+
  theme_classic()+scale_color_manual(name="Treat", labels=c("Post","Pre"),
                                     values=c("#121257","gray") )

a
a + geom_text=data(dat_text, label=label)
?geom_errorbar


ggsave("10_Figures/DiD_Mx_withCovariates_rolling_average.png", width=10, height=5)


#### X,8 Getting ATT ####
att = res1 %>%  filter(Time=="post") %>%  filter(coeff<0 & abs(t_value)>=1.96) %>% 
  group_by(group) %>% summarize(att=sum(coeff, na.rm=T)) %>% 
  mutate(att=case_when(group=="Mexico" ~ round(att/2,2),
                       group=="Bolivia"~ round(att/5,2)))

