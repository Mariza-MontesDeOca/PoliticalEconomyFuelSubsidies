##########################

# Project code for FORESEE: FFS study 

##########################
#### 0. Importing libraries ----------------------------------------------------------------
gc()
rm(list = ls())
getwd()
setwd("/Users/montesdeoca/Dropbox/PoliticalEconomyFuelSubsidies")
library(foreign)
library(zoo)
library(dplyr)
library(ggplot2)
library(data.table)
library(compare)
library(lubridate)
library(tibbletime)
library(pracma)
library(readxl)
library(tidyverse)
library(tidyr)
library(expss)
library(xtable)
library(digest)
library(tidyr)
source("05_Scripts/transforming_date.R")
options(scipen=999)
##########################

##########################

####################################################
##### 1.  Loading data on approval ################ -----------------------------------------------------------------------------
####################################################
##### 1.1 Loading data and cleaning data       ####
getwd()
# setwd("D:/users/mmontesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/Mexico_gasoline/01_Data")
data <- read.csv("02_RawData/EAD+2.0+quarter+101019.csv")
#formatting dates
data$quarterly = as.yearqtr(data$qtr,format="%Yq%q")
data$last_month = as.Date(data$quarterly, frac=1)
#filtering countries out and renaming variasbles
colnames(data)
unique(data$Country)
target <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic",
            "Ecuador", "ElSalvador", "Guatemala", "Honduras", "Mexico", "Nicaragua",
            "Panama", "Paraguay", "Peru", "United States", "Uruguay")
data <- data %>% dplyr::select("country"="Country", "qtr","last_month","quarterly", "year", "quarter", "approval_smoothed"="Approval_Smoothed", 
                               "approval_not_smoothed"="Approval_Not_Smoothed", "net_smoothed"="NET_Smoothed",
                               "net_not_smoothed"="NET_Not_Smoothed", "relative_smoothed"="Relative_Smoothed",
                               "relative_not_smoothed"="Relative_Not_Smoothed")  %>% 
  dplyr::filter(country %in% target)  
#order data 
data = data[order(data$country, data$last_month), ] 



####################################################
##### 2.  Adding president/term dummy vars ######### -----------------------------------------------------------------------------
####################################################
##### 2.1 Argentina ####
Argentina <- data %>%
  filter(country == "Argentina") %>%
  mutate(presidents = case_when(
      last_month > as.Date("11/30/1983", format = "%m/%d/%Y") & last_month <= as.Date("07/08/1989", format = "%m/%d/%Y") ~ "Alfonsin",
      last_month > as.Date("07/08/1989", format = "%m/%d/%Y") & last_month <= as.Date("07/08/1995", format = "%m/%d/%Y") ~ "Menem_first",
      last_month > as.Date("07/08/1995", format = "%m/%d/%Y") & last_month <= as.Date("12/10/1999", format = "%m/%d/%Y") ~ "Menem_second",
      last_month > as.Date("12/10/1999", format = "%m/%d/%Y") & last_month <= as.Date("12/20/2001", format = "%m/%d/%Y") ~ "Delarua",
      last_month > as.Date("12/30/2001", format = "%m/%d/%Y") & last_month <= as.Date("05/25/2003", format = "%m/%d/%Y") ~ "Duhalde",
      last_month > as.Date("05/25/2003", format = "%m/%d/%Y") & last_month <= as.Date("12/10/2007", format = "%m/%d/%Y") ~ "Kirchner",
      last_month > as.Date("12/10/2007", format = "%m/%d/%Y") & last_month <= as.Date("12/10/2011", format = "%m/%d/%Y") ~ "Fernandezdek_first",
      last_month > as.Date("12/10/2011", format = "%m/%d/%Y") & last_month <= as.Date("12/10/2015", format = "%m/%d/%Y") ~ "Fernandezdek_second",
      last_month > as.Date("12/10/2015", format = "%m/%d/%Y") & last_month <= as.Date("12/10/2019", format = "%m/%d/%Y") ~ "Macri",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Alfonsin", "Menem_first", "Menem_second", "Delarua",
                 "Duhalde", "Kirchner", "Fernandezdek_first", "Fernandezdek_second", "Macri"),
      ordered = TRUE))

Argentina <- Argentina %>%
  group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Argentina$presidents)

##### 2.2 Bolivia ####
Bolivia <- data %>%
  filter(country == "Bolivia") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1997-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2001-08-07", format = "%Y-%m-%d") ~ "Banzer",
      last_month > as.Date("2001-08-07", format = "%Y-%m-%d") & last_month <= as.Date("2002-08-06", format = "%Y-%m-%d") ~ "Quiroga",
      last_month > as.Date("2002-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2003-08-17", format = "%Y-%m-%d") ~ "Sanchez",
      last_month > as.Date("2003-08-17", format = "%Y-%m-%d") & last_month <= as.Date("2005-03-09", format = "%Y-%m-%d") ~ "Mesa",
      last_month > as.Date("2005-03-09", format = "%Y-%m-%d") & last_month <= as.Date("2006-01-22", format = "%Y-%m-%d") ~ "Rodriguez",
      last_month > as.Date("2006-01-21", format = "%Y-%m-%d") & last_month <= as.Date("2010-01-22", format = "%Y-%m-%d") ~ "Evo_first",
      last_month > as.Date("2010-01-22", format = "%Y-%m-%d") & last_month <= as.Date("2015-01-22", format = "%Y-%m-%d") ~ "Evo_second",
      last_month > as.Date("2015-01-22", format = "%Y-%m-%d") & last_month <= as.Date("2019-11-10", format = "%Y-%m-%d") ~ "Evo_third",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Banzer", "Quiroga", "Sanchez", "Mesa", "Rodriguez", 
                 "Evo_first", "Evo_second", "Evo_third", "Otro"),
      ordered = TRUE))

Bolivia <- Bolivia %>%
  group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Bolivia$presidents)

##### 2.3 Brazil ####
Brazil <- data %>%
  filter(country == "Brazil") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1978-03-15", format = "%Y-%m-%d") & last_month <= as.Date("1985-04-14", format = "%Y-%m-%d") ~ "Figuereido",
      last_month > as.Date("1985-04-21", format = "%Y-%m-%d") & last_month <= as.Date("1990-03-14", format = "%Y-%m-%d") ~ "Sarney",
      last_month > as.Date("1990-03-15", format = "%Y-%m-%d") & last_month <= as.Date("1992-12-29", format = "%Y-%m-%d") ~ "Collor",
      last_month > as.Date("1992-12-29", format = "%Y-%m-%d") & last_month <= as.Date("1994-12-31", format = "%Y-%m-%d") ~ "Franco",
      last_month > as.Date("1994-12-31", format = "%Y-%m-%d") & last_month <= as.Date("1998-12-31", format = "%Y-%m-%d") ~ "Henrique_first",
      last_month > as.Date("1998-12-31", format = "%Y-%m-%d") & last_month <= as.Date("2002-12-31", format = "%Y-%m-%d") ~ "Henrique_second",
      last_month > as.Date("2002-12-31", format = "%Y-%m-%d") & last_month <= as.Date("2006-12-31", format = "%Y-%m-%d") ~ "Lula_first",
      last_month > as.Date("2006-12-31", format = "%Y-%m-%d") & last_month <= as.Date("2010-12-31", format = "%Y-%m-%d") ~ "Lula_second",
      last_month > as.Date("2010-12-31", format = "%Y-%m-%d") & last_month <= as.Date("2014-12-31", format = "%Y-%m-%d") ~ "Rousseff_first",
      last_month > as.Date("2014-12-31", format = "%Y-%m-%d") & last_month <= as.Date("2016-08-31", format = "%Y-%m-%d") ~ "Rousseff_second",
      last_month > as.Date("2016-08-31", format = "%Y-%m-%d") & last_month <= as.Date("2018-12-31", format = "%Y-%m-%d") ~ "Temer",
      last_month > as.Date("2018-12-31", format = "%Y-%m-%d") & last_month <= as.Date("2020-08-31", format = "%Y-%m-%d") ~ "Bolsonaro",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Figuereido", "Sarney", "Collor", "Franco", "Henrique_first", "Henrique_second",
                 "Lula_first", "Lula_second", "Rousseff_first", "Rousseff_second", "Temer", "Bolsonaro"),
      ordered = TRUE))

Brazil <- Brazil %>%
  group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Brazil$presidents)

##### 2.4 Chile ####
Chile <- data %>%
  filter(country == "Chile") %>%
  mutate( presidents = case_when(
      last_month > as.Date("1990-03-11", format = "%Y-%m-%d") & last_month <= as.Date("1994-03-11", format = "%Y-%m-%d") ~ "Aylwin",
      last_month > as.Date("1994-03-11", format = "%Y-%m-%d") & last_month <= as.Date("2000-03-11", format = "%Y-%m-%d") ~ "Frei",
      last_month > as.Date("2000-03-11", format = "%Y-%m-%d") & last_month <= as.Date("2006-03-11", format = "%Y-%m-%d") ~ "Lagos",
      last_month > as.Date("2006-03-11", format = "%Y-%m-%d") & last_month <= as.Date("2010-03-11", format = "%Y-%m-%d") ~ "Bachelet_first",
      last_month > as.Date("2010-03-11", format = "%Y-%m-%d") & last_month <= as.Date("2014-03-11", format = "%Y-%m-%d") ~ "Piñera_first",
      last_month > as.Date("2014-03-11", format = "%Y-%m-%d") & last_month <= as.Date("2018-03-11", format = "%Y-%m-%d") ~ "Bachelet_second",
      last_month > as.Date("2018-03-11", format = "%Y-%m-%d") & last_month <= as.Date("2022-03-11", format = "%Y-%m-%d") ~ "Piñera_second",
      TRUE ~ NA_character_  ),
    presidents = factor( presidents,
      levels = c("Aylwin", "Frei", "Lagos", "Bachelet_first", "Piñera_first", "Bachelet_second", "Piñera_second", "Otro"),
      ordered = TRUE))

Chile <- Chile %>%
  group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Chile$presidents)
##### 2.5 Colombia ####
Colombia <- data %>%
  filter(country == "Colombia") %>%
  mutate( presidents = case_when(
      last_month > as.Date("1990-08-06", format = "%Y-%m-%d") & last_month <= as.Date("1994-08-06", format = "%Y-%m-%d") ~ "Gaviria",
      last_month > as.Date("1994-08-06", format = "%Y-%m-%d") & last_month <= as.Date("1998-08-06", format = "%Y-%m-%d") ~ "Samper",
      last_month > as.Date("1998-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2002-08-06", format = "%Y-%m-%d") ~ "Pastrana",
      last_month > as.Date("2002-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2006-08-06", format = "%Y-%m-%d") ~ "Uribe_first",
      last_month > as.Date("2006-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2010-08-06", format = "%Y-%m-%d") ~ "Uribe_second",
      last_month > as.Date("2010-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2014-08-06", format = "%Y-%m-%d") ~ "Santos_first",
      last_month > as.Date("2014-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2018-08-06", format = "%Y-%m-%d") ~ "Santos_second",
      last_month > as.Date("2018-08-06", format = "%Y-%m-%d") & last_month <= as.Date("2022-08-06", format = "%Y-%m-%d") ~ "Duque",
      TRUE ~ NA_character_ ),
    presidents = factor(
      presidents,
      levels = c(
        "Gaviria", "Samper", "Pastrana", "Uribe_first", "Uribe_second",
        "Santos_first", "Santos_second", "Duque" ),
      ordered = TRUE ))

Colombia <- Colombia %>%
  group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Chile$presidents)
 
##### 2.6 Costa Rica ####
CostaRica <- data %>%
  filter(country == "Costa Rica") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1978-05-08", format = "%Y-%m-%d") & last_month <= as.Date("1982-05-08", format = "%Y-%m-%d") ~ "Carazo",
      last_month > as.Date("1982-05-08", format = "%Y-%m-%d") & last_month <= as.Date("1986-05-08", format = "%Y-%m-%d") ~ "Monge",
      last_month > as.Date("1986-05-08", format = "%Y-%m-%d") & last_month <= as.Date("1990-05-08", format = "%Y-%m-%d") ~ "Arias",
      last_month > as.Date("1990-05-08", format = "%Y-%m-%d") & last_month <= as.Date("1994-05-08", format = "%Y-%m-%d") ~ "Calderon_cr",
      last_month > as.Date("1994-05-08", format = "%Y-%m-%d") & last_month <= as.Date("1998-05-08", format = "%Y-%m-%d") ~ "Figueres",
      last_month > as.Date("1998-05-08", format = "%Y-%m-%d") & last_month <= as.Date("2002-05-08", format = "%Y-%m-%d") ~ "Rodriguez_cr",
      last_month > as.Date("2002-05-08", format = "%Y-%m-%d") & last_month <= as.Date("2006-05-08", format = "%Y-%m-%d") ~ "Pacheco",
      last_month > as.Date("2006-05-08", format = "%Y-%m-%d") & last_month <= as.Date("2010-05-08", format = "%Y-%m-%d") ~ "Arias_second",
      last_month > as.Date("2010-05-08", format = "%Y-%m-%d") & last_month <= as.Date("2014-05-08", format = "%Y-%m-%d") ~ "Chinchilla",
      last_month > as.Date("2014-05-08", format = "%Y-%m-%d") & last_month <= as.Date("2018-05-08", format = "%Y-%m-%d") ~ "Solis",
      last_month > as.Date("2018-05-08", format = "%Y-%m-%d") & last_month <= as.Date("2020-05-08", format = "%Y-%m-%d") ~ "Alvarado",
      TRUE ~ NA_character_),
   
     presidents = factor(presidents,
      levels = c("Carazo", "Monge", "Arias", "Calderon_cr", "Figueres", "Rodriguez_cr",
        "Pacheco", "Arias_second", "Chinchilla", "Solis", "Alvarado"), ordered = TRUE))


CostaRica <- CostaRica %>%
  group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(CostaRica$presidents)
##### 2.7 Dominican Republic ####
DominicanRepublic <- data %>%
  filter(country == "Dominican Republic") %>%
  mutate(presidents = case_when(
      last_month > as.Date("2000-08-16", format = "%Y-%m-%d") & last_month <= as.Date("2004-08-15", format = "%Y-%m-%d") ~ "Mejia",
      last_month > as.Date("2004-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2008-08-15", format = "%Y-%m-%d") ~ "Fernandez_first_dr",
      last_month > as.Date("2008-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2012-08-15", format = "%Y-%m-%d") ~ "Fernandez_second_dr",
      last_month > as.Date("2012-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2016-08-15", format = "%Y-%m-%d") ~ "Medina_first",
      last_month > as.Date("2016-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2020-08-15", format = "%Y-%m-%d") ~ "Medina_second",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Mejia", "Fernandez_first_dr", "Fernandez_second_dr",
        "Medina_first", "Medina_second"), ordered = TRUE))

DominicanRepublic <- DominicanRepublic %>%
  group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(DominicanRepublic $presidents)
##### 2.8 Ecuador ####
Ecuador <- data %>%
  filter(country == "Ecuador") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1979-08-10", format = "%Y-%m-%d") & last_month <= as.Date("1981-05-24", format = "%Y-%m-%d") ~ "Roldos",
      last_month > as.Date("1981-05-24", format = "%Y-%m-%d") & last_month <= as.Date("1984-08-10", format = "%Y-%m-%d") ~ "Hurtado",
      last_month > as.Date("1984-08-10", format = "%Y-%m-%d") & last_month <= as.Date("1988-08-10", format = "%Y-%m-%d") ~ "Febres",
      last_month > as.Date("1988-08-10", format = "%Y-%m-%d") & last_month <= as.Date("1992-08-10", format = "%Y-%m-%d") ~ "Borja",
      last_month > as.Date("1992-08-10", format = "%Y-%m-%d") & last_month <= as.Date("1996-08-10", format = "%Y-%m-%d") ~ "Duran",
      last_month > as.Date("1996-08-10", format = "%Y-%m-%d") & last_month <= as.Date("1997-02-06", format = "%Y-%m-%d") ~ "Bucaram",
      last_month > as.Date("1997-02-11", format = "%Y-%m-%d") & last_month <= as.Date("1998-08-10", format = "%Y-%m-%d") ~ "Alarcon",
      last_month > as.Date("1998-08-10", format = "%Y-%m-%d") & last_month <= as.Date("2000-01-21", format = "%Y-%m-%d") ~ "Mahuad",
      last_month > as.Date("2000-01-21", format = "%Y-%m-%d") & last_month <= as.Date("2003-01-15", format = "%Y-%m-%d") ~ "Noboa",
      last_month > as.Date("2003-01-15", format = "%Y-%m-%d") & last_month <= as.Date("2005-04-20", format = "%Y-%m-%d") ~ "Gutierrez_ecu",
      last_month > as.Date("2005-04-20", format = "%Y-%m-%d") & last_month <= as.Date("2007-01-15", format = "%Y-%m-%d") ~ "Palacio",
      last_month > as.Date("2007-01-15", format = "%Y-%m-%d") & last_month <= as.Date("2009-08-09", format = "%Y-%m-%d") ~ "Correa_first",
      last_month > as.Date("2009-08-09", format = "%Y-%m-%d") & last_month <= as.Date("2013-05-23", format = "%Y-%m-%d") ~ "Correa_second",
      last_month > as.Date("2013-05-23", format = "%Y-%m-%d") & last_month <= as.Date("2017-05-23", format = "%Y-%m-%d") ~ "Correa_third",
      last_month > as.Date("2017-05-23", format = "%Y-%m-%d") & last_month <= as.Date("2020-03-17", format = "%Y-%m-%d") ~ "Moreno",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c( "Roldos", "Hurtado", "Febres", "Borja", "Duran", "Bucaram", "Alarcon",
        "Mahuad", "Noboa", "Gutierrez_ecu", "Palacio",
        "Correa_first", "Correa_second", "Correa_third", "Moreno"), ordered = TRUE))

Ecuador <- Ecuador %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Ecuador $presidents)
##### 2.9 El Salvador ####
ElSalvador <- data %>%
  filter(country == "ElSalvador") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1984-06-01", format = "%Y-%m-%d") & last_month <= as.Date("1989-06-01", format = "%Y-%m-%d") ~ "Napoleon",
      last_month > as.Date("1989-06-01", format = "%Y-%m-%d") & last_month <= as.Date("1994-06-01", format = "%Y-%m-%d") ~ "Cristiani",
      last_month > as.Date("1994-06-01", format = "%Y-%m-%d") & last_month <= as.Date("1999-06-01", format = "%Y-%m-%d") ~ "Calderon_elsal",
      last_month > as.Date("1999-06-01", format = "%Y-%m-%d") & last_month <= as.Date("2004-06-01", format = "%Y-%m-%d") ~ "Flores",
      last_month > as.Date("2004-06-01", format = "%Y-%m-%d") & last_month <= as.Date("2009-06-01", format = "%Y-%m-%d") ~ "Saca",
      last_month > as.Date("2009-06-01", format = "%Y-%m-%d") & last_month <= as.Date("2014-06-01", format = "%Y-%m-%d") ~ "Funes",
      last_month > as.Date("2014-06-01", format = "%Y-%m-%d") & last_month <= as.Date("2019-06-01", format = "%Y-%m-%d") ~ "Sanchez_elsal",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Napoleon", "Cristiani", "Calderon_elsal", "Flores", "Saca", "Funes", "Sanchez_elsal"),ordered = TRUE))

ElSalvador <- ElSalvador %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(ElSalvador $presidents)
##### 2.10 Guatemala ####
Guatemala <- data %>%
  filter(country == "Guatemala") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1986-01-14", format = "%Y-%m-%d") & last_month <= as.Date("1991-01-14", format = "%Y-%m-%d") ~ "Cerezo",
      last_month > as.Date("1991-01-14", format = "%Y-%m-%d") & last_month <= as.Date("1993-06-01", format = "%Y-%m-%d") ~ "Serrano",
      last_month > as.Date("1993-06-06", format = "%Y-%m-%d") & last_month <= as.Date("1996-01-14", format = "%Y-%m-%d") ~ "DeLeon",
      last_month > as.Date("1996-01-14", format = "%Y-%m-%d") & last_month <= as.Date("2000-01-14", format = "%Y-%m-%d") ~ "Azru",
      last_month > as.Date("2000-01-14", format = "%Y-%m-%d") & last_month <= as.Date("2004-01-14", format = "%Y-%m-%d") ~ "Portillo",
      last_month > as.Date("2004-01-14", format = "%Y-%m-%d") & last_month <= as.Date("2008-01-14", format = "%Y-%m-%d") ~ "Berger",
      last_month > as.Date("2008-01-14", format = "%Y-%m-%d") & last_month <= as.Date("2012-01-14", format = "%Y-%m-%d") ~ "Colom",
      last_month > as.Date("2012-01-14", format = "%Y-%m-%d") & last_month <= as.Date("2015-09-03", format = "%Y-%m-%d") ~ "Perez_guate",
      last_month > as.Date("2015-09-03", format = "%Y-%m-%d") & last_month <= as.Date("2016-01-14", format = "%Y-%m-%d") ~ "Baltasar",
      last_month > as.Date("2016-01-14", format = "%Y-%m-%d") & last_month <= as.Date("2020-01-14", format = "%Y-%m-%d") ~ "Morales_guate",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Cerezo", "Serrano", "DeLeon", "Azru", "Portillo", "Berger", "Colom", "Perez_guate", "Baltasar", "Morales_guate"),
      ordered = TRUE))

Guatemala <- Guatemala %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Guatemala $presidents)

##### 2.11 Honduras ####
Honduras <- data %>%
  filter(country == "Honduras") %>%
  mutate(presidents = case_when(
    last_month > as.Date("1986-01-27", format = "%Y-%m-%d") & last_month <= as.Date("1990-01-27", format = "%Y-%m-%d") ~ "Azcona",
    last_month > as.Date("1990-01-27", format = "%Y-%m-%d") & last_month <= as.Date("1994-01-27", format = "%Y-%m-%d") ~ "Calejas",
    last_month > as.Date("1994-01-27", format = "%Y-%m-%d") & last_month <= as.Date("1998-01-27", format = "%Y-%m-%d") ~ "Roberto_reina",
    last_month > as.Date("1998-01-27", format = "%Y-%m-%d") & last_month <= as.Date("2002-01-27", format = "%Y-%m-%d") ~ "Roberto_flores",
    last_month > as.Date("2002-01-27", format = "%Y-%m-%d") & last_month <= as.Date("2006-01-27", format = "%Y-%m-%d") ~ "Maduro_hond",
    last_month > as.Date("2006-01-27", format = "%Y-%m-%d") & last_month <= as.Date("2009-06-28", format = "%Y-%m-%d") ~ "Zelaya",
    last_month > as.Date("2009-06-28", format = "%Y-%m-%d") & last_month <= as.Date("2010-01-27", format = "%Y-%m-%d") ~ "Micheletti",
    last_month > as.Date("2010-01-27", format = "%Y-%m-%d") & last_month <= as.Date("2014-01-27", format = "%Y-%m-%d") ~ "Lobo",
    last_month > as.Date("2014-01-27", format = "%Y-%m-%d") & last_month <= as.Date("2018-01-27", format = "%Y-%m-%d") ~ "Orlando_first",
    last_month > as.Date("2018-01-27", format = "%Y-%m-%d") & last_month <= as.Date("2022-01-27", format = "%Y-%m-%d") ~ "Orlando_second",
    TRUE ~ NA_character_),
  presidents = factor(presidents,
    levels = c("Azcona", "Calejas", "Roberto_reina", "Roberto_flores", "Maduro_hond", 
               "Zelaya", "Micheletti", "Lobo", "Orlando_first", "Orlando_second"),
    ordered = TRUE ))

Honduras <- Honduras %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Honduras $presidents)
 
##### 2.12 Mexico ####
Mexico <- data %>%
  filter(country == "Mexico") %>%
  mutate(presidents = case_when(
    last_month > as.Date("1988-11-30", format = "%Y-%m-%d") & last_month <= as.Date("1994-11-01", format = "%Y-%m-%d") ~ "Salinas",
    last_month > as.Date("1994-11-30", format = "%Y-%m-%d") & last_month <= as.Date("2000-11-01", format = "%Y-%m-%d") ~ "Zedillo",
    last_month > as.Date("2000-11-30", format = "%Y-%m-%d") & last_month <= as.Date("2006-11-01", format = "%Y-%m-%d") ~ "Fox",
    last_month > as.Date("2006-11-01", format = "%Y-%m-%d") & last_month <= as.Date("2012-11-01", format = "%Y-%m-%d") ~ "Calderon_mex",
    last_month > as.Date("2012-11-01", format = "%Y-%m-%d") & last_month <= as.Date("2018-12-31", format = "%Y-%m-%d") ~ "Pena",
    last_month > as.Date("2018-12-31", format = "%Y-%m-%d") & last_month <= as.Date("2019-09-01", format = "%Y-%m-%d") ~ "Lopez",
    TRUE ~ NA_character_),
  presidents = factor(
    presidents,
    levels = c("Salinas", "Zedillo", "Fox", "Calderon_mex", "Pena", "Lopez"),
    ordered = TRUE))

Mexico <- Mexico %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Mexico $presidents)
##### 2.13 Nicaragua ####
Nicaragua <- data %>%
  filter(country == "Nicaragua") %>%
  mutate(presidents = case_when(
    last_month > as.Date("1985-01-10", format = "%Y-%m-%d") & last_month <= as.Date("1990-04-25", format = "%Y-%m-%d") ~ "Ortega_nica",
    last_month > as.Date("1990-04-25", format = "%Y-%m-%d") & last_month <= as.Date("1997-01-10", format = "%Y-%m-%d") ~ "Chamorro",
    last_month > as.Date("1997-01-10", format = "%Y-%m-%d") & last_month <= as.Date("2002-01-10", format = "%Y-%m-%d") ~ "Aleman",
    last_month > as.Date("2002-01-10", format = "%Y-%m-%d") & last_month <= as.Date("2007-01-10", format = "%Y-%m-%d") ~ "Bolanos",
    last_month > as.Date("2007-01-10", format = "%Y-%m-%d") & last_month <= as.Date("2012-01-10", format = "%Y-%m-%d") ~ "Ortega_first",
    last_month > as.Date("2012-01-10", format = "%Y-%m-%d") & last_month <= as.Date("2017-01-10", format = "%Y-%m-%d") ~ "Ortega_second",
    last_month > as.Date("2017-01-10", format = "%Y-%m-%d") & last_month <= as.Date("2022-01-10", format = "%Y-%m-%d") ~ "Ortega_third",
    TRUE ~ NA_character_ ),
  presidents = factor(presidents,
    levels = c("Ortega_nica", "Chamorro", "Aleman", "Bolanos", "Ortega_first", "Ortega_second", "Ortega_third"),
    ordered = TRUE))

Nicaragua <- Nicaragua %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Nicaragua $presidents)
##### 2.14 Panama ####
Panama <- data %>%
  filter(country == "Panama") %>%
  mutate(presidents = case_when(
    last_month > as.Date("1989-12-20", format = "%Y-%m-%d") & last_month <= as.Date("1994-09-01", format = "%Y-%m-%d") ~ "Endara",
    last_month > as.Date("1994-09-01", format = "%Y-%m-%d") & last_month <= as.Date("1999-09-01", format = "%Y-%m-%d") ~ "Perez_panam",
    last_month > as.Date("1999-09-01", format = "%Y-%m-%d") & last_month <= as.Date("2004-09-01", format = "%Y-%m-%d") ~ "Moscoso",
    last_month > as.Date("2004-09-01", format = "%Y-%m-%d") & last_month <= as.Date("2009-07-01", format = "%Y-%m-%d") ~ "Torrijos",
    last_month > as.Date("2009-07-01", format = "%Y-%m-%d") & last_month <= as.Date("2014-07-01", format = "%Y-%m-%d") ~ "Martinelli",
    last_month > as.Date("2014-07-01", format = "%Y-%m-%d") & last_month <= as.Date("2019-07-01", format = "%Y-%m-%d") ~ "Varela",
    last_month > as.Date("2019-07-01", format = "%Y-%m-%d") & last_month <= as.Date("2024-07-01", format = "%Y-%m-%d") ~ "Cotizo",
    TRUE ~ NA_character_),
  presidents = factor(presidents,
    levels = c("Endara", "Perez_panam", "Moscoso", "Torrijos", "Martinelli", "Varela", "Cotizo"),
    ordered = TRUE))

Panama <- Panama %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Panama $presidents)

##### 2.15 Paraguay ####
Paraguay <- data %>%
  filter(country == "Paraguay") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1993-08-15", format = "%Y-%m-%d") & last_month <= as.Date("1998-08-15", format = "%Y-%m-%d") ~ "Wasmosy",
      last_month > as.Date("1998-08-15", format = "%Y-%m-%d") & last_month <= as.Date("1999-03-28", format = "%Y-%m-%d") ~ "Cubas",
      last_month > as.Date("1999-03-28", format = "%Y-%m-%d") & last_month <= as.Date("2003-08-15", format = "%Y-%m-%d") ~ "Gonzalez",
      last_month > as.Date("2003-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2008-08-15", format = "%Y-%m-%d") ~ "Duarte",
      last_month > as.Date("2008-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2012-06-22", format = "%Y-%m-%d") ~ "Lugo",
      last_month > as.Date("2012-06-22", format = "%Y-%m-%d") & last_month <= as.Date("2013-08-15", format = "%Y-%m-%d") ~ "Franco_parag",
      last_month > as.Date("2013-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2018-08-15", format = "%Y-%m-%d") ~ "Cartes",
      last_month > as.Date("2018-08-15", format = "%Y-%m-%d") & last_month <= as.Date("2023-08-15", format = "%Y-%m-%d") ~ "Abdo",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Wasmosy", "Cubas", "Gonzalez", "Duarte", "Lugo", "Franco_parag", "Cartes", "Abdo"),
      ordered = TRUE))

Paraguay <- Paraguay %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Paraguay $presidents)
##### 2.16 Peru ####
Peru <- data %>%
  filter(country == "Peru") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1980-07-28", format = "%Y-%m-%d") & last_month <= as.Date("1985-07-28", format = "%Y-%m-%d") ~ "Belaunde",
      last_month > as.Date("1985-07-28", format = "%Y-%m-%d") & last_month <= as.Date("1990-07-28", format = "%Y-%m-%d") ~ "Garcia_first_peru",
      last_month > as.Date("1990-07-28", format = "%Y-%m-%d") & last_month <= as.Date("1995-07-28", format = "%Y-%m-%d") ~ "Fujimori_first",
      last_month > as.Date("1995-07-28", format = "%Y-%m-%d") & last_month <= as.Date("2000-07-28", format = "%Y-%m-%d") ~ "Fujimori_second",
      last_month > as.Date("2000-07-28", format = "%Y-%m-%d") & last_month <= as.Date("2000-11-21", format = "%Y-%m-%d") ~ "Fujimori_third",
      last_month > as.Date("2000-11-21", format = "%Y-%m-%d") & last_month <= as.Date("2001-07-28", format = "%Y-%m-%d") ~ "Paniagua",
      last_month > as.Date("2001-07-28", format = "%Y-%m-%d") & last_month <= as.Date("2006-07-28", format = "%Y-%m-%d") ~ "Toledo",
      last_month > as.Date("2006-07-28", format = "%Y-%m-%d") & last_month <= as.Date("2011-07-28", format = "%Y-%m-%d") ~ "Garcia_second_peru",
      last_month > as.Date("2011-07-28", format = "%Y-%m-%d") & last_month <= as.Date("2016-07-28", format = "%Y-%m-%d") ~ "Humala",
      last_month > as.Date("2016-07-28", format = "%Y-%m-%d") & last_month <= as.Date("2018-03-21", format = "%Y-%m-%d") ~ "Kuczynski",
      last_month > as.Date("2018-03-21", format = "%Y-%m-%d") & last_month <= as.Date("2023-03-21", format = "%Y-%m-%d") ~ "Vizcarra",
      TRUE ~ NA_character_),
    presidents = factor( presidents,
      levels = c("Belaunde", "Garcia_first_peru", "Fujimori_first", "Fujimori_second", 
                 "Fujimori_third", "Paniagua", "Toledo", "Garcia_second_peru", 
                 "Humala", "Kuczynski", "Vizcarra"), ordered = TRUE))

Peru <- Peru %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Peru $presidents)

##### 2.17 United States ####
UnitedStates <- data %>%
  filter(country == "United States") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1941-01-20") & last_month <= as.Date("1945-01-20") ~ "Roosevelt_second",
      last_month > as.Date("1945-01-20") & last_month <= as.Date("1945-04-12") ~ "Roosevelt_third",
      last_month > as.Date("1945-04-12") & last_month <= as.Date("1949-01-20") ~ "Truman_first",
      last_month > as.Date("1949-01-20") & last_month <= as.Date("1953-01-20") ~ "Truman_second",
      last_month > as.Date("1953-01-20") & last_month <= as.Date("1961-01-20") ~ "Eisenhower",
      last_month > as.Date("1961-01-20") & last_month <= as.Date("1963-11-22") ~ "Kennedy",
      last_month > as.Date("1963-11-22") & last_month <= as.Date("1965-01-20") ~ "Johnson_first",
      last_month > as.Date("1965-01-20") & last_month <= as.Date("1969-01-20") ~ "Johnson_second",
      last_month > as.Date("1969-01-20") & last_month <= as.Date("1973-12-06") ~ "Nixon_first",
      last_month > as.Date("1973-12-06") & last_month <= as.Date("1974-08-09") ~ "Nixon_second",
      last_month > as.Date("1974-08-09") & last_month <= as.Date("1974-12-19") ~ "Ford_first",
      last_month > as.Date("1974-12-19") & last_month <= as.Date("1977-01-20") ~ "Ford_second",
      last_month > as.Date("1977-01-20") & last_month <= as.Date("1981-01-20") ~ "Carter",
      last_month > as.Date("1981-01-20") & last_month <= as.Date("1985-01-20") ~ "Reagan_first",
      last_month > as.Date("1985-01-20") & last_month <= as.Date("1989-01-20") ~ "Reagan_second",
      last_month > as.Date("1989-01-20") & last_month <= as.Date("1993-01-20") ~ "Bush",
      last_month > as.Date("1993-01-20") & last_month <= as.Date("1997-01-20") ~ "Clinton_first",
      last_month > as.Date("1997-01-20") & last_month <= as.Date("2001-01-20") ~ "Clinton_second",
      last_month > as.Date("2001-01-20") & last_month <= as.Date("2005-01-20") ~ "WBush_first",
      last_month > as.Date("2005-01-20") & last_month <= as.Date("2009-01-20") ~ "WBush_second",
      last_month > as.Date("2009-01-20") & last_month <= as.Date("2013-01-20") ~ "Obama_first",
      last_month > as.Date("2013-01-20") & last_month <= as.Date("2017-01-20") ~ "Obama_second",
      last_month > as.Date("2017-01-20") & last_month <= as.Date("2021-01-20") ~ "Trump",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c( "Roosevelt_second", "Roosevelt_third", "Truman_first", "Truman_second", 
        "Eisenhower", "Kennedy", "Johnson_first", "Johnson_second", "Nixon_first", 
        "Nixon_second", "Ford_first", "Ford_second", "Carter", "Reagan_first", 
        "Reagan_second", "Bush", "Clinton_first", "Clinton_second", 
        "WBush_first", "WBush_second", "Obama_first", "Obama_second", "Trump"),
      ordered = TRUE))

UnitedStates <- UnitedStates %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(UnitedStates $presidents)
##### 2.18 Uruguay ####
Uruguay <- data %>%
  filter(country == "Uruguay") %>%
  mutate(presidents = case_when(
      last_month > as.Date("1985-03-01", format = "%Y-%m-%d") & last_month <= as.Date("1990-03-01", format = "%Y-%m-%d") ~ "Sanguinetti_first",
      last_month > as.Date("1990-03-01", format = "%Y-%m-%d") & last_month <= as.Date("1995-03-01", format = "%Y-%m-%d") ~ "La Calle",
      last_month > as.Date("1995-03-01", format = "%Y-%m-%d") & last_month <= as.Date("2000-03-01", format = "%Y-%m-%d") ~ "Sanguinetti_second",
      last_month > as.Date("2000-03-01", format = "%Y-%m-%d") & last_month <= as.Date("2005-03-01", format = "%Y-%m-%d") ~ "Batle",
      last_month > as.Date("2005-03-01", format = "%Y-%m-%d") & last_month <= as.Date("2010-03-01", format = "%Y-%m-%d") ~ "Vazquez_first",
      last_month > as.Date("2010-03-01", format = "%Y-%m-%d") & last_month <= as.Date("2015-03-01", format = "%Y-%m-%d") ~ "Mujica",
      last_month > as.Date("2015-03-01", format = "%Y-%m-%d") & last_month <= as.Date("2020-03-01", format = "%Y-%m-%d") ~ "Vazquez_second",
      TRUE ~ NA_character_),
    presidents = factor(presidents,
      levels = c("Sanguinetti_first", "La Calle", "Sanguinetti_second", "Batle", "Vazquez_first", "Mujica", "Vazquez_second"),
      ordered = TRUE ))

Uruguay <- Uruguay %>% group_by(presidents) %>%
  arrange(last_month, .by_group = TRUE) %>%
  mutate(duration = row_number()) %>%
  ungroup()

is.ordered(Uruguay$presidents)
##### 2.19 Rbinding  the diferent datasets ####
unique(data$country)

vars=list(Argentina, Bolivia, Brazil, Chile, Colombia, CostaRica, DominicanRepublic, Ecuador, ElSalvador, Guatemala,
          Honduras, Mexico, Nicaragua, Panama, Paraguay, Peru, UnitedStates, Uruguay)

data=do.call("rbind", vars)

rm(Argentina, Bolivia, Brazil, Chile, Colombia, CostaRica, DominicanRepublic, Ecuador, ElSalvador, Guatemala,
   Honduras, Mexico, Nicaragua, Panama, Paraguay, Peru, UnitedStates, Uruguay, vars, target)

str(data)
levels(data$presidents)

data$duration.sqr=data$duration*data$duration
data$honey=ifelse(data$duration<=3, 1, 0)

write_csv(data, "11_GeneratedData/01_Approval/Approval_LAC_president_dummies_V3.csv")

##########################

####################################################
####################################################
##### 3. Adding the price data from OLADE #####
####################################################

##### 3.1 Argentina ######
argentina      <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_argentina.xlsx",sheet=2,skip=293))
natural_gas    <- argentina[c(1:378),c(1:2)]

colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"

other           <- argentina[c(384:764),]
colnames(other) <- other[1,]
other           <- other[-1,]
colnames(other)[1] <- "date"
other           <- other %>% 
                    dplyr::select(date, diesel_usd="Diésel Oil", 
                                  hi_oct_usd="Gasolina Premiun", 
                                  lo_oct_usd="Gasolina Regular" )

argentina        = full_join(other, natural_gas, by="date")

rm(natural_gas, other)

argentina$date      <- seq(as.Date("1988/01/01"), as.Date("2019/08/01"),"months")
cols.num            <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
argentina[cols.num] <- sapply(argentina[cols.num],as.numeric)

sapply(argentina, class)

#converting to local currency
exchange              <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=1998))
exchange              <- exchange[c(1:298),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"

source("05_Scripts/transforming_date.R")

exchange$date         <- transforming_date(exchange$date)
exchange$year         <- substr(exchange$date, 6,9)
exchange$year[c(1:8)] <- exchange$date[c(1:8)]
exchange$month        <- substr(exchange$date, 1,2)
exchange$day          <- "01"
exchange$date         <- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange               = exchange %>% drop_na(date)

argentina             <- left_join(argentina, exchange, by="date")

exchange_fred         <- read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/Exchange rates/argentina.csv")
colnames(exchange_fred)[1] <- "date"
colnames(exchange_fred)[2] <- "exchange_fred"
exchange_fred$date         <- as.Date(exchange_fred$date)
argentina                  <- left_join(argentina, exchange_fred, by="date")

#we use the exchange rate with less NAs according to the next funciton:
ifelse(sum(is.na(argentina$exchange_fred))>=sum(is.na(argentina$exchange_olade)),"Olade", "Fred")
# A smarter one
datframe_fred               = argentina %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_fred)
datframe_olade              = argentina %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd,exchange_olade)
ifelse(sum(is.na(datframe_fred))>=sum(is.na(datframe_olade)),"Olade", "Fred")
#we use the Fred exchange rate
argentina$lo_oct_local      = argentina$lo_oct_usd*argentina$exchange_fred
argentina$hi_oct_local      = argentina$hi_oct_usd*argentina$exchange_fred
argentina$diesel_local      = argentina$diesel_usd*argentina$exchange_fred
argentina$nat_gas_local     = argentina$nat_gas_usd*argentina$exchange_fred
argentina$exchange_olade    =as.numeric(argentina$exchange_olade)
argentina$country           ="Argentina"
argentina_monthly           =argentina
argentina_monthly           = argentina_monthly %>% 
                              select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, 
                                     nat_gas_usd,lo_oct_local, hi_oct_local, diesel_local,
                                     nat_gas_local, exchange_olade, exchange_fred)
# #converting monnthly to quarterly
argentina                   = argentina %>% 
                              group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
                              summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
                                        hi_oct_usd=mean(hi_oct_usd, na.rm=T),
                                        diesel_usd=mean(diesel_usd, na.rm=T),
                                        nat_gas_usd=mean(nat_gas_local, na.rm=T),
                                        lo_oct_local=mean(lo_oct_local, na.rm=T),
                                        hi_oct_local=mean(hi_oct_local, na.rm=T),
                                        diesel_local=mean(diesel_local, na.rm=T),
                                        nat_gas_local=mean(nat_gas_local, na.rm=T), 
                                        exchange_olade=mean(exchange_olade),
                                        exchange_fred=mean(exchange_fred))
argentina$quarterly         = as.yearqtr(argentina$date,format="%Yq%q")
argentina$last_month        = as.Date(argentina$quarterly, frac=1)    
argentina$country           <- "Argentina"
summary(argentina)
argentina[argentina== 0]     <- NA
summary(argentina)

argentina$lo_oct_local[is.nan(argentina$lo_oct_local)]=NA
argentina$hi_oct_local[is.nan(argentina$hi_oct_local)]=NA
argentina$diesel_local[is.nan(argentina$diesel_local)]=NA
argentina$nat_gas_local[is.nan(argentina$nat_gas_local)]=NA

argentina                     =argentina %>% 
                              mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% 
                              rowSums(na.rm = F))
index_base=argentina$sum[1]
argentina$price_ind            =(argentina$sum/index_base)*100
colnames(argentina)
argentina[argentina == 0]     <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
argentina$diff_lo_oct         <- c(NA, diff(argentina$lo_oct_local, lag=1, differences=1))
argentina$pace_lo_oct          <- ifelse(argentina$diff_lo_oct>=quantile(argentina$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# argentina$pace_lo_oct =argentina$pace_dummy*argentina$price_diff_lo
argentina$diff_price_ind       <- c(NA, diff(argentina$price_ind, lag=1, differences=1))
argentina$pace_price_ind       <- ifelse(argentina$diff_price_ind>=quantile(argentina$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# argentina$pace_price_ind =argentina$pace_price_ind*argentina$diff_price_ind

argentina = argentina %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                 lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                                 pace_lo_oct, pace_price_ind)

##### 3.2 Bolivia  ######
#Important: prices from OLADE for Bol are in (US$/bbl)
bolivia             <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_bolivia.xlsx",sheet=2,skip=296))
natural_gas         <- bolivia[c(1:229),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
other <- bolivia[c(234:539),] #data set of the other fuels
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
bolivia = full_join(other, natural_gas, by="date")
rm(natural_gas, other)
bolivia= bolivia[-305,]
bolivia2 = data.frame(date=seq(as.Date("2007/01/01"), as.Date("2009/12/01"), "months"),
                      diesel_usd=NA, hi_oct_usd=NA, lo_oct_usd=NA, nat_gas_usd=NA)
bolivia2$diesel_usd[6] = 74.30
bolivia2$hi_oct_usd[6] = 95.67
bolivia2$lo_oct_usd[6] = 74.70
bolivia2$diesel_usd[18] = 83.07
bolivia2$hi_oct_usd[18] = 106.96
bolivia2$lo_oct_usd[18] = 83.51
bolivia2$diesel_usd[23] = 84.24
bolivia2$hi_oct_usd[23] = 108.47
bolivia2$lo_oct_usd[23] = 84.72
bolivia2$diesel_usd[30] = 83.65
bolivia2$hi_oct_usd[30] = 107.72
bolivia2$lo_oct_usd[30] = 84.10
bolivia1=bolivia[c(1:228),]
bolivia3=bolivia[c(233:304),]
bolivia4 = data.frame(date=seq(as.Date("2016/01/01"), as.Date("2017/03/01"), "months"),
                      diesel_usd="84.9759759465817", hi_oct_usd="109.417990533367", lo_oct_usd="85.432836032316", nat_gas_usd=NA)
bolivia=rbind(bolivia1, bolivia2, bolivia3, bolivia4)
bolivia$date <- seq(as.Date("1988/01/01"), as.Date("2017/03/01"),"months")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
bolivia[cols.num] <- sapply(bolivia[cols.num],as.numeric)
sapply(bolivia, class)
#converting to local currency
#we have usd/bbl, we need first to convert to BOL/bbl
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=347))
exchange <- exchange[c(1:333),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
bolivia <- left_join(bolivia, exchange, by="date")
str(bolivia)
bolivia$exchange_olade <- as.numeric(bolivia$exchange_olade)
#No data from the FRED, we HAVE to use the OLADE exchange rate
#exchange rate (more/less): 1 dollar, 6 bolivian 
bolivia$lo_oct_local = bolivia$lo_oct_usd*bolivia$exchange_olade
bolivia$hi_oct_local = bolivia$hi_oct_usd*bolivia$exchange_olade
bolivia$diesel_local = bolivia$diesel_usd*bolivia$exchange_olade
bolivia$nat_gas_local = bolivia$nat_gas_usd*bolivia$exchange_olade
#Now we have the prices in Bol/BBL
bolivia$exchange_fred=NA
bolivia$country="Bolivia"
bolivia_monthly=bolivia
bolivia_monthly=bolivia_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                           lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting monthly to quarterly
bolivia = bolivia %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
bolivia$quarterly = as.yearqtr(bolivia$date,format="%Yq%q")
bolivia$last_month = as.Date(bolivia$quarterly, frac=1)    
bolivia$country <- "Bolivia"
rm(bolivia1,bolivia2, bolivia3,bolivia4)
summary(bolivia)
bolivia[bolivia== 0] <- NA

#obtaining the prices collected on my own and replacing NAs with these prices 
price.bol.own=as.data.frame(read_csv("03_Data_Country by country/Bolivia/master_data_bolivia.csv"))
colnames(price.bol.own)
#THE PRICE HERE IS IN Bs/L, TO CONVERT WE HAVE TO 
#first, converting prices in Bol per liter to USD per liter
exchange <- read.csv("03_Data_Country by country/Bolivia/04_exchange.rate/exchange.rate.csv", sep=";")
exchange <- exchange[c(48:212),]
price.bol.own$price_lo_usd_l <- price.bol.own$gasoline_price_low/exchange$exchange_rate
price.bol.own$price_hi_usd_l <- price.bol.own$gasoline_price_high/exchange$exchange_rate
#then, convert USD/l to USD/BBL
price.bol.own.2=select(price.bol.own, date, price_lo_usd_l, price_hi_usd_l) %>% 
  group_by(date=format(as.yearqtr(date), "%Yq%q")) %>%
  summarize(lo_oct_usd_own=(mean(price_lo_usd_l, na.rm=T))*158.987,
            hi_oct_usd_own=(mean(price_hi_usd_l, na.rm=T))*158.987)
bolivia=full_join(bolivia, price.bol.own.2)

### VERY IMPORTANT STEP
bolivia[92,c("lo_oct_usd","hi_oct_usd")] <- NA #deleting the observed prices, these will be replaced with the 
#planned prices during the planned reform of Q4 2010
bolivia=bolivia %>% mutate(lo_oct_usd=coalesce(lo_oct_usd, lo_oct_usd_own))
bolivia=bolivia %>% mutate(hi_oct_usd=coalesce(hi_oct_usd, hi_oct_usd_own))
#Imputing exchange rate for 2007, 2008, 2009. Data from the FRED
bolivia$exchange_olade[77:80]<-7.8512451610 #Adding yearly exchange rate from FRED
bolivia$exchange_olade[81:84]<-7.2383206990 #Adding yearly exchange rate from FREd
bolivia$exchange_olade[85:88]<-7.0200000000 #Adding yearly exchange rate from FRED
bolivia$exchange_olade[115:123]<-6.96 #Banco Central de Bolivia

bolivia$lo_oct_local = bolivia$lo_oct_usd*bolivia$exchange_olade
bolivia$hi_oct_local = bolivia$hi_oct_usd*bolivia$exchange_olade
bolivia$diesel_local = bolivia$diesel_usd*bolivia$exchange_olade
bolivia$nat_gas_local = bolivia$nat_gas_usd*bolivia$exchange_olade

#Getting the price indicator, sum 
bolivia=bolivia %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=bolivia$sum[1]
bolivia$price_ind=(bolivia$sum/index_base)*100
colnames(bolivia)
bolivia[bolivia== 0] <- NA
bolivia$quarterly = as.yearqtr(bolivia$date,format="%Yq%q")
bolivia$last_month = as.Date(bolivia$quarterly, frac=1) 
bolivia$country=rep("Bolivia",123)


# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
bolivia$diff_lo_oct<- c(NA, diff(bolivia$lo_oct_local, lag=1, differences=1))
bolivia$pace_lo_oct <- ifelse(bolivia$diff_lo_oct>=quantile(bolivia$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# bolivia$pace_lo_oct =bolivia$pace_dummy*bolivia$price_diff_lo
bolivia$diff_price_ind<- c(NA, diff(bolivia$price_ind, lag=1, differences=1))
bolivia$pace_price_ind <- ifelse(bolivia$diff_price_ind>=quantile(bolivia$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# bolivia$pace_price_ind =bolivia$pace_price_ind*bolivia$diff_price_ind


bolivia = bolivia %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                             lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                             pace_lo_oct, pace_price_ind)
?plot
plot(bolivia$last_month, bolivia$lo_oct_local, type="l")

##### 3.3. Brazil  ######
brazil <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_brazil.xlsx",sheet=2,skip=294))
natural_gas <- brazil[c(1:190),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming_date(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)
other <- brazil[c(195:567),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
brazil = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2019/09/01"), "months"))

brazil=full_join(daty, brazil, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
brazil[cols.num] <- sapply(brazil[cols.num],as.numeric)
sapply(brazil, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=4403))
exchange <- exchange[c(1:273),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
brazil <- left_join(brazil, exchange, by="date")
str(brazil)
exchange_fred <- read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/Exchange rates/Brazil.csv")
colnames(exchange_fred)[1] <- "date"
colnames(exchange_fred)[2] <- "exchange_fred"
exchange_fred$date <- as.Date(exchange_fred$date)
brazil <- left_join(brazil, exchange_fred, by="date")
brazil$exchange_olade =as.numeric(brazil$exchange_olade)
#we use the exchange rate with less NAs according to the next function(s):
ifelse(sum(is.na(brazil$exchange_fred))>=sum(is.na(brazil$exchange_olade)),"Olade", "Fred")
# A smarter one
datframe_fred = brazil %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_fred)
datframe_olade = brazil %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_olade)
ifelse(sum(is.na(datframe_fred))>=sum(is.na(datframe_olade)),"Olade", "Fred")
#We decide to use FRED
brazil$lo_oct_local = brazil$lo_oct_usd*brazil$exchange_fred
brazil$hi_oct_local = brazil$hi_oct_usd*brazil$exchange_fred
brazil$diesel_local = brazil$diesel_usd*brazil$exchange_fred
brazil$nat_gas_local = brazil$nat_gas_usd*brazil$exchange_fred
brazil$country="Brazil"
brazil_monthly=brazil
brazil_monthly=brazil_monthly %>% select(date, country, lo_oct_usd, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                         lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
brazil = brazil %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=mean(exchange_fred))
brazil$quarterly = as.yearqtr(brazil$date,format="%Yq%q")
brazil$last_month = as.Date(brazil$quarterly, frac=1)    
brazil$country <- "Brazil"
brazil[brazil == 0] <- NA
rm(other, natural_gas, daty)

brazil$lo_oct_local[is.nan(brazil$lo_oct_local)]=NA
brazil$hi_oct_local[is.nan(brazil$hi_oct_local)]=NA
brazil$diesel_local[is.nan(brazil$diesel_local)]=NA
brazil$nat_gas_local[is.nan(brazil$nat_gas_local)]=NA
brazil=brazil %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=brazil$sum[37]
brazil$price_ind=(brazil$sum/index_base)*100
colnames(brazil)
brazil[brazil == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
brazil$diff_lo_oct<- c(NA, diff(brazil$lo_oct_local, lag=1, differences=1))
brazil$pace_lo_oct <- ifelse(brazil$diff_lo_oct>=quantile(brazil$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# brazil$pace_lo_oct =brazil$pace_dummy*brazil$price_diff_lo
brazil$diff_price_ind<- c(NA, diff(brazil$price_ind, lag=1, differences=1))
brazil$pace_price_ind <- ifelse(brazil$diff_price_ind>=quantile(brazil$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# brazil$pace_price_ind =brazil$pace_price_ind*brazil$diff_price_ind


brazil = brazil %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                           lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                           pace_lo_oct, pace_price_ind)
##### 3.4 Chile  ######
chile <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_chile.xlsx",sheet=2,skip=275))
natural_gas <- chile[c(1:345),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming_date(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)

other <- chile[c(350:695),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
chile = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2017/02/01"), "months"))

chile=full_join(daty, chile, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
chile[cols.num] <- sapply(chile[cols.num],as.numeric)
sapply(chile, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=2310))
exchange <- exchange[c(1:366),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
chile <- left_join(chile, exchange, by="date")
str(chile)
exchange_fred <- read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/Exchange rates/Chile.csv")
colnames(exchange_fred)[1] <- "date"
colnames(exchange_fred)[2] <- "exchange_fred"
exchange_fred$date <- as.Date(exchange_fred$date)
chile <- left_join(chile, exchange_fred, by="date")
chile$exchange_olade =as.numeric(chile$exchange_olade)
#we use the exchange rate with less NAs according to the next function(s):
ifelse(sum(is.na(chile$exchange_fred))>=sum(is.na(chile$exchange_olade)),"Olade", "Fred")
# A smarter one
datframe_fred = chile %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_fred)
datframe_olade = chile %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_olade)
ifelse(sum(is.na(datframe_fred))>=sum(is.na(datframe_olade)),"Olade", "Fred")
#We decide to use FRED
chile$lo_oct_local = chile$lo_oct_usd*chile$exchange_fred
chile$hi_oct_local = chile$hi_oct_usd*chile$exchange_fred
chile$diesel_local = chile$diesel_usd*chile$exchange_fred
chile$nat_gas_local = chile$nat_gas_usd*chile$exchange_fred
chile$country="Chile"
chile_monthly=chile
chile_monthly=chile_monthly  %>% select(date, country, lo_oct_usd, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                        lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
chile = chile %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=mean(exchange_fred))

chile$quarterly = as.yearqtr(chile$date,format="%Yq%q")
chile$last_month = as.Date(chile$quarterly, frac=1)    
chile$country <- "Chile"
summary(chile)
chile[chile == 0] <- NA
rm(other, natural_gas, daty)


chile$lo_oct_local[is.nan(chile$lo_oct_local)]=NA
chile$hi_oct_local[is.nan(chile$hi_oct_local)]=NA
chile$diesel_local[is.nan(chile$diesel_local)]=NA
chile$nat_gas_local[is.nan(chile$nat_gas_local)]=NA
chile=chile %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=chile$sum[1]
chile$price_ind=(chile$sum/index_base)*100
colnames(chile)
chile[chile == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
chile$diff_lo_oct<- c(NA, diff(chile$lo_oct_local, lag=1, differences=1))
chile$pace_lo_oct <- ifelse(chile$diff_lo_oct>=quantile(chile$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# chile$pace_lo_oct =chile$pace_dummy*chile$price_diff_lo
chile$diff_price_ind<- c(NA, diff(chile$price_ind, lag=1, differences=1))
chile$pace_price_ind <- ifelse(chile$diff_price_ind>=quantile(chile$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# chile$pace_price_ind =chile$pace_price_ind*chile$diff_price_ind


chile = chile %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                         lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                         pace_lo_oct, pace_price_ind)

##### 3.5 Colombia  ######
colombia <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_colombia.xlsx",sheet=2,skip=427))
natural_gas <- colombia[c(1:333),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming_date(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)

other <- colombia[c(339:679),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
colombia = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2017/04/01"), "months"))

colombia=full_join(daty, colombia, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
colombia[cols.num] <- sapply(colombia[cols.num],as.numeric)
sapply(colombia, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=2680))
exchange <- exchange[c(1:329),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
colombia <- left_join(colombia, exchange, by="date")
str(colombia)
colombia$exchange_olade =as.numeric(colombia$exchange_olade)
exchange_fred <- read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/Exchange rates/colombia.csv")
colnames(exchange_fred)[1] <- "date"
colnames(exchange_fred)[2] <- "exchange_fred"
exchange_fred$date <- as.Date(exchange_fred$date)
colombia <- left_join(colombia, exchange_fred, by="date")
#we use the exchange rate with less NAs according to the next function(s):
ifelse(sum(is.na(colombia$exchange_fred))>=sum(is.na(colombia$exchange_olade)),"Olade", "Fred")
# A smarter one
datframe_fred = colombia %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_fred)
datframe_olade = colombia %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_olade)
ifelse(sum(is.na(datframe_fred))>=sum(is.na(datframe_olade)),"Olade", "Fred")
#We decide to use FRED
colombia$lo_oct_local = colombia$lo_oct_usd*colombia$exchange_fred
colombia$hi_oct_local = colombia$hi_oct_usd*colombia$exchange_fred
colombia$diesel_local = colombia$diesel_usd*colombia$exchange_fred
colombia$nat_gas_local = colombia$nat_gas_usd*colombia$exchange_fred
colombia$country="Country"
colombia_monthly=colombia
colombia_monthly=colombia_monthly %>% select(date, country, lo_oct_usd, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                             lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
colombia = colombia %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=mean(exchange_fred))
colombia$quarterly = as.yearqtr(colombia$date,format="%Yq%q")
colombia$last_month = as.Date(colombia$quarterly, frac=1)    
colombia$country <- "Colombia"
summary(colombia)
colombia[colombia == 0] <- NA
summary(colombia)
rm(other, natural_gas, daty)
colombia$lo_oct_local[is.nan(colombia$lo_oct_local)]=NA
colombia$hi_oct_local[is.nan(colombia$hi_oct_local)]=NA
colombia$diesel_local[is.nan(colombia$diesel_local)]=NA
colombia$nat_gas_local[is.nan(colombia$nat_gas_local)]=NA
colombia=colombia %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=colombia$sum[1]
colombia$price_ind=(colombia$sum/index_base)*100
colnames(colombia)
colombia[colombia == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
colombia$diff_lo_oct<- c(NA, diff(colombia$lo_oct_local, lag=1, differences=1))
colombia$pace_lo_oct <- ifelse(colombia$diff_lo_oct>=quantile(colombia$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# colombia$pace_lo_oct =colombia$pace_dummy*colombia$price_diff_lo
colombia$diff_price_ind<- c(NA, diff(colombia$price_ind, lag=1, differences=1))
colombia$pace_price_ind <- ifelse(colombia$diff_price_ind>=quantile(colombia$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# colombia$pace_price_ind =colombia$pace_price_ind*colombia$diff_price_ind


colombia = colombia %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                               lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                               pace_lo_oct, pace_price_ind)
##### 3.6 Costa Rica ######
costa_rica <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_costa_rica.xlsx",sheet=2,skip=246))
natural_gas <- costa_rica[c(1:74),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming_date(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)

other <- costa_rica[c(80:455),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
costa_rica = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2019/04/01"), "months"))

costa_rica=full_join(daty, costa_rica, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
costa_rica[cols.num] <- sapply(costa_rica[cols.num],as.numeric)
sapply(costa_rica, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=685))
exchange <- exchange[c(1:413),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
costa_rica <- left_join(costa_rica, exchange, by="date")
str(costa_rica)
costa_rica$exchange_olade =as.numeric(costa_rica$exchange_olade)
exchange_fred <- read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/Exchange rates/costa_rica.csv")
colnames(exchange_fred)[1] <- "date"
colnames(exchange_fred)[2] <- "exchange_fred"
exchange_fred$date <- as.Date(exchange_fred$date)
costa_rica <- left_join(costa_rica, exchange_fred, by="date")
#we use the exchange rate with less NAs according to the next function(s):
ifelse(sum(is.na(costa_rica$exchange_fred))>=sum(is.na(costa_rica$exchange_olade)),"Olade", "Fred")
# A smarter one
datframe_fred = costa_rica %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_fred)
datframe_olade = costa_rica %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_olade)
ifelse(sum(is.na(datframe_fred))>=sum(is.na(datframe_olade)),"Olade", "Fred")
#We decide to use FRED
costa_rica$lo_oct_local = costa_rica$lo_oct_usd*costa_rica$exchange_fred
costa_rica$hi_oct_local = costa_rica$hi_oct_usd*costa_rica$exchange_fred
costa_rica$diesel_local = costa_rica$diesel_usd*costa_rica$exchange_fred
costa_rica$nat_gas_local = costa_rica$nat_gas_usd*costa_rica$exchange_fred
costa_rica$country="Costa Rica"
costa_rica_monthly=costa_rica
costa_rica_monthly=costa_rica_monthly %>% select(date, country, lo_oct_usd, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                                 lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
costa_rica = costa_rica %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=mean(exchange_fred))
costa_rica$quarterly = as.yearqtr(costa_rica$date,format="%Yq%q")
costa_rica$last_month = as.Date(costa_rica$quarterly, frac=1)    
costa_rica$country <- "Costa Rica"
summary(costa_rica)
costa_rica[costa_rica == 0] <- NA
summary(costa_rica)
rm(other, natural_gas, daty)


costa_rica$lo_oct_local[is.nan(costa_rica$lo_oct_local)]=NA
costa_rica$hi_oct_local[is.nan(costa_rica$hi_oct_local)]=NA
costa_rica$diesel_local[is.nan(costa_rica$diesel_local)]=NA
costa_rica$nat_gas_local[is.nan(costa_rica$nat_gas_local)]=NA
costa_rica=costa_rica %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=costa_rica$sum[8]
costa_rica$price_ind=(costa_rica$sum/index_base)*100
colnames(costa_rica)
costa_rica[costa_rica == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
costa_rica$diff_lo_oct<- c(NA, diff(costa_rica$lo_oct_local, lag=1, differences=1))
costa_rica$pace_lo_oct <- ifelse(costa_rica$diff_lo_oct>=quantile(costa_rica$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# costa_rica$pace_lo_oct =costa_rica$pace_dummy*costa_rica$price_diff_lo
costa_rica$diff_price_ind<- c(NA, diff(costa_rica$price_ind, lag=1, differences=1))
costa_rica$pace_price_ind <- ifelse(costa_rica$diff_price_ind>=quantile(costa_rica$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# costa_rica$pace_price_ind =costa_rica$pace_price_ind*costa_rica$diff_price_ind


costa_rica = costa_rica %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                   lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                                   pace_lo_oct, pace_price_ind)

##### 3.7 Dominican Republic ######
dominican_republic <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_dominican_republic.xlsx",sheet=2,skip=255))
natural_gas <- dominican_republic[c(1:26),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming_date(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)

other <- dominican_republic[c(31:411),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
dominican_republic = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2019/08/01"), "months"))

dominican_republic=full_join(daty, dominican_republic, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
dominican_republic[cols.num] <- sapply(dominican_republic[cols.num],as.numeric)
sapply(dominican_republic, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=3013))
exchange <- exchange[c(1:415),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
dominican_republic <- left_join(dominican_republic, exchange, by="date")
str(dominican_republic)
dominican_republic$exchange_olade =as.numeric(dominican_republic$exchange_olade)
#NO FRED DATA, SO WE USE OLADE FOR THE EXCHANGE RATE
#We decide to use olade
dominican_republic$lo_oct_local = dominican_republic$lo_oct_usd*dominican_republic$exchange_olade
dominican_republic$hi_oct_local = dominican_republic$hi_oct_usd*dominican_republic$exchange_olade
dominican_republic$diesel_local = dominican_republic$diesel_usd*dominican_republic$exchange_olade
dominican_republic$nat_gas_local = dominican_republic$nat_gas_usd*dominican_republic$exchange_olade
dominican_republic$exchange_fred=NA
dominican_republic$country="Dominican Republic"
dominican_republic_monthly=dominican_republic
dominican_republic_monthly=dominican_republic_monthly %>% select(date, country, lo_oct_usd, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                                                 lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
dominican_republic = dominican_republic %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
dominican_republic$quarterly = as.yearqtr(dominican_republic$date,format="%Yq%q")
dominican_republic$last_month = as.Date(dominican_republic$quarterly, frac=1)    
dominican_republic$country <- "Dominican Republic"
summary(dominican_republic)
dominican_republic[dominican_republic == 0] <- NA
summary(dominican_republic)
rm(other, natural_gas, daty)

dominican_republic$lo_oct_local[is.nan(dominican_republic$lo_oct_local)]=NA
dominican_republic$hi_oct_local[is.nan(dominican_republic$hi_oct_local)]=NA
dominican_republic$diesel_local[is.nan(dominican_republic$diesel_local)]=NA
dominican_republic$nat_gas_local[is.nan(dominican_republic$nat_gas_local)]=NA
dominican_republic=dominican_republic %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=dominican_republic$sum[1]
dominican_republic$price_ind=(dominican_republic$sum/index_base)*100
colnames(dominican_republic)
dominican_republic[dominican_republic == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
dominican_republic$diff_lo_oct<- c(NA, diff(dominican_republic$lo_oct_local, lag=1, differences=1))
dominican_republic$pace_lo_oct <- ifelse(dominican_republic$diff_lo_oct>=quantile(dominican_republic$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# dominican_republic$pace_lo_oct =dominican_republic$pace_dummy*dominican_republic$price_diff_lo
dominican_republic$diff_price_ind<- c(NA, diff(dominican_republic$price_ind, lag=1, differences=1))
dominican_republic$pace_price_ind <- ifelse(dominican_republic$diff_price_ind>=quantile(dominican_republic$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# dominican_republic$pace_price_ind =dominican_republic$pace_price_ind*dominican_republic$diff_price_ind


dominican_republic = dominican_republic %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                                   lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                                                   pace_lo_oct, pace_price_ind)

##### 3.8 Ecuador  ######
ecuador <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_ecuador.xlsx",sheet=2,skip=6))
colnames(ecuador)[1] <- "date"
ecuador <- ecuador %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
ecuador$date <- transforming_date(ecuador$date)
ecuador$month <- substr(ecuador$date, 1,2)
ecuador$year <- substr(ecuador$date, 6,9)
ecuador$day <- "01"
ecuador$date<- as.Date(paste(ecuador$year, ecuador$month, ecuador$day, sep="/"), "%Y/%m/%d")
ecuador=ecuador%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2016/12/01"), "months"))

ecuador=full_join(daty, ecuador, by="date")
ecuador$nat_gas_usd=NA
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
ecuador[cols.num] <- sapply(ecuador[cols.num],as.numeric)
sapply(ecuador, class)
#converting to local currency
#ECUADOR uses the US dollar as the currency since 2000, therefore we use USD as local currency 
ecuador$lo_oct_local=ecuador$lo_oct_usd
ecuador$hi_oct_local=ecuador$hi_oct_usd
ecuador$diesel_local=ecuador$diesel_usd
ecuador$nat_gas_local=ecuador$nat_gas_usd
ecuador$exchange_olade=NA
ecuador$exchange_fred=NA
ecuador$country="Ecuador"
ecuador_monthly=ecuador
ecuador_monthly=ecuador_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                           lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)

# #converting weekly to monthly 
ecuador=ecuador %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=NA,
            exchange_fred=NA)
ecuador$quarterly = as.yearqtr(ecuador$date,format="%Yq%q")
ecuador$last_month = as.Date(ecuador$quarterly, frac=1)    
ecuador$country <- "Ecuador"
summary(ecuador)
ecuador[ecuador == 0] <- NA
summary(ecuador)
rm(daty)
#REPLACING NANs with NA in an efficient way
ecuador[is.na(ecuador)]=NA
#obtaining the prices collected on my own and replacing NAs with these prices 
price.ecu.own=as.data.frame(read_csv("03_Data_Country by country/Ecuador/master_ecuador_R.csv"))
#I replace the actual price by the announced price in October 2019 due to the subsidy removal
price.ecu.own[118, "gasoline_low"]<-2.3
price.ecu.own=select(price.ecu.own, date, gasoline_low, gasoline_high) %>% 
  group_by(date=format(as.yearqtr(date), "%Yq%q")) %>%
  summarize(lo_oct_usd_own=(mean(gasoline_low, na.rm=T))/0.02101212998713,
            hi_oct_usd_own=(mean(gasoline_high, na.rm=T))/0.02101212998713)
ecuador=full_join(ecuador,price.ecu.own)
ecuador=ecuador %>% mutate(lo_oct_usd=coalesce(lo_oct_usd, lo_oct_usd_own))
ecuador=ecuador %>% mutate(hi_oct_usd=coalesce(hi_oct_usd, hi_oct_usd_own))
ecuador$lo_oct_local=ecuador$lo_oct_usd
ecuador$hi_oct_local=ecuador$hi_oct_usd
#Getting the price indicator, sum 
ecuador=ecuador %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=ecuador$sum[1]
ecuador$price_ind=(ecuador$sum/index_base)*100
colnames(ecuador)
ecuador[ecuador == 0] <- NA
ecuador$quarterly = as.yearqtr(ecuador$date,format="%Yq%q")
ecuador$last_month = as.Date(ecuador$quarterly, frac=1) 
ecuador$country=rep("Ecuador",129)
# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
ecuador$diff_lo_oct<- c(NA, diff(ecuador$lo_oct_local, lag=1, differences=1))
ecuador$pace_lo_oct <- ifelse(ecuador$diff_lo_oct>=quantile(ecuador$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# ecuador$pace_lo_oct =ecuador$pace_dummy*ecuador$price_diff_lo
ecuador$diff_price_ind<- c(NA, diff(ecuador$price_ind, lag=1, differences=1))
ecuador$pace_price_ind <- ifelse(ecuador$diff_price_ind>=quantile(ecuador$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# ecuador$pace_price_ind =ecuador$pace_price_ind*ecuador$diff_price_ind


ecuador = ecuador %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                             lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                             pace_lo_oct, pace_price_ind)

##### 3.9 El Salvador ##### 
el_salvador <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_elsalvador.xlsx",sheet=2,skip=6))
colnames(el_salvador)[1] <- "date"
el_salvador <- el_salvador %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
el_salvador$date <- transforming_date(el_salvador$date)
el_salvador$month <- substr(el_salvador$date, 1,2)
el_salvador$year <- substr(el_salvador$date, 6,9)
el_salvador$day <- "01"
el_salvador$date<- as.Date(paste(el_salvador$year, el_salvador$month, el_salvador$day, sep="/"), "%Y/%m/%d")
el_salvador=el_salvador%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2017/12/01"), "months"))
el_salvador=full_join(daty, el_salvador, by="date")
el_salvador$nat_gas_usd=NA
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
el_salvador[cols.num] <- sapply(el_salvador[cols.num],as.numeric)
sapply(el_salvador, class)
#converting to local currency
#EL SALVADOR uses the US dollar as the currency since 2001, therefore we use USD as local currency 
el_salvador$lo_oct_local=el_salvador$lo_oct_usd
el_salvador$hi_oct_local=el_salvador$hi_oct_usd
el_salvador$diesel_local=el_salvador$diesel_usd
el_salvador$nat_gas_local=NA
el_salvador$exchange_fred=NA
el_salvador$exchange_olade=NA
el_salvador$country="ElSalvador"
el_salvador_monthly=el_salvador
el_salvador_monthly=el_salvador_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                                   lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
el_salvador= el_salvador%>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=NA,
            exchange_fred=NA)
el_salvador$quarterly = as.yearqtr(el_salvador$date,format="%Yq%q")
el_salvador$last_month = as.Date(el_salvador$quarterly, frac=1)    
el_salvador$country <- "ElSalvador"
summary(el_salvador)
el_salvador[el_salvador == 0] <- NA
summary(el_salvador)
rm(other, natural_gas, daty)

el_salvador$lo_oct_local[is.nan(el_salvador$lo_oct_local)]=NA
el_salvador$hi_oct_local[is.nan(el_salvador$hi_oct_local)]=NA
el_salvador$diesel_local[is.nan(el_salvador$diesel_local)]=NA
el_salvador$nat_gas_local[is.nan(el_salvador$nat_gas_local)]=NA
el_salvador=el_salvador %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=el_salvador$sum[1]
el_salvador$price_ind=(el_salvador$sum/index_base)*100
colnames(el_salvador)
el_salvador[el_salvador == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
el_salvador$diff_lo_oct<- c(NA, diff(el_salvador$lo_oct_local, lag=1, differences=1))
el_salvador$pace_lo_oct <- ifelse(el_salvador$diff_lo_oct>=quantile(el_salvador$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# el_salvador$pace_lo_oct =el_salvador$pace_dummy*el_salvador$price_diff_lo
el_salvador$diff_price_ind<- c(NA, diff(el_salvador$price_ind, lag=1, differences=1))
el_salvador$pace_price_ind <- ifelse(el_salvador$diff_price_ind>=quantile(el_salvador$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# el_salvador$pace_price_ind =el_salvador$pace_price_ind*el_salvador$diff_price_ind


el_salvador = el_salvador %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                     lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                                     pace_lo_oct, pace_price_ind)

##### 3.10 Guatemala ##### 
guatemala <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_guatemala.xlsx",sheet=2,skip=6))
colnames(guatemala)[1] <- "date"
guatemala <- guatemala %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
guatemala$date <- transforming_date(guatemala$date)
guatemala$month <- substr(guatemala$date, 1,2)
guatemala$year <- substr(guatemala$date, 6,9)
guatemala$day <- "01"
guatemala$date<- as.Date(paste(guatemala$year, guatemala$month, guatemala$day, sep="/"), "%Y/%m/%d")
guatemala=guatemala%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2016/12/01"), "months"))

guatemala=full_join(daty, guatemala, by="date")
guatemala$nat_gas_usd=NA
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
guatemala[cols.num] <- sapply(guatemala[cols.num],as.numeric)
sapply(guatemala, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=4072))
exchange <- exchange[c(1:328),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
guatemala <- left_join(guatemala, exchange, by="date")
str(guatemala)
guatemala$exchange_olade =as.numeric(guatemala$exchange_olade)
#NO FRED DATA, SO WE USE OLADE FOR THE EXCHANGE RATE
#We decide to use olade
guatemala$lo_oct_local = guatemala$lo_oct_usd*guatemala$exchange_olade
guatemala$hi_oct_local = guatemala$hi_oct_usd*guatemala$exchange_olade
guatemala$diesel_local = guatemala$diesel_usd*guatemala$exchange_olade
guatemala$nat_gas_local = guatemala$nat_gas_usd*guatemala$exchange_olade
guatemala$exchange_fred=NA
guatemala$country="Guatemala"
guatemala_monthly=guatemala
guatemala_monthly=guatemala_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                               lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
guatemala = guatemala %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
guatemala$quarterly = as.yearqtr(guatemala$date,format="%Yq%q")
guatemala$last_month = as.Date(guatemala$quarterly, frac=1)    
guatemala$country <- "Guatemala"
summary(guatemala)
guatemala[guatemala == 0] <- NA
summary(guatemala)
rm(other, natural_gas, daty)

guatemala$lo_oct_local[is.nan(guatemala$lo_oct_local)]=NA
guatemala$hi_oct_local[is.nan(guatemala$hi_oct_local)]=NA
guatemala$diesel_local[is.nan(guatemala$diesel_local)]=NA
guatemala$nat_gas_local[is.nan(guatemala$nat_gas_local)]=NA
guatemala=guatemala %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=guatemala$sum[1]
guatemala$price_ind=(guatemala$sum/index_base)*100
colnames(guatemala)
guatemala[guatemala == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
guatemala$diff_lo_oct<- c(NA, diff(guatemala$lo_oct_local, lag=1, differences=1))
guatemala$pace_lo_oct <- ifelse(guatemala$diff_lo_oct>=quantile(guatemala$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# guatemala$pace_lo_oct =guatemala$pace_dummy*guatemala$price_diff_lo
guatemala$diff_price_ind<- c(NA, diff(guatemala$price_ind, lag=1, differences=1))
guatemala$pace_price_ind <- ifelse(guatemala$diff_price_ind>=quantile(guatemala$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# guatemala$pace_price_ind =guatemala$pace_price_ind*guatemala$diff_price_ind


guatemala = guatemala %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                 lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                                 pace_lo_oct, pace_price_ind)

##### 3.11 Honduras ##### 
honduras <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_honduras.xlsx",sheet=2,skip=6))
colnames(honduras)[1] <- "date"
honduras <- honduras %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
honduras$date <- transforming_date(honduras$date)
honduras$month <- substr(honduras$date, 1,2)
honduras$year <- substr(honduras$date, 6,9)
honduras$day <- "01"
honduras$date<- as.Date(paste(honduras$year, honduras$month, honduras$day, sep="/"), "%Y/%m/%d")
honduras=honduras%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2010/07/01"), "months"))

honduras=full_join(daty, honduras, by="date")
honduras$nat_gas_usd=NA
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
honduras[cols.num] <- sapply(honduras[cols.num],as.numeric)
sapply(honduras, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=1352))
exchange <- exchange[c(1:314),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
honduras <- left_join(honduras, exchange, by="date")
str(honduras)
honduras$exchange_olade =as.numeric(honduras$exchange_olade)
#NO FRED DATA, SO WE USE OLADE FOR THE EXCHANGE RATE
#We decide to use olade
honduras$lo_oct_local = honduras$lo_oct_usd*honduras$exchange_olade
honduras$hi_oct_local = honduras$hi_oct_usd*honduras$exchange_olade
honduras$diesel_local = honduras$diesel_usd*honduras$exchange_olade
honduras$nat_gas_local = honduras$nat_gas_usd*honduras$exchange_olade
honduras$exchange_fred=NA
honduras$country="Honduras"
honduras_monthly=honduras
honduras_monthly=honduras_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                             lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)

# #converting weekly to monthly 
honduras = honduras %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
honduras$quarterly = as.yearqtr(honduras$date,format="%Yq%q")
honduras$last_month = as.Date(honduras$quarterly, frac=1)    
honduras$country <- "Honduras"
summary(honduras)
honduras[honduras == 0] <- NA
summary(honduras)
rm(other, natural_gas, daty)


honduras$lo_oct_local[is.nan(honduras$lo_oct_local)]=NA
honduras$hi_oct_local[is.nan(honduras$hi_oct_local)]=NA
honduras$diesel_local[is.nan(honduras$diesel_local)]=NA
honduras$nat_gas_local[is.nan(honduras$nat_gas_local)]=NA
honduras=honduras %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=honduras$sum[1]
honduras$price_ind=(honduras$sum/index_base)*100
colnames(honduras)
honduras[honduras == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
honduras$diff_lo_oct<- c(NA, diff(honduras$lo_oct_local, lag=1, differences=1))
honduras$pace_lo_oct <- ifelse(honduras$diff_lo_oct>=quantile(honduras$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# honduras$pace_lo_oct =honduras$pace_dummy*honduras$price_diff_lo
honduras$diff_price_ind<- c(NA, diff(honduras$price_ind, lag=1, differences=1))
honduras$pace_price_ind <- ifelse(honduras$diff_price_ind>=quantile(honduras$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# honduras$pace_price_ind =honduras$pace_price_ind*honduras$diff_price_ind


honduras = honduras %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                               lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                               pace_lo_oct, pace_price_ind)


##### 3.12 Mexico  ##### 
mexico <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_mexico.xlsx",sheet=2,skip=570))
natural_gas <- mexico[c(1:334),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming_date(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)

other <- mexico[c(340:688),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
mexico = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2017/10/01"), "months"))

mexico=full_join(daty, mexico, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
mexico[cols.num] <- sapply(mexico[cols.num],as.numeric)
sapply(mexico, class)
#1084.97860 is unrealistic and is a mistake, it should be 108.49786
mexico[248,4]=108.49786
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=3432))
exchange <- exchange[c(1:170),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
mexico <- left_join(mexico, exchange, by="date")
str(mexico)
mexico$exchange_olade =as.numeric(mexico$exchange_olade)
exchange_fred <- read_csv("/Users/montesdeoca/Dropbox/FoReSee - DIW/FFSubsidies/03_Data_Country by country/Exchange rates/Mexico.csv")
colnames(exchange_fred)[1] <- "date"
colnames(exchange_fred)[2] <- "exchange_fred"
exchange_fred$date <- as.Date(exchange_fred$date)
mexico <- left_join(mexico, exchange_fred, by="date")
#we use the exchange rate with less NAs according to the next function(s):
ifelse(sum(is.na(mexico$exchange_fred))>=sum(is.na(mexico$exchange_olade)),"Olade", "Fred")
# A smarter one
datframe_fred = mexico %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_fred)
datframe_olade = mexico %>% select(hi_oct_usd, lo_oct_usd, diesel_usd, nat_gas_usd, exchange_olade)
ifelse(sum(is.na(datframe_fred))>=sum(is.na(datframe_olade)),"Olade", "Fred")



#obtaining the prices collected on my own and replacing NAs with these prices 
price.mex.own=as.data.frame(read_csv("03_Data_Country by country/Mexico_gasoline/01_Data/master_data_mexico.csv"))
price.mex.own=select(price.mex.own, date, price_low)
mexico=left_join(mexico, price.mex.own)
#THE price_low HERE IS IN MXN/L, TO CONVERT WE HAVE TO 
#first, converting prices in MXN/L to USD per liter
mexico$price_low_usd_l<-mexico$price_low/mexico$exchange_fred
#then, convert USD/l to USD/BBL
mexico$price_low_usd_bbl=mexico$price_low_usd_l*158.987
View(mexico[,c("lo_oct_usd","price_low_usd_bbl")])

mexico=mexico %>% mutate(lo_oct_usd=coalesce(lo_oct_usd, price_low_usd_bbl))

#We decide to use FRED
mexico$lo_oct_local = mexico$lo_oct_usd*mexico$exchange_fred
mexico$hi_oct_local = mexico$hi_oct_usd*mexico$exchange_fred
mexico$diesel_local = mexico$diesel_usd*mexico$exchange_fred
mexico$nat_gas_local = mexico$nat_gas_usd*mexico$exchange_fred
#Completing the dataset
mexico$country="Mexico"
mexico_monthly=mexico
mexico_monthly=mexico_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                         lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
mexico = mexico %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=mean(exchange_fred))
mexico$quarterly = as.yearqtr(mexico$date,format="%Yq%q")
mexico$last_month = as.Date(mexico$quarterly, frac=1)    
mexico$country <- "Mexico"
summary(mexico)
mexico[mexico == 0] <- NA
summary(mexico)
rm(other, natural_gas, daty)

mexico$lo_oct_local[is.nan(mexico$lo_oct_local)]=NA
mexico$hi_oct_local[is.nan(mexico$hi_oct_local)]=NA
mexico$diesel_local[is.nan(mexico$diesel_local)]=NA
mexico$nat_gas_local[is.nan(mexico$nat_gas_local)]=NA
mexico=mexico %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=mexico$sum[24]
mexico$price_ind=(mexico$sum/index_base)*100
colnames(mexico)
mexico[mexico == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
mexico$diff_lo_oct<- c(NA, diff(mexico$lo_oct_local, lag=1, differences=1))
mexico$pace_lo_oct <- ifelse(mexico$diff_lo_oct>=quantile(mexico$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# mexico$pace_lo_oct =mexico$pace_dummy*mexico$price_diff_lo
mexico$diff_price_ind<- c(NA, diff(mexico$price_ind, lag=1, differences=1))
mexico$pace_price_ind <- ifelse(mexico$diff_price_ind>=quantile(mexico$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# mexico$pace_price_ind =mexico$pace_price_ind*mexico$diff_price_ind


mexico = mexico %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                           lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                           pace_lo_oct, pace_price_ind)

##### 3.13 Nicaragua  ##### 
nicaragua <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_nicaragua.xlsx",sheet=2,skip=6))
colnames(nicaragua)[1] <- "date"
nicaragua <- nicaragua %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
nicaragua$date <- transforming_date(nicaragua$date)
nicaragua$month <- substr(nicaragua$date, 1,2)
nicaragua$year <- substr(nicaragua$date, 6,9)
nicaragua$day <- "01"
nicaragua$date<- as.Date(paste(nicaragua$year, nicaragua$month, nicaragua$day, sep="/"), "%Y/%m/%d")
nicaragua=nicaragua%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2019/08/01"), "months"))
nicaragua=full_join(daty, nicaragua, by="date")
nicaragua$nat_gas_usd=NA
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
nicaragua[cols.num] <- sapply(nicaragua[cols.num],as.numeric)
sapply(nicaragua, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=1101))
exchange <- exchange[c(1:248),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
nicaragua <- left_join(nicaragua, exchange, by="date")
str(nicaragua)
nicaragua$exchange_olade =as.numeric(nicaragua$exchange_olade)
#NO FRED DATA, SO WE USE OLADE FOR THE EXCHANGE RATE
#We decide to use olade
nicaragua$lo_oct_local = nicaragua$lo_oct_usd*nicaragua$exchange_olade
nicaragua$hi_oct_local = nicaragua$hi_oct_usd*nicaragua$exchange_olade
nicaragua$diesel_local = nicaragua$diesel_usd*nicaragua$exchange_olade
nicaragua$nat_gas_local = nicaragua$nat_gas_usd*nicaragua$exchange_olade
nicaragua$exchange_fred=NA
nicaragua$country="Nicaragua"
nicaragua_monthly=nicaragua
nicaragua_monthly=nicaragua_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                               lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
nicaragua = nicaragua %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
nicaragua$quarterly = as.yearqtr(nicaragua$date,format="%Yq%q")
nicaragua$last_month = as.Date(nicaragua$quarterly, frac=1)    
nicaragua$country <- "Nicaragua"
summary(nicaragua)
nicaragua[nicaragua == 0] <- NA
summary(nicaragua)
rm(other, natural_gas, daty)
nicaragua$lo_oct_local[is.nan(nicaragua$lo_oct_local)]=NA
nicaragua$hi_oct_local[is.nan(nicaragua$hi_oct_local)]=NA
nicaragua$diesel_local[is.nan(nicaragua$diesel_local)]=NA
nicaragua$nat_gas_local[is.nan(nicaragua$nat_gas_local)]=NA
nicaragua=nicaragua %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=nicaragua$sum[57]
nicaragua$price_ind=(nicaragua$sum/index_base)*100
colnames(nicaragua)
nicaragua[nicaragua == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
nicaragua$diff_lo_oct<- c(NA, diff(nicaragua$lo_oct_local, lag=1, differences=1))
nicaragua$pace_lo_oct <- ifelse(nicaragua$diff_lo_oct>=quantile(nicaragua$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# nicaragua$pace_lo_oct =nicaragua$pace_dummy*nicaragua$price_diff_lo
nicaragua$diff_price_ind<- c(NA, diff(nicaragua$price_ind, lag=1, differences=1))
nicaragua$pace_price_ind <- ifelse(nicaragua$diff_price_ind>=quantile(nicaragua$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# nicaragua$pace_price_ind =nicaragua$pace_price_ind*nicaragua$diff_price_ind


nicaragua = nicaragua %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                 lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                                 pace_lo_oct, pace_price_ind)

##### 3.14 Panama  ##### 
panama <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_panama.xlsx",sheet=2,skip=6))
colnames(panama)[1] <- "date"
panama <- panama %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
panama$date <- transforming_date(panama$date)
panama$month <- substr(panama$date, 1,2)
panama$year <- substr(panama$date, 6,9)
panama$day <- "01"
panama$date<- as.Date(paste(panama$year, panama$month, panama$day, sep="/"), "%Y/%m/%d")
panama=panama%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2019/06/01"), "months"))

panama=full_join(daty, panama, by="date")
panama$nat_gas_usd=NA
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
panama[cols.num] <- sapply(panama[cols.num],as.numeric)
sapply(panama, class)
#converting to local currency
#PNAMA uses the US dollar as the currency, therefore we use USD as local currency 
panama$lo_oct_local=panama$lo_oct_usd
panama$hi_oct_local=panama$hi_oct_usd
panama$diesel_local=panama$diesel_usd
panama$nat_gas_local=panama$nat_gas_usd
panama$exchange_fred=NA
panama$exchange_olade=NA
panama$country="Panama"
panama_monthly=panama
panama_monthly=panama_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                         lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
panama= panama%>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=NA,
            exchange_fred=NA)
panama$quarterly = as.yearqtr(panama$date,format="%Yq%q")
panama$last_month = as.Date(panama$quarterly, frac=1)    
panama$country <- "Panama"
summary(panama)
panama[panama == 0] <- NA
summary(panama)
rm(other, natural_gas, daty)
panama$lo_oct_local[is.nan(panama$lo_oct_local)]=NA
panama$hi_oct_local[is.nan(panama$hi_oct_local)]=NA
panama$diesel_local[is.nan(panama$diesel_local)]=NA
panama$nat_gas_local[is.nan(panama$nat_gas_local)]=NA
panama=panama %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=panama$sum[1]
panama$price_ind=(panama$sum/index_base)*100
colnames(panama)
panama[panama == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
panama$diff_lo_oct<- c(NA, diff(panama$lo_oct_local, lag=1, differences=1))
panama$pace_lo_oct <- ifelse(panama$diff_lo_oct>=quantile(panama$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# panama$pace_lo_oct =panama$pace_dummy*panama$price_diff_lo
panama$diff_price_ind<- c(NA, diff(panama$price_ind, lag=1, differences=1))
panama$pace_price_ind <- ifelse(panama$diff_price_ind>=quantile(panama$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# panama$pace_price_ind =panama$pace_price_ind*panama$diff_price_ind


panama = panama %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                           lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                           pace_lo_oct, pace_price_ind)


##### 3.15 Paraguay ##### 
paraguay <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_paraguay.xlsx",sheet=2,skip=42))
colnames(paraguay)[1] <- "date"
paraguay <- paraguay %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
paraguay$date <- transforming_date(paraguay$date)                  
paraguay$month <- substr(paraguay$date, 1,2)
paraguay$year <- substr(paraguay$date, 6,9)
paraguay$day <- "01"
paraguay$date<- as.Date(paste(paraguay$year, paraguay$month, paraguay$day, sep="/"), "%Y/%m/%d")
paraguay=paraguay%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2019/07/01"), "months"))

paraguay=full_join(daty, paraguay, by="date")
paraguay$nat_gas_usd=NA
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
paraguay[cols.num] <- sapply(paraguay[cols.num],as.numeric)
sapply(paraguay, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_paraguay.xlsx",sheet=1,skip=4))
exchange <- exchange[c(1:118),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
paraguay <- left_join(paraguay, exchange, by="date")
str(paraguay)
paraguay$exchange_olade =as.numeric(paraguay$exchange_olade)
#NO FRED DATA, SO WE USE OLADE FOR THE EXCHANGE RATE
#We decide to use olade
paraguay$lo_oct_local = paraguay$lo_oct_usd*paraguay$exchange_olade
paraguay$hi_oct_local = paraguay$hi_oct_usd*paraguay$exchange_olade
paraguay$diesel_local = paraguay$diesel_usd*paraguay$exchange_olade
paraguay$nat_gas_local = paraguay$nat_gas_usd*paraguay$exchange_olade
paraguay$exchange_fred=NA
paraguay$country="Paraguay"
paraguay_monthly=paraguay
paraguay_monthly=paraguay_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                             lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
paraguay = paraguay %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
paraguay$quarterly = as.yearqtr(paraguay$date,format="%Yq%q")
paraguay$last_month = as.Date(paraguay$quarterly, frac=1)    
paraguay$country <- "Paraguay"
summary(paraguay)
paraguay[paraguay == 0] <- NA
summary(paraguay)
rm(other, natural_gas, daty)
paraguay$lo_oct_local[is.nan(paraguay$lo_oct_local)]=NA
paraguay$hi_oct_local[is.nan(paraguay$hi_oct_local)]=NA
paraguay$diesel_local[is.nan(paraguay$diesel_local)]=NA
paraguay$nat_gas_local[is.nan(paraguay$nat_gas_local)]=NA
paraguay=paraguay %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=paraguay$sum[69]
paraguay$price_ind=(paraguay$sum/index_base)*100
colnames(paraguay)
paraguay[paraguay == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
paraguay$diff_lo_oct<- c(NA, diff(paraguay$lo_oct_local, lag=1, differences=1))
paraguay$pace_lo_oct <- ifelse(paraguay$diff_lo_oct>=quantile(paraguay$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# paraguay$pace_lo_oct =paraguay$pace_dummy*paraguay$price_diff_lo
paraguay$diff_price_ind<- c(NA, diff(paraguay$price_ind, lag=1, differences=1))
paraguay$pace_price_ind <- ifelse(paraguay$diff_price_ind>=quantile(paraguay$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# paraguay$pace_price_ind =paraguay$pace_price_ind*paraguay$diff_price_ind


paraguay = paraguay %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                               lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                               pace_lo_oct, pace_price_ind)

##### 3.16 Peru ##### 
peru <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_peru.xlsx",sheet=2,skip=117))
natural_gas <- peru[c(1:50),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)

other <- peru[c(55:418),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
peru = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2018/03/01"), "months"))

peru=full_join(daty, peru, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
peru[cols.num] <- sapply(peru[cols.num],as.numeric)
sapply(peru, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=1670))
exchange <- exchange[c(1:325),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
peru <- left_join(peru, exchange, by="date")
str(peru)
peru$exchange_olade =as.numeric(peru$exchange_olade)
#NO FRED DATA, SO WE USE OLADE FOR THE EXCHANGE RATE
#We decide to use olade
peru$lo_oct_local = peru$lo_oct_usd*peru$exchange_olade
peru$hi_oct_local = peru$hi_oct_usd*peru$exchange_olade
peru$diesel_local = peru$diesel_usd*peru$exchange_olade
peru$nat_gas_local = peru$nat_gas_usd*peru$exchange_olade
peru$exchange_fred=NA
peru$country="Peru"
peru_monthly=peru
peru_monthly=peru_monthly  %>% select(date,country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                      lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting weekly to monthly 
peru = peru %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
peru$quarterly = as.yearqtr(peru$date,format="%Yq%q")
peru$last_month = as.Date(peru$quarterly, frac=1)    
peru$country <- "Peru"
summary(peru)
peru[peru == 0] <- NA
summary(peru)
rm(other, natural_gas, daty)

peru$lo_oct_local[is.nan(peru$lo_oct_local)]=NA
peru$hi_oct_local[is.nan(peru$hi_oct_local)]=NA
peru$diesel_local[is.nan(peru$diesel_local)]=NA
peru$nat_gas_local[is.nan(peru$nat_gas_local)]=NA
peru=peru %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=peru$sum[15]
peru$price_ind=(peru$sum/index_base)*100
colnames(peru)
peru[peru == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
peru$diff_lo_oct<- c(NA, diff(peru$lo_oct_local, lag=1, differences=1))
peru$pace_lo_oct <- ifelse(peru$diff_lo_oct>=quantile(peru$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# peru$pace_lo_oct =peru$pace_dummy*peru$price_diff_lo
peru$diff_price_ind<- c(NA, diff(peru$price_ind, lag=1, differences=1))
peru$pace_price_ind <- ifelse(peru$diff_price_ind>=quantile(peru$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# peru$pace_price_ind =peru$pace_price_ind*peru$diff_price_ind


peru = peru %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                       lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                       pace_lo_oct, pace_price_ind)

##### 3.17 Uruguay  ##### 

uruguay <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_uruguay.xlsx",sheet=2,skip=377))
natural_gas <- uruguay[c(1:182),c(1:2)]
colnames(natural_gas)[1] <- "date"
colnames(natural_gas)[2] <- "nat_gas_usd"
#changing the date variable
natural_gas$date <- transforming_date(natural_gas$date)
natural_gas$month <- substr(natural_gas$date, 1,2)
natural_gas$year <- substr(natural_gas$date, 6,9)
natural_gas$day <- "01"
natural_gas$date<- as.Date(paste(natural_gas$year, natural_gas$month, natural_gas$day, sep="/"), "%Y/%m/%d")
natural_gas=natural_gas%>%dplyr::select(date,nat_gas_usd)

other <- uruguay[c(188:548),]
colnames(other) <- other[1,]
other <- other[-1,]
colnames(other)[1] <- "date"
other <- other %>% dplyr::select(date, diesel_usd="Diésel Oil", hi_oct_usd="Gasolina Premiun", lo_oct_usd="Gasolina Regular" )
#changing the date variable
other$date <- transforming_date(other$date)                  
other$month <- substr(other$date, 1,2)
other$year <- substr(other$date, 6,9)
other$day <- "01"
other$date<- as.Date(paste(other$year, other$month, other$day, sep="/"), "%Y/%m/%d")
other=other%>% dplyr::select(date,diesel_usd,hi_oct_usd,lo_oct_usd)
uruguay = full_join(other, natural_gas, by="date")
daty = data.frame(date=seq(as.Date("1988/01/01"), as.Date("2017/12/01"), "months"))

uruguay=full_join(daty, uruguay, by="date")
cols.num <- c("lo_oct_usd", "hi_oct_usd", "diesel_usd", "nat_gas_usd")
uruguay[cols.num] <- sapply(uruguay[cols.num],as.numeric)
sapply(uruguay, class)
#converting to local currency
exchange <- as.data.frame(read_excel("03_Data_Country by country/Olade database/olade_exchange_rates.xlsx",sheet=2,skip=3605))
exchange <- exchange[c(1:463),]
colnames(exchange)[1] <- "date"
colnames(exchange)[2] <- "exchange_olade"
source("05_Scripts/transforming_date.R")
exchange$date <- transforming_date(exchange$date)
exchange$year <- substr(exchange$date, 6,9)
exchange$month <- substr(exchange$date, 1,2)
exchange$day <- "01"
exchange$date<- as.Date(paste(exchange$year, exchange$month, exchange$day, sep="/"), "%Y/%m/%d")
exchange = exchange %>% drop_na(date)
uruguay <- left_join(uruguay, exchange, by="date")
str(uruguay)
uruguay$exchange_olade =as.numeric(uruguay$exchange_olade)
#NO FRED DATA, SO WE USE OLADE FOR THE EXCHANGE RATE
#We decide to use olade
uruguay$lo_oct_local = uruguay$lo_oct_usd*uruguay$exchange_olade
uruguay$hi_oct_local = uruguay$hi_oct_usd*uruguay$exchange_olade
uruguay$diesel_local = uruguay$diesel_usd*uruguay$exchange_olade
uruguay$nat_gas_local = uruguay$nat_gas_usd*uruguay$exchange_olade
uruguay$exchange_fred=NA
uruguay$country="Uruguay"
uruguay_monthly=uruguay
uruguay_monthly=uruguay_monthly %>% select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                                           lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, exchange_olade, exchange_fred)
# #converting monthly to quarterly
uruguay = uruguay %>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
  summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
            hi_oct_usd=mean(hi_oct_usd, na.rm=T),
            diesel_usd=mean(diesel_usd, na.rm=T),
            nat_gas_usd=mean(nat_gas_usd, na.rm=T),
            lo_oct_local=mean(lo_oct_local, na.rm=T),
            hi_oct_local=mean(hi_oct_local, na.rm=T),
            diesel_local=mean(diesel_local, na.rm=T),
            nat_gas_local=mean(nat_gas_local, na.rm=T), 
            exchange_olade=mean(exchange_olade),
            exchange_fred=NA)
uruguay$quarterly = as.yearqtr(uruguay$date,format="%Yq%q")
uruguay$last_month = as.Date(uruguay$quarterly, frac=1)    
uruguay$country <- "Uruguay"
summary(uruguay)
uruguay[uruguay == 0] <- NA
summary(uruguay)
rm(other, natural_gas, daty)
uruguay$lo_oct_local[is.nan(uruguay$lo_oct_local)]=NA
uruguay$hi_oct_local[is.nan(uruguay$hi_oct_local)]=NA
uruguay$diesel_local[is.nan(uruguay$diesel_local)]=NA
uruguay$nat_gas_local[is.nan(uruguay$nat_gas_local)]=NA
uruguay=uruguay %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=uruguay$sum[1]
uruguay$price_ind=(uruguay$sum/index_base)*100
colnames(uruguay)
uruguay[uruguay == 0] <- NA

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
uruguay$diff_lo_oct<- c(NA, diff(uruguay$lo_oct_local, lag=1, differences=1))
uruguay$pace_lo_oct <- ifelse(uruguay$diff_lo_oct>=quantile(uruguay$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# uruguay$pace_lo_oct =uruguay$pace_dummy*uruguay$price_diff_lo
uruguay$diff_price_ind<- c(NA, diff(uruguay$price_ind, lag=1, differences=1))
uruguay$pace_price_ind <- ifelse(uruguay$diff_price_ind>=quantile(uruguay$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# uruguay$pace_price_ind =uruguay$pace_price_ind*uruguay$diff_price_ind


uruguay = uruguay %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                             lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                             pace_lo_oct, pace_price_ind)

#Rbinding the quarterly data for all countries
prices <- rbind(argentina, bolivia, brazil, chile, colombia, costa_rica, dominican_republic, ecuador, el_salvador, guatemala,
                honduras, mexico, nicaragua, panama, paraguay, peru, uruguay)
rm(argentina, bolivia, brazil, chile, colombia, costa_rica, dominican_republic, ecuador, el_salvador, guatemala,
   honduras, mexico, nicaragua, panama, paraguay, peru, uruguay, cols.num, datframe_fred, datframe_olade, exchange, exchange_fred)
#Rbinding the monthly data for all countries
prices_monthly = rbind(argentina_monthly, bolivia_monthly, brazil_monthly, chile_monthly, colombia_monthly, costa_rica_monthly, dominican_republic_monthly,
                       ecuador_monthly, el_salvador_monthly, guatemala_monthly, honduras_monthly, mexico_monthly, nicaragua_monthly, 
                       panama_monthly, paraguay_monthly, peru_monthly, uruguay_monthly)
rm(argentina_monthly, bolivia_monthly, brazil_monthly, chile_monthly, colombia_monthly, costa_rica_monthly, dominican_republic_monthly,
   ecuador_monthly, el_salvador_monthly, guatemala_monthly, honduras_monthly, mexico_monthly, nicaragua_monthly, 
   panama_monthly, paraguay_monthly, peru_monthly, uruguay_monthly)


##### 3.18 United States  ##### 
getwd()
usprices=as.data.frame(read_excel("03_Data_Country by country/United States/fuel_prices_us.xls", sheet=2, skip=2))
usprices=usprices[, c(1, 5,11,14)]
colnames(usprices)
usprices=usprices %>% select("date"="Date", 
                             "lo_oct_usd_gallon"="U.S. Regular All Formulations Retail Gasoline Prices (Dollars per Gallon)",
                             "hi_oct_usd_gallon"="U.S. Premium All Formulations Retail Gasoline Prices (Dollars per Gallon)",
                             "diesel_usd_gallon"="U.S. No 2 Diesel Retail Prices (Dollars per Gallon)")
usprices$date=as.Date(usprices$date, "%Y-%m-%d")
usprices$lo_oct_usd_bbl=usprices$lo_oct_usd_gallon/0.0238095 
usprices$hi_oct_usd_bbl=usprices$hi_oct_usd_gallon/0.0238095 
usprices$diesel_usd_bbl=usprices$diesel_usd_gallon/0.0238095
usprices$country="United States"
usprices=usprices %>% select(date, country, lo_oct_local=lo_oct_usd_bbl, hi_oct_local=hi_oct_usd_bbl,
                             diesel_local=diesel_usd_bbl)
usprices$lo_oct_usd=usprices$lo_oct_local
usprices$hi_oct_usd=usprices$hi_oct_local
usprices$diesel_usd=usprices$diesel_local

us_gas_prices=as.data.frame(read_excel("03_Data_Country by country/United States/nat_gas_prices_us.xls", sheet=2, skip=2))
colnames(us_gas_prices)[1]="date"
colnames(us_gas_prices)[2]="nat_gas_usd_thousand3feet"
us_gas_prices$nat_gas_usd_bbl=us_gas_prices$nat_gas_usd_thousand3feet*178.108
us_gas_prices$country="United States"
us_gas_prices$date=as.Date(us_gas_prices$date, "%Y-%m-%d")
us_gas_prices=us_gas_prices %>% select(date, country, nat_gas_local=nat_gas_usd_bbl)
us_gas_prices$nat_gas_usd=us_gas_prices$nat_gas_local
usprices=left_join(usprices, us_gas_prices, by=c("date", "country"))
usprices=usprices%>%select(date, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd, lo_oct_local, hi_oct_local, diesel_local, nat_gas_local)
usprices$exchange_olade=NA
usprices$exchange_fred=NA
#Adding US prices to the monthly data set and saving
prices_monthly=rbind(prices_monthly, usprices)
write.csv(prices_monthly,"03_Data_Country by country/Generated_data/prices_monthly_olade.csv", row.names = F)
#converting to quarterly and saving in the quarterly dataset
usprices   =          usprices%>% group_by(date=format(as.yearqtr(date, "%b-%Y"), "%Yq%q")) %>%
                      summarise(lo_oct_usd=mean(lo_oct_usd, na.rm=T),
                                hi_oct_usd=mean(hi_oct_usd, na.rm=T),
                                diesel_usd=mean(diesel_usd, na.rm=T),
                                nat_gas_usd=mean(nat_gas_usd, na.rm=T),
                                lo_oct_local=mean(lo_oct_local, na.rm=T),
                                hi_oct_local=mean(hi_oct_local, na.rm=T),
                                diesel_local=mean(diesel_local, na.rm=T),
                                nat_gas_local=mean(nat_gas_local, na.rm=T), 
                                exchange_olade=NA,
                                exchange_fred=NA)
#getting the price index 
usprices$quarterly = as.yearqtr(usprices$date,format="%Yq%q")
usprices$last_month = as.Date(usprices$quarterly, frac=1) 
usprices$country="United States"
usprices$lo_oct_local[is.nan(usprices$lo_oct_local)]=NA
usprices$hi_oct_local[is.nan(usprices$hi_oct_local)]=NA
usprices$diesel_local[is.nan(usprices$diesel_local)]=NA
usprices$nat_gas_local[is.nan(usprices$nat_gas_local)]=NA
usprices=usprices %>% mutate(sum = select(., lo_oct_local, hi_oct_local, diesel_local) %>% rowSums(na.rm = F))
index_base=usprices$sum[18]
usprices$price_ind=(usprices$sum/index_base)*100
colnames(usprices)
usprices[usprices == 0] <- NA
colnames(prices)
usprices = usprices %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                               lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred)
colnames(prices)

# Creating the pace variable and adding it to the dataset
# We create the dummy variable with those values (price hikes) higher than the 90 percentile
usprices$diff_lo_oct<- c(NA, diff(usprices$lo_oct_local, lag=1, differences=1))
usprices$pace_lo_oct <- ifelse(usprices$diff_lo_oct>=quantile(usprices$diff_lo_oct, .75, na.rm=TRUE), 1 , 0)
# usprices$pace_lo_oct =usprices$pace_dummy*usprices$price_diff_lo
usprices$diff_price_ind<- c(NA, diff(usprices$price_ind, lag=1, differences=1))
usprices$pace_price_ind <- ifelse(usprices$diff_price_ind>=quantile(usprices$diff_price_ind, .75, na.rm=TRUE), 1 , 0)
# usprices$pace_price_ind =usprices$pace_price_ind*usprices$diff_price_ind


usprices = usprices %>% select(date, quarterly, last_month, country, lo_oct_usd, hi_oct_usd, diesel_usd, nat_gas_usd,
                               lo_oct_local, hi_oct_local, diesel_local, nat_gas_local, price_ind, exchange_olade, exchange_fred,
                               pace_lo_oct, pace_price_ind)
prices=rbind(prices, usprices)

##### 3.19 Exporting the price data  ##### 
getwd()
write.csv(prices_monthly,"03_Data_Country by country/Generated_data/prices_monthly_olade.csv", row.names = F)
write.csv(prices,"03_Data_Country by country/Generated_data/prices_quarterly_olade.csv", row.names = F)

##########################
##### 4. Merging price and presidential approval data  ##### 
##########################

#merging with the presidential data(
prices=prices %>% select(last_month, country, lo_oct_usd_olade=lo_oct_usd, hi_oct_usd_olade=hi_oct_usd, 
                         diesel_usd_olade=diesel_usd, nat_gas_usd_olade=nat_gas_usd,
                         lo_oct_local_olade=lo_oct_local, hi_oct_local_olade=hi_oct_local, diesel_local_olade=diesel_local,
                         nat_gas_local_olade=nat_gas_local, price_ind,exchange_olade, exchange_fred,
                         pace_lo_oct, pace_price_ind)
data= left_join(data, prices, by=c("country", "last_month"))

data1= data %>% 
                apply_labels(lo_oct_usd_olade="USD/bbl",
                             hi_oct_usd_olade="USD/bbl",
                             diesel_usd_olade="USD/bbl",
                             nat_gas_usd_olade="USD/km3",
                             lo_oct_local_olade="LCU/bbl",
                             hi_oct_local_olade="LCU/bbl",
                             diesel_local_olade="LCU/bbl",
                             nat_gas_local_olade="LCU/km3", 
                             price_ind="USD/bbl")

##### 4.1 Exporting merged price and presidential approval data  ##### 

write_csv(data1, "11_GeneratedData/02_Prices/Approval+Prices.csv")

##########################
#### 5. Adding all economic variables IMF ####
##########################
#### 5.1 GDP  ####
gdp=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/quarterly_gdp_growth_year_to_year.xlsx", sheet=1,skip=1))
colnames(gdp)[1] = "country"
colnames(gdp)[2] = "date"
colnames(gdp)[3] = "gdp_growth_imf"
unique(gdp$country)
gdp$country=gsub("Dominican Rep.", "Dominican Republic", gdp$country)
# Adding US econ data ##
us_econ=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/econ_united_States.xlsx", sheet=1, skip=2))
colnames(us_econ)[1] = "date"
colnames(us_econ)[3] = "gdp_growth_imf"
us_econ$country="United States"
us_econ=us_econ%>%select(country, date, gdp_growth_imf)
gdp=rbind(gdp, us_econ)
str(gdp)
gdp$quarterly=as.yearqtr(gdp$date, format="Q%q %Y")
gdp$last_month= as.Date(gdp$quarterly, frac=1) 
gdp=gdp%>%dplyr::select(last_month, country, gdp_growth_imf)
data=left_join(data, gdp, by=c("last_month", "country"))

#### 5.2 Industrial prod.  ####
ip=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/quarterly_industrial_prod_growth_year_to_year.xlsx", skip=1))
colnames(ip)[1] = "country"
colnames(ip)[2] = "date"
colnames(ip)[3] = "ip_imf"
unique(ip$country) #NOTE THAT WE ONLY HAVE VERY FEW COUNTRIES WITH IP data :-(
### Adding US econ data ##
us_econ=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/econ_united_States.xlsx", sheet=1, skip=2))
colnames(us_econ)[1] = "date"
colnames(us_econ)[2] = "ip_imf"
us_econ$country="United States"
us_econ=us_econ%>%select(country, date, ip_imf)
ip=rbind(ip, us_econ)
ip$quarterly=as.yearqtr(ip$date, format="Q%q %Y")
ip$last_month= as.Date(ip$quarterly, frac=1) 
ip=ip%>%dplyr::select(last_month, country, ip_imf)
data=left_join(data, ip, by=c("last_month", "country"))

#### 5.3 Unemploymen.  ####
unemp=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/quarterly_unemp_rate.xlsx", skip=1))
colnames(unemp)[1]="country"
colnames(unemp)[2] = "date"
colnames(unemp)[3] = "unemp_imf"
unique(unemp$country) 
unemp$country=gsub("Dominican Rep.", "Dominican Republic", unemp$country)
### Adding US econ data ##
us_econ=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/econ_united_States.xlsx", sheet=1, skip=2))
colnames(us_econ)[1] = "date"
colnames(us_econ)[4] = "unemp_imf"
us_econ$country="United States"
us_econ=us_econ%>%select(country, date, unemp_imf)
unemp=rbind(unemp, us_econ)
unemp$quarterly=as.yearqtr(unemp$date, format="Q%q %Y")
unemp$last_month= as.Date(unemp$quarterly, frac=1) 
unemp=unemp%>%dplyr::select(last_month, country, unemp_imf)
data=left_join(data, unemp, by=c("last_month", "country"))
#### 5.4 Unemployment growth  ####
unemp_growth=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/quarterly_unemp_growth_year_to_year.xlsx", skip=1))
colnames(unemp_growth)[1]="country"
colnames(unemp_growth)[2] = "date"
colnames(unemp_growth)[3] = "unemp_growth_imf"
unique(unemp_growth$country) 
unemp_growth$country=gsub("Dominican Rep.", "Dominican Republic", unemp_growth$country)
### Adding US econ data ##
us_econ=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/econ_united_States.xlsx", sheet=1, skip=2))
colnames(us_econ)[1] = "date"
colnames(us_econ)[3] = "unemp_growth_imf"
us_econ$country="United States"
us_econ=us_econ%>%select(country, date, unemp_growth_imf)
gdp=rbind(gdp, us_econ)
unemp_growth$quarterly=as.yearqtr(unemp_growth$date, format="Q%q %Y")
unemp_growth$last_month= as.Date(unemp_growth$quarterly, frac=1) 
unemp_growth=unemp_growth%>%dplyr::select(last_month, country, unemp_growth_imf)
data=left_join(data, unemp_growth, by=c("last_month", "country"))
#### 5.5 CPI Index  ####
cpi=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/quarterly_CPI_index.xlsx", skip=1))
colnames(cpi)[1]="country"
colnames(cpi)[2] = "date"
colnames(cpi)[3] = "cpi_imf"
unique(cpi$country) 
cpi$country=gsub("Dominican Rep.", "Dominican Republic", cpi$country)
### Adding US econ data ##
us_econ=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/econ_united_States.xlsx", sheet=1, skip=2))
colnames(us_econ)[1] = "date"
colnames(us_econ)[6] = "cpi_imf"
us_econ$country="United States"
us_econ=us_econ%>%select(country, date, cpi_imf)
cpi=rbind(cpi, us_econ)
cpi$quarterly=as.yearqtr(cpi$date, format="Q%q %Y")
cpi$last_month= as.Date(cpi$quarterly, frac=1) 
cpi=cpi%>%dplyr::select(last_month, country, cpi_imf)
data=left_join(data, cpi, by=c("last_month", "country"))

#### 5.6 CPI growth  ####
cpi_growth=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/quarterly_CPI_growth_year_to_year.xlsx", skip=1))
colnames(cpi_growth)[1]="country"
colnames(cpi_growth)[2] = "date"
colnames(cpi_growth)[3] = "cpi_growth_imf"
unique(cpi_growth$country) 
cpi_growth$country=gsub("Dominican Rep.", "Dominican Republic", cpi_growth$country)
### Adding US econ data ##
us_econ=as.data.frame(read_excel("03_Data_Country by country/IMF econ data/econ_united_States.xlsx", sheet=1, skip=2))
colnames(us_econ)[1] = "date"
colnames(us_econ)[7] = "cpi_growth_imf"
us_econ$country="United States"
us_econ=us_econ%>%select(country, date, cpi_growth_imf)
cpi_growth=rbind(cpi_growth, us_econ)
cpi_growth$quarterly=as.yearqtr(cpi_growth$date, format="Q%q %Y")
cpi_growth$last_month= as.Date(cpi_growth$quarterly, frac=1) 
cpi_growth=cpi_growth%>%dplyr::select(last_month, country, cpi_growth_imf)
data=left_join(data, cpi_growth, by=c("last_month", "country"))

rm(cpi, cpi_growth, gdp, ip, unemp, unemp_growth, data1,
   price.bol.own, price.bol.own.2, price.ecu.own,
   price.mex.own, prices, prices_monthly, us_econ, us_gas_prices,
   usprices)




##########################
#### 6. Creating new variables and adding labels #####
##########################
#### 6.1 Country index #####
summary(data)
colnames(data)

data              = data %>% 
                    mutate(countryno=dplyr::group_indices(.,country)) #creating country number/id
length(unique(data$countryno))

#### 6.2 Formating date variable #####
data$quarterly   = as.yearqtr(data$quarterly, format="Q%q %Y")
str(data$quarterly)

#### 6.3 Adding real gasoline price #####
data$lo_oct_real = (data$lo_oct_local_olade/data$cpi_imf)*100 #What is the point of using local gasoline data? 

#### 6.4 Adding log variables  ####
#we log relevant variables
data$ln_approval_not_smoothed   =  log(data$approval_not_smoothed)
data$ln_approval_smoothed       =  log(data$approval_smoothed)
data$ln_lo_oct_usd_olade        =  log(data$lo_oct_usd_olade)
data$ln_lo_oct_local_olade      =  log(data$lo_oct_local_olade)
data$ln_hi_oct_local_olade      =  log(data$hi_oct_local_olade)
data$ln_diesel_local_olade      =  log(data$diesel_local_olade)
data$ln_nat_gas_local_olade     =  log(data$nat_gas_local_olade)
data$ln_price_ind               =  log(data$price_ind)
data$ln_gdp_growth_imf          =  log(data$gdp_growth_imf)
data$ln_unemp_growth_imf        =  log(data$unemp_growth_imf)
data$ln_unemp_growth_imf[is.nan(data$ln_unemp_growth_imf)]=NA
data$ln_cpi_growth_imf          =  log(data$cpi_growth_imf)
data$ln_lo_oct_real             =  log(data$lo_oct_real)
data$ln_cpi                     =  log(data$cpi_imf)
data$ln_unemp                   =  log(data$unemp_imf)

data[data=="NaN"]<-NA

#### 6.5 Adding labels #####
data      =       data %>% 
                  apply_labels(last_month        ="Last month of quarter",
                               approval_smoothed ="%",
                               approval_not_smoothed="%",
                               net_smoothed       ="%approve-%dissapprove",
                               net_not_smoothed   ="%approve-%dissapprove",
                               relative_smoothed  ="(%approve)/(%approve-%disapprove)",
                               relative_not_smoothed="(%approve)/(%approve-%disapprove)",
                               presidents         ="CategoricalPresidents",
                               duration           ="# of quarters",
                               duration.sqr       ="# of quarters sqr",
                               honey              ="Dummy, 1=honeymoon",
                               lo_oct_usd_olade   ="USD/bbl",
                               hi_oct_usd_olade   ="USD/bbl",
                               diesel_usd_olade   ="USD/bbl",
                               nat_gas_usd_olade  ="USD/km3",
                               lo_oct_local_olade ="LCU/bbl",
                               hi_oct_local_olade ="LCU/bbl",
                               diesel_local_olade ="LCU/bbl",
                               nat_gas_local_olade="LCU/km3", 
                               price_ind          ="USD/bbl",
                               gdp_growth_imf     ="realgrowth YtoY %",
                               ip_imf             ="realgrowth YtoY %",
                               unemp_imf          ="rate",
                               unemp_growth_imf   ="realgrowth YtoY %",
                               cpi_imf            ="Index 2010Q3=100",
                               cpi_growth_imf     ="realgrowth YtoY %",
                               countryno          ="Country Index",
                               ln_approval_smoothed     = "Log approve smoothed",
                               ln_approval_not_smoothed = "Log approve not smoothed",
                               ln_lo_oct_usd_olade      = "Log low-oct price",
                               ln_lo_oct_local_olade    = "Log low-oct price LCU",
                               ln_hi_oct_local_olade    = "Log high-oct price LCU",
                               ln_diesel_local_olade    = "Log diesel price LCU", 
                               ln_nat_gas_local_olade   = "Log nat gas price LCU",
                               ln_price_ind             = "Ln price index",
                               ln_gdp_growth_imf        = "Ln GDP growth",
                               ln_unemp_growth_imf      = "Ln unemp_growth",
                               ln_cpi_growth_imf        = "Ln CPI growth",
                               ln_cpi                   = "Ln cpi", 
                               ln_unemp                  = "ln unemp")



##########################
#### 7.  Some basic plots and descriptives #####
##########################
#### 7.1 Basic plots #####

#PRES VS PRICE NOMINAL TERMS
mex=data[data$country=="Mexico",]#preselecting the data to be plotted
mex=mex[48:121,]#same as above
presidents2 = ggplot(mex, aes(x=quarterly, y=approval_smoothed)) + geom_line(aes(col=presidents), size=1.5) +
  labs(subtitle = "Approval(left/color) and quarterly gasoline prices (right/black)",
       y ="Approval", x = "Date") + theme_minimal() +
  scale_y_continuous(limits=c(0,100)) +
  geom_vline(xintercept=as.Date("2010 Q4","Q%q %Y"),linetype=4, colour="black")
presidents2
presidents2 +  geom_line(aes(x= quarterly, y = lo_oct_local_olade/158.9873*4.5), size=1.5, stat = 'identity') + 
  scale_y_continuous(sec.axis = sec_axis(~./4.5, name = 'MXN per liter'), limits=c(0,100)) + theme_test() +
  scale_color_brewer(palette="Blues") +
  geom_vline(xintercept=as.numeric(mex$quarterly[37]), linetype=4, colour="black")+
  geom_text(aes(x=as.numeric(mex$quarterly[51]), label="Subsidy removal", y=95), colour="darkgray", angle=0, size=3) 
ggsave(path="02_Presentation/Brown bag 2020.08.20",
       filename="Presvsprice_nominal_mx.png", width=7, height=5)

#PRES VS PRICE REAL TERMS
presidents2 = ggplot(data[data$country=="Mexico",], aes(x=quarterly, y=approval_not_smoothed)) + geom_line(aes(col=presidents), size=1.5) +
  labs(title ="Approval(left/color) and real gasoline prices (right/black)",
       y ="Approval", x = "Date") + theme_minimal() 
presidents2
presidents2 +  geom_line(aes(x= quarterly, y = lo_oct_real/158.9873*4.5), size=1.5, stat = 'identity') + 
  scale_y_continuous(sec.axis = sec_axis(~./4.5, name = 'MXN per liter')) + theme_minimal() 
ggsave(path="02_Presentation/Brown bag 2020.08.20",
       filename="Presvsprice_real_mx.png", width=7, height=5)
#PRES VS PRICE
presidents2 = ggplot(data[data$country=="Bolivia",], aes(x=quarterly, y=approval_not_smoothed)) + geom_line(aes(col=presidents), size=1.5) +
  labs(title ="Bolivia", subtitle = "Approval(left/color) and gasoline prices (right/black)",
       y ="Approval", x = "Date") + theme_minimal() 
presidents2
presidents2 +  geom_line(aes(x= quarterly, y = lo_oct_local_olade/158.9873*4.5), size=1.5, stat = 'identity') + 
  scale_y_continuous(sec.axis = sec_axis(~./4.5, name = 'Price per liter')) + theme_minimal() 


#PRES VS PRICE BOLIVIA
library(wesanderson)
#Recent prices in bolivia are at 3.7 bol/liter
bol=data[data$country=="Bolivia",]#preselecting the data to be plotted
bol=bol[10:81,]#same as above
presidents2 = ggplot(bol, aes(x=quarterly, y=approval_smoothed)) + 
  geom_line(aes(col=presidents), size=1.5) +
  labs(subtitle ="Approval(left/color) and quarterly gasoline prices (right/black)",
       y ="Approval", x = "Date") + theme_minimal() + ylim(0,100)
presidents2 +  geom_line(aes(x= quarterly, y = lo_oct_local_olade/158.987*14), size=1.5, stat = 'identity') + 
  scale_y_continuous(sec.axis = sec_axis(~./14, name = 'Bolivianos per liter'), limits=c(0,100)) + theme_test() +
  scale_color_brewer(palette="Blues") +
  geom_vline(xintercept=as.numeric(mex$quarterly[41]), linetype=4, colour="black")+
  geom_text(aes(x=as.numeric(mex$quarterly[53]), label="Subsidy removal", y=95), colour="darkgray", angle=0, size=3) 
ggsave(path="02_Presentation/Brown bag 2020.08.20",
       filename="Presvsprice_nominal_bol.png", width=7, height=5)

#### 7.2 Descriptive stats ####
colnames(data)
data %>% dplyr::select( "Approval"="approval_smoothed",  
                        "Duration (of presidential term)"="duration",
                        "Duration squared"="duration.sqr", 
                        "Low-octane gasoline"="lo_oct_usd_olade", 
                        "GDP growth"="gdp_growth_imf", 
                        "Consumer price index"="cpi_imf", 
                        "Consumer price index growth"="cpi_growth_imf", 
                        "Industrial production"="ip_imf",
                        "Unemployment"="unemp_imf", 
                        "Unemployment growth"="unemp_growth_imf") %>%
  stargazer(title="Descriptive statistics", digits=1)


##########################
#### 8. SAVING the data until this point (for SCM) ####
##########################

write_csv(data, "03_Data_Country by country/Generated_data/complete_data.csv")




