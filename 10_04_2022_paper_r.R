library(tidyverse)
library(readxl)
library(janitor)
library(cowplot)
library(writexl)

options(scipen=999)

url2018 <- "https://mrv.emsa.europa.eu/api/public-emission-report/reporting-period-document/binary/2018?"
url2019 <- "https://mrv.emsa.europa.eu/api/public-emission-report/reporting-period-document/binary/2019?"
url2020 <- "https://mrv.emsa.europa.eu/api/public-emission-report/reporting-period-document/binary/2020?"

if (file.exists("./a2018.xlsx")) {
  
  raw_2018 <- read_excel("a2018.xlsx", skip = 2) #import all the raw 
  
} else { 
  
  download.file(url2018,destfile = "./a2018.xlsx",mode = "wb")
  raw_2018 <- read_excel("a2018.xlsx", skip = 2) #import all the raw
}

if (file.exists("./a2019.xlsx")) {
  
  raw_2019 <- read_excel("a2019.xlsx", skip = 2) #import all the raw 
  
} else { 
  
  download.file(url2019,destfile = "./a2019.xlsx",mode = "wb")
  raw_2019 <- read_excel("a2019.xlsx", skip = 2) #import all the raw
}

if (file.exists("./a2020.xlsx")) {
  
  raw_2020 <- read_excel("a2020.xlsx", skip = 2) #import all the raw 
  
} else { 
  
  download.file(url2020,destfile = "./a2020.xlsx",mode = "wb")
  raw_2020 <- read_excel("a2020.xlsx", skip = 2) #import all the raw
}

data_2018 <- raw_2018
data_2019 <- raw_2019
data_2020 <- raw_2020


#Fix the names of columns
data_2018 <- clean_names(data_2018)
data_2019 <- clean_names(data_2019)
data_2020 <- clean_names(data_2020)

detach("package:janitor", unload = TRUE)


#Change the class 
data_2018[,23:59] <- data_2018[,23:59]  %>% mutate_if(is.character, as.double)
data_2018[,23:59] <- na_if(data_2018[,23:59], 0.00)

data_2019[,23:59] <- data_2019[,23:59]  %>% mutate_if(is.character, as.double)
data_2019[,23:59] <- na_if(data_2019[,23:59], 0.00)

data_2020[,23:59] <- data_2020[,23:59]  %>% mutate_if(is.character, as.double)
data_2020[,23:59] <- na_if(data_2020[,23:59], 0.00)


#Lets see the fuel type.

#Calculate the factor 
#https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:32016R1927&from=EN ANNEX I  Table B.4.   Emission factors   

fuel_list <- c("Methanol","Ethanol","LNG","LPG","HFO","LFO","MGO")
fuel_list_CFCO2 <- c(1.375,1.913,2.750,3.015,3.114,3.151,3.206)
eth_meth_stop <- c((fuel_list_CFCO2[2] - fuel_list_CFCO2[1]) / 2) + fuel_list_CFCO2[1]
LNG_eth_stop  <- c((fuel_list_CFCO2[3] - fuel_list_CFCO2[2]) / 2) + fuel_list_CFCO2[2]
LPG_LNG_stop <- c((fuel_list_CFCO2[4] - fuel_list_CFCO2[3]) / 2) + fuel_list_CFCO2[3]
HFO_LPG_stop <- c((fuel_list_CFCO2[5] - fuel_list_CFCO2[4]) / 2) + fuel_list_CFCO2[4]
LFO_HFO_stop <- c((fuel_list_CFCO2[6] - fuel_list_CFCO2[5]) / 2) + fuel_list_CFCO2[5]
MGO_LFO_stop <- c((fuel_list_CFCO2[7] - fuel_list_CFCO2[6]) / 2) + fuel_list_CFCO2[6]
down_stop <- fuel_list_CFCO2[1] - c((fuel_list_CFCO2[2] - fuel_list_CFCO2[1]) / 2)
up_stop <- fuel_list_CFCO2[7] + c((fuel_list_CFCO2[7] - fuel_list_CFCO2[6]) / 2)


fuel_list_stop <- c(down_stop,eth_meth_stop,LNG_eth_stop,LPG_LNG_stop,HFO_LPG_stop,LFO_HFO_stop,MGO_LFO_stop,up_stop)


data_2018 <- data_2018 %>% mutate(fuel_grade_n = round(total_co2_emissions_m_tonnes/total_fuel_consumption_m_tonnes, digits = 3))
data_2018$fuel_grade <-  if_else(data_2018$fuel_grade_n > down_stop & data_2018$fuel_grade_n  < eth_meth_stop,"Methanol",
                                 if_else(data_2018$fuel_grade_n >= eth_meth_stop & data_2018$fuel_grade_n < LNG_eth_stop,"Ethanol",
                                         if_else(data_2018$fuel_grade_n >= LNG_eth_stop & data_2018$fuel_grade_n < LPG_LNG_stop,"LNG",
                                                 if_else(data_2018$fuel_grade_n >= LPG_LNG_stop & data_2018$fuel_grade_n < HFO_LPG_stop,"LPG",
                                                         if_else(data_2018$fuel_grade_n >= HFO_LPG_stop & data_2018$fuel_grade_n < LFO_HFO_stop,"HFO",
                                                                 if_else(data_2018$fuel_grade_n >= LFO_HFO_stop & data_2018$fuel_grade_n < MGO_LFO_stop,"LFO",
                                                                         if_else(data_2018$fuel_grade_n >= MGO_LFO_stop & data_2018$fuel_grade_n <= up_stop,"MGO", "NA-others")))))))



data_2019 <- data_2019 %>% mutate(fuel_grade_n = round(total_co2_emissions_m_tonnes/total_fuel_consumption_m_tonnes, digits = 3))
data_2019$fuel_grade <-  if_else(data_2019$fuel_grade_n > down_stop & data_2019$fuel_grade_n  < eth_meth_stop,"Methanol",
                                 if_else(data_2019$fuel_grade_n >= eth_meth_stop & data_2019$fuel_grade_n < LNG_eth_stop,"Ethanol",
                                         if_else(data_2019$fuel_grade_n >= LNG_eth_stop & data_2019$fuel_grade_n < LPG_LNG_stop,"LNG",
                                                 if_else(data_2019$fuel_grade_n >= LPG_LNG_stop & data_2019$fuel_grade_n < HFO_LPG_stop,"LPG",
                                                         if_else(data_2019$fuel_grade_n >= HFO_LPG_stop & data_2019$fuel_grade_n < LFO_HFO_stop,"HFO",
                                                                 if_else(data_2019$fuel_grade_n >= LFO_HFO_stop & data_2019$fuel_grade_n < MGO_LFO_stop,"LFO",
                                                                         if_else(data_2019$fuel_grade_n >= MGO_LFO_stop & data_2019$fuel_grade_n <= up_stop,"MGO", "NA-others")))))))





data_2020 <- data_2020 %>% mutate(fuel_grade_n = round(total_co2_emissions_m_tonnes/total_fuel_consumption_m_tonnes, digits = 3))
data_2020$fuel_grade <-  if_else(data_2020$fuel_grade_n > down_stop & data_2020$fuel_grade_n  < eth_meth_stop,"Methanol",
                                 if_else(data_2020$fuel_grade_n >= eth_meth_stop & data_2020$fuel_grade_n < LNG_eth_stop,"Ethanol",
                                         if_else(data_2020$fuel_grade_n >= LNG_eth_stop & data_2020$fuel_grade_n < LPG_LNG_stop,"LNG",
                                                 if_else(data_2020$fuel_grade_n >= LPG_LNG_stop & data_2020$fuel_grade_n < HFO_LPG_stop,"LPG",
                                                         if_else(data_2020$fuel_grade_n >= HFO_LPG_stop & data_2020$fuel_grade_n < LFO_HFO_stop,"HFO",
                                                                 if_else(data_2020$fuel_grade_n >= LFO_HFO_stop & data_2020$fuel_grade_n < MGO_LFO_stop,"LFO",
                                                                         if_else(data_2020$fuel_grade_n >= MGO_LFO_stop & data_2020$fuel_grade_n <= up_stop,"MGO", "NA-others")))))))



rm(url2018)
rm(url2019)
rm(url2020)



# group by Ship type
by_ship_type_table_2018 <- data_2018 %>% group_by(ship_type) %>% count()
by_ship_type_table_2019 <- data_2019 %>% group_by(ship_type) %>% count()
by_ship_type_table_2020 <- data_2020 %>% group_by(ship_type) %>% count()

#Number of ships per year
a <- matrix(rnorm(150), ncol = 10)
ship_types <- by_ship_type_table_2018$ship_type
ship_types_df <- as_tibble(a)
colnames(ship_types_df) <- c("Ship_type","year_2018","year_2019","year_2020","Absolut difference between 2019-2018","% Difference between 2019-2018","Absolut difference between 2020-2019", "% Difference between 2020-2019", "Absolut difference between 2020-2018", "% Difference between 2020-2018")
ship_types_df[,1] <- ship_types
ship_types_df[,2] <- by_ship_type_table_2018[,2]
ship_types_df[,3] <- by_ship_type_table_2019[,2]
ship_types_df[,4] <- by_ship_type_table_2020[,2]
ship_types_df[,5] <- ship_types_df[,3]-ship_types_df[,2]
ship_types_df[,6] <- ship_types_df$year_2019/ship_types_df$year_2018*100 - 100
ship_types_df[,7] <- ship_types_df[,4]-ship_types_df[,3]
ship_types_df[,8] <- ship_types_df$year_2020/ship_types_df$year_2019*100 - 100
ship_types_df[,9] <- ship_types_df[,4]-ship_types_df[,2]
ship_types_df[,10] <- ship_types_df$year_2020/ship_types_df$year_2018*100 - 100







###Lets see what happens with the total fuel consumption 
total_fuel_consumption_table_2018 <- data_2018 %>% group_by(ship_type)  %>% summarise(total_fuel_consumption = sum(total_fuel_consumption_m_tonnes))
total_fuel_consumption_table_2019 <- data_2019 %>% group_by(ship_type)  %>% summarise(total_fuel_consumption = sum(total_fuel_consumption_m_tonnes))
total_fuel_consumption_table_2020 <- data_2020 %>% group_by(ship_type)  %>% summarise(total_fuel_consumption = sum(total_fuel_consumption_m_tonnes))

a <- matrix(rnorm(150), ncol = 10)
total_fuel_consumption_df <- as_tibble(a)
colnames(total_fuel_consumption_df) <- c("Ship_type","year_2018","year_2019","year_2020","Absolut difference between 2019-2018","% Difference between 2019-2018","Absolut difference between 2020-2019", "% Difference between 2020-2019", "Absolut difference between 2020-2018", "% Difference between 2020-2018")
total_fuel_consumption_df[,1] <- ship_types
total_fuel_consumption_df[,2] <- total_fuel_consumption_table_2018[,2]
total_fuel_consumption_df[,3] <- total_fuel_consumption_table_2019[,2]
total_fuel_consumption_df[,4] <- total_fuel_consumption_table_2020[,2]
total_fuel_consumption_df[,5] <- total_fuel_consumption_df[,3]-total_fuel_consumption_df[,2]
total_fuel_consumption_df[,6] <- total_fuel_consumption_df$year_2019/total_fuel_consumption_df$year_2018*100 - 100
total_fuel_consumption_df[,7] <- total_fuel_consumption_df[,4]-total_fuel_consumption_df[,3]
total_fuel_consumption_df[,8] <- total_fuel_consumption_df$year_2020/total_fuel_consumption_df$year_2019*100 - 100
total_fuel_consumption_df[,9] <- total_fuel_consumption_df[,4]-total_fuel_consumption_df[,2]
total_fuel_consumption_df[,10] <- total_fuel_consumption_df$year_2020/total_fuel_consumption_df$year_2018*100 - 100





rm(total_fuel_consumption_table_2018)
rm(total_fuel_consumption_table_2019)
rm(total_fuel_consumption_table_2020)


#Fuel grade
fuel_grade_table_2018 <- data_2018 %>%  group_by(ship_type)  %>% count(fuel_grade)  %>% mutate(grade_percenta = n/sum(n)*100) 
fuel_grade_table_2019 <- data_2019 %>%  group_by(ship_type)  %>% count(fuel_grade)  %>% mutate(grade_percenta = n/sum(n)*100) 
fuel_grade_table_2020 <- data_2020 %>%  group_by(ship_type)  %>% count(fuel_grade)  %>% mutate(grade_percenta = n/sum(n)*100) 



ggplot(fuel_grade_table_2018, aes(y=ship_type, x= grade_percenta, colour = fuel_grade, shape = fuel_grade, size = 1)) + geom_point()+
  labs(title = "Percentange of different fuel type consumption", subtitle = "Year 2018, percentange for every segment", x ="100%", y="Type of Ships")  + xlim(0,100)

ggplot(fuel_grade_table_2019, aes(y=ship_type, x= grade_percenta, colour = fuel_grade, shape = fuel_grade, size = 1)) + geom_point()+
  labs(title = "Percentange of different fuel type consumption", subtitle = "Year 2019, percentange for every segment", x ="100%", y="Type of Ships") + xlim(0,100) 

ggplot(fuel_grade_table_2020, aes(y=ship_type, x= grade_percenta, colour = fuel_grade, shape = fuel_grade, size = 1)) + geom_point()+
  labs(title = "Percentange of different fuel type consumption", subtitle = "Year 2020, percentange for every segment", x ="100%", y="Type of Ships") + xlim(0,100)  





MGO2018 <- data_2018 %>% group_by(ship_type) %>% filter(fuel_grade =="MGO") %>%  count() %>% mutate(MGO = n, .keep = "none")
LPG2018 <- data_2018 %>% group_by(ship_type) %>% filter(fuel_grade =="LPG") %>%  count() %>% mutate(LPG = n, .keep = "none")
HFO2018 <- data_2018 %>% group_by(ship_type) %>% filter(fuel_grade =="HFO") %>%  count() %>% mutate(HFO = n, .keep = "none")
LFO2018 <- data_2018 %>% group_by(ship_type) %>% filter(fuel_grade =="LFO") %>%  count() %>% mutate(LFO = n, .keep = "none")
LNG2018 <- data_2018 %>% group_by(ship_type) %>% filter(fuel_grade =="LNG") %>%  count() %>% mutate(LNG = n, .keep = "none")
Methanol2018 <- data_2018 %>% group_by(ship_type) %>% filter(fuel_grade =="Methanol") %>%  count() %>% mutate(Methanol = n, .keep = "none")
Ethanol2018 <- data_2018 %>% group_by(ship_type) %>% filter(fuel_grade =="Ethanol") %>%  count() %>% mutate(Ethanol = n, .keep = "none")

fuel_2018_df <- merge(HFO2018,LFO2018, all=TRUE) %>% merge(MGO2018, all = TRUE) %>%  merge(LPG2018, all = TRUE) %>%
  merge(LNG2018, all = TRUE) %>% merge(Methanol2018, all = TRUE) %>% merge(Ethanol2018, all = TRUE)

rm(MGO2018)
rm(LPG2018)
rm(HFO2018)
rm(LFO2018)
rm(LNG2018)
rm(Methanol2018)
rm(Ethanol2018)

MGO2019 <- data_2019 %>% group_by(ship_type) %>% filter(fuel_grade =="MGO") %>%  count() %>% mutate(MGO = n, .keep = "none")
LPG2019 <- data_2019 %>% group_by(ship_type) %>% filter(fuel_grade =="LPG") %>%  count() %>% mutate(LPG = n, .keep = "none")
HFO2019 <- data_2019 %>% group_by(ship_type) %>% filter(fuel_grade =="HFO") %>%  count() %>% mutate(HFO = n, .keep = "none")
LFO2019 <- data_2019 %>% group_by(ship_type) %>% filter(fuel_grade =="LFO") %>%  count() %>% mutate(LFO = n, .keep = "none")
LNG2019 <- data_2019 %>% group_by(ship_type) %>% filter(fuel_grade =="LNG") %>%  count() %>% mutate(LNG = n, .keep = "none")
Methanol2019 <- data_2019 %>% group_by(ship_type) %>% filter(fuel_grade =="Methanol") %>%  count() %>% mutate(Methanol = n, .keep = "none")
Ethanol2019 <- data_2019 %>% group_by(ship_type) %>% filter(fuel_grade =="Ethanol") %>%  count() %>% mutate(Ethanol = n, .keep = "none")

fuel_2019_df <- merge(HFO2019,LFO2019, all=TRUE) %>% merge(MGO2019, all = TRUE) %>%  merge(LPG2019, all = TRUE) %>%
  merge(LNG2019, all = TRUE) %>% merge(Methanol2019, all = TRUE) %>% merge(Ethanol2019, all = TRUE)

rm(MGO2019)
rm(LPG2019)
rm(HFO2019)
rm(LFO2019)
rm(LNG2019)
rm(Methanol2019)
rm(Ethanol2019)


MGO2020 <- data_2020 %>% group_by(ship_type) %>% filter(fuel_grade =="MGO") %>%  count() %>% mutate(MGO = n, .keep = "none")
LPG2020 <- data_2020 %>% group_by(ship_type) %>% filter(fuel_grade =="LPG") %>%  count() %>% mutate(LPG = n, .keep = "none")
HFO2020 <- data_2020 %>% group_by(ship_type) %>% filter(fuel_grade =="HFO") %>%  count() %>% mutate(HFO = n, .keep = "none")
LFO2020 <- data_2020 %>% group_by(ship_type) %>% filter(fuel_grade =="LFO") %>%  count() %>% mutate(LFO = n, .keep = "none")
LNG2020 <- data_2020 %>% group_by(ship_type) %>% filter(fuel_grade =="LNG") %>%  count() %>% mutate(LNG = n, .keep = "none")
Methanol2020 <- data_2020 %>% group_by(ship_type) %>% filter(fuel_grade =="Methanol") %>%  count() %>% mutate(Methanol = n, .keep = "none")
Ethanol2020 <- data_2020 %>% group_by(ship_type) %>% filter(fuel_grade =="Ethanol") %>%  count() %>% mutate(Ethanol = n, .keep = "none")

fuel_2020_df <- merge(HFO2020,LFO2020, all=TRUE) %>% merge(MGO2020, all = TRUE) %>%  merge(LPG2020, all = TRUE) %>%
  merge(LNG2020, all = TRUE) %>% merge(Methanol2020, all = TRUE) %>% merge(Ethanol2020, all = TRUE)



###CO2 emissions from intraeuropean voyages

total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2018 <- data_2018 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))
total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2019 <- data_2019 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))
total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020 <- data_2020 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))


###Co2 emissions from departure voyages
total_co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2018 <- data_2018 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))
total_co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2019 <- data_2019 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))
total_co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020 <- data_2020 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))


###CO2 emissions from arriving voyages
total_co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2018 <- data_2018 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))
total_co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2019 <- data_2019 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))
total_co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020 <- data_2020 %>% group_by(ship_type)  %>% summarise(co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes = sum(co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))


###CO2 emissions intra + 50% with the outliers
co2_intra_plus_50 <- total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020
colnames(co2_intra_plus_50) <- c("ship_type","co2_intra_plus_50")
co2_intra_plus_50$co2_intra_plus_50 <- total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020$co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes + (total_co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020$co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes + total_co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020$co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes) * 0.5


### The fuel consumptions all_voyages_which_departed_from_ports_under_a_ms_jurisdiction

data_2020$fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes = data_2020$co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes / data_2020$fuel_grade_n
total_fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020 <- data_2020 %>% group_by(ship_type)  %>% summarise(fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes = sum(fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))


## fuel consumption to 

data_2020$fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes = data_2020$co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes / data_2020$fuel_grade_n
total_fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020 <- data_2020 %>% group_by(ship_type)  %>% summarise(fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes = sum(fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))


## intra 
data_2020$fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes = data_2020$co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes / data_2020$fuel_grade_n
total_fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020 <- data_2020 %>% group_by(ship_type)  %>% summarise(fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes = sum(fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes, na.rm = T))


#fuel consumption intra + 50% with the outliers
fuel_intra_plus_50 <- total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020
colnames(fuel_intra_plus_50) <- c("ship_type","fuel_intra_plus_50")
fuel_intra_plus_50$fuel_intra_plus_50 <- total_fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020$fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes + (total_fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020$fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes + total_fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020$fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes) * 0.5


### total factor based on intra + 50%
total_factor_intra_plus_50 <- sum(co2_intra_plus_50$co2_intra_plus_50)/sum(fuel_intra_plus_50$fuel_intra_plus_50)

factor_intra_plus_50 <- total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020
colnames(factor_intra_plus_50) <- c("ship_type","factor_intra_plus_50")
factor_intra_plus_50$factor_intra_plus_50 <- co2_intra_plus_50$co2_intra_plus_50 / fuel_intra_plus_50$fuel_intra_plus_50





rm(MGO2020)
rm(LPG2020)
rm(HFO2020)
rm(LFO2020)
rm(LNG2020)
rm(Methanol2020)
rm(Ethanol2020)
rm(by_ship_type_table_2018)
rm(by_ship_type_table_2019)
rm(by_ship_type_table_2020)
rm(a)
rm(fuel_grade_table_2018)
rm(fuel_grade_table_2019)
rm(fuel_grade_table_2020)

write_xlsx(total_fuel_consumption_df, "total_fuel_consumption.xlsx")
write_xlsx(ship_types_df, "numper_of_different_ship_types.xlsx")
write_xlsx(fuel_2018_df, "fuel_ship_2018.xlsx")
write_xlsx(fuel_2019_df, "fuel_ship_2019.xlsx")
write_xlsx(fuel_2020_df, "fuel_ship_2020.xlsx")
write_xlsx(co2_intra_plus_50, "co2_intra_plus_50.xlsx")
write_xlsx(fuel_intra_plus_50, "fuel_intra_plus_50.xlsx")
write_xlsx(factor_intra_plus_50, "factor_intra_plus_50.xlsx")
write_xlsx(total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020, "total_co2_emissions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020.xlsx")
write_xlsx(total_co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020, "total_co2_emissions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020.xlsx")
write_xlsx(total_co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020, "total_co2_emissions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020.xlsx")
write_xlsx(total_fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020, "total_fuel_consumptions_from_all_voyages_between_ports_under_a_ms_jurisdiction_m_tonnes_2020.xlsx")
write_xlsx(total_fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020, "total_fuel_consumptions_from_all_voyages_to_ports_under_a_ms_jurisdiction_m_tonnes_2020.xlsx")
write_xlsx(total_fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020, "total_fuel_consumptions_from_all_voyages_which_departed_from_ports_under_a_ms_jurisdiction_m_tonnes_2020.xlsx")
