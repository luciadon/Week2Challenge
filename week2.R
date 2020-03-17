library(tidyverse)

#bringing data in#
BOM_data <- read_csv('data/BOM_data.csv')
BOM_stations <- read_csv('data/BOM_stations.csv')
BOM_data
BOM_stations
view(BOM_stations)
view(BOM_data)

#answering question 1#
Bom_seperate <- separate(BOM_data, Temp_min_max, into = c('Temp_min', 'Temp_max'), sep='/')
view(Bom_seperate)
filtered <- filter(Bom_seperate, Rainfall != '-', Temp_min !='-', Temp_max != '-')
grouped <- group_by(filtered,Station_number)
Question_1 <- summarise(grouped, n())
Question_1

#answering question 2#
filterered2<- filter(Bom_seperate, Temp_min !='-', Temp_max != '-')
filterered2
?as.numeric
bom_clean <- mutate(filterered2, Temp_minnum = (as.numeric(filterered2$Temp_min)), Temp_maxnum = (as.numeric(filterered2$Temp_max))) %>% 
  mutate(Temp_diff = Temp_maxnum-Temp_minnum)
Question_2 <- bom_clean %>% group_by(Month) %>% summarise(avg_daily = mean(Temp_diff))%>% arrange(avg_daily)
Question_2 %>% head(1)                                                
# June saw the lowest average daily temperature difference#

#answering question 3#
tidy_bom <- gather(BOM_stations, key = Station_num , value = value, -info) %>% 
spread(key=info, value = value)
num_bom <- mutate(tidy_bom, Station_number = (as.numeric(tidy_bom$Station_num)))
num_bom
Bom_seperate
joined <- left_join (bom_clean, num_bom, by = c('Station_number'='Station_number'))
joined
Question_3 <- joined %>% group_by(state) %>% summarise(avg_daily= mean(Temp_diff)) %>% arrange(avg_daily) %>% head(1)
Question_3

#answering question 4#