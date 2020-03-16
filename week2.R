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
as.numeric(filterered2, c('Temp_min'))
mutate(Bom_seperate, Temp_diff = Temp_max-Temp_min)
