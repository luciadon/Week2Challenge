library(tidyverse)

#bringing data in#
BOM_data <- read_csv('data/BOM_data.csv')
BOM_stations <- read_csv('data/BOM_stations.csv')

#Q1: For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?#

#start by seperating the min & max into own columns#
Bom_seperate <- separate(BOM_data, Temp_min_max, into = c('Temp_min', 'Temp_max'), sep='/')
#filter out anything where relevant measurement not recorded#
filtered <- filter(Bom_seperate, Rainfall != '-', Temp_min !='-', Temp_max != '-')
#group data & summarise data to answer the question#
grouped <- group_by(filtered,Station_number)
Question_1 <- summarise(grouped, n())
Question_1

#Q2: Which month saw the lowest average daily temperature difference?#

#need new filtered data as above filtered out too much#
filterered2<- filter(Bom_seperate, Temp_min !='-', Temp_max != '-')
#converting min & max temps to numeric and calculating the difference#
bom_clean <- mutate(filterered2, Temp_minnum = (as.numeric(filterered2$Temp_min)), Temp_maxnum = (as.numeric(filterered2$Temp_max))) %>% 
  mutate(Temp_diff = Temp_maxnum-Temp_minnum)
#group data & summarise data to answer the question#
Question_2almost <- bom_clean %>% group_by(Month) %>% summarise(avg_daily = mean(Temp_diff))%>% arrange(avg_daily)
#pull out the answer#
Question2 <- Question_2almost %>% head(1)     
# June saw the lowest average daily temperature difference#

#Q3: Which state saw the lowest average daily temperature difference?#
#making the bom_stations data tidy#
tidy_bom <- gather(BOM_stations, key = Station_num , value = value, -info) %>% 
spread(key=info, value = value)
#making station_number numeric to enable a join#
num_bom <- mutate(tidy_bom, Station_number = (as.numeric(tidy_bom$Station_num)))
#joining the two data sets to know which state the stations are in#
joined <- left_join (bom_clean, num_bom, by = c('Station_number'='Station_number'))
#group & summarise, arrange data to answer the question, and pull out the top#
Question_3 <- joined %>% group_by(state) %>% summarise(avg_daily= mean(Temp_diff)) %>% arrange(avg_daily) %>% head(1)
Question_3
#QLD saw the lowest average daily temperature difference#

#Q4: Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?#

#arranging my data different ways to find westmost and eastmost points#
joined %>% arrange(desc(lon)) %>% arrange(lon)
#westmost = 9194, eastmost = 40043
#cleaning out data where solar exposure is 
filtered3 <- joined %>%filter(Solar_exposure != '-')``
  mutate(filtered3, SE =(as.numeric(filtered3$Solar_exposure))) %>% 
    group_by(Station_number) %>% summarise(avgSE=mean(SE))
#eastmost- with solar exposure at 19.5 avg
  
  
# I did question 4 WRONG- started with an already filtered dataset. NEW QUESTION 4:
  
#start by filering my data
filtered4 <- filter(Bom_seperate,Solar_exposure != '-')
#then join this to the tidy data above#
joined2 <- left_join (bom_clean, Bom_seperate, by = c('Station_number'='Station_number'))
  

  joined %>% arrange(desc(lon)) %>% arrange(lon)
  
  #westmost = 9194, eastmost = 40043
  
  #cleaning out data where solar exposure is 
  filtered3 <- joined %>%filter(Solar_exposure != '-')
  
  mutate(filtered3, SE =(as.numeric(filtered3$Solar_exposure))) %>% 
    group_by(Station_number) %>% summarise(avgSE=mean(SE))
  
  #eastmost- with solar exposure at 19.5 avg
