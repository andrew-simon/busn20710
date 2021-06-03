library(tidyverse)
library()
data <- read_csv('BE Final Project_June 2, 2021_14.52.csv') %>%
  pivot_longer(c('A1' , 'B1' , 'C1' , 'A2' , 'B2' , 'C2') , names_to ='treatment' 
               , values_to = 'response')
  
data <- filter(data , is.na(data$response) == F) %>%
  # prop_req = 2 if 100% , = 1 if 90% , = 0 if 50%
  mutate(prop_req = case_when(
    treatment %in% c('A1' , 'A2') ~ 2 ,
    treatment %in% c('B1' , 'B2') ~ 1 ,
    treatment %in% c('C1' , 'C2') ~ 0)) %>%
  # two = 1 if the group was 100,000
  mutate(two = ifelse(treatment %in% c('A1' , 'B1' , 'C1') , 0 , 1))

model <- lm(response ~ two + prop_req + two*prop_req , data = data)

summary(model)

