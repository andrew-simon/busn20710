library(tidyverse)

data <- read_csv('BE Final Project_June 2, 2021_14.52.csv') %>%
  pivot_longer(c('A1' , 'B1' , 'C1' , 'A2' , 'B2' , 'C2') , names_to ='treatment' 
               , values_to = 'response')
# response = 1 if sell, =0 if hold
  
data <- filter(data , is.na(data$response) == F) %>%
  # prop_req = 2 if 100% , = 1 if 90% , = 0 if 50%
  mutate(prop_req = case_when(
    treatment %in% c('A1' , 'A2') ~ 2 ,
    treatment %in% c('B1' , 'B2') ~ 1 ,
    treatment %in% c('C1' , 'C2') ~ 0)) %>%
  # groupsize = 1 if the group was 100,000
  mutate(groupsize = ifelse(treatment %in% c('A1' , 'B1' , 'C1') , 0 , 1))

model <- lm(response ~ groupsize + prop_req + groupsize*prop_req , data = data)

summary(model)

data <- mutate(data , genderdum = ifelse(gender == 'Male' , 1 , 0))

demomodel <- lm(response ~ age + genderdum + hedgefunds + stocks + Q31 + Q33 , data = data)
summary(demomodel)
