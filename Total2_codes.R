# https://www.ffiec.gov/Medianincome.htm (MSA) 

# https://freddiemac.embs.com/FLoan/Data/download3.php (FMac)

# https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-metro-and-micro-statistical-areas.html#par_textimage (POP) 

# https://www.census.gov/library/publications/2012/dec/c2010sr-01.html (Race) 

library(magrittr)
library(readxl)
library(caret)
library(tidyverse)
library(keras)
library(tensorflow) 

# Advantages of Loan dataset
# 1. big data 80+ GB
# 2. use of models to predict delic
# 3. group by state to create delic index (compare with survey)
# 4. use in predicting home prices, plug into lstm? or just VAR
# 5. 

load("C:/Users/Jess-/Desktop/Data_M4/Total.rdata")


Inc_MSA <- read_delim("https://raw.githubusercontent.com/holle94/M4/master/MedianIncome_MSA.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE, 
                      skip = 1) %>% select(c(1,3,4))%>% rename( area = `MSA/MD FIPS CODE NO.` ,
                                                              median_income2018 = `2018 FFIEC EST. MSA/MD MEDIAN FAMILY INCOME**`,
                                                              median_income2015 = `2015 MSA/MD MED* FAMILY INCOME`) 
  
  
  

Race_MSA <- read_delim("https://raw.githubusercontent.com/holle94/M4/master/Race_MSA.csv", 
                       ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                   grouping_mark = "."), trim_ws = TRUE, 
                       skip = 4) %>% select(-c(12)) %>% 
  na.omit()

Race_MSA$area %<>% as.numeric("area")

state_pop <- read_delim("https://raw.githubusercontent.com/holle94/M4/master/state_pop.csv", 
                           ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = "."), trim_ws = TRUE, 
                           skip = 3) %>% 
  na.omit(state) %>% 
  select(c(1,10)) %>% 
  rename(pop2018 = `2018`) 

extra <- cbind(state = c("PR","GU","VI"), pop2018 = c(3195153,162742,106405))

state_pop <- rbind(state_pop, extra)

Age_MSA <- read_delim("C:/Users/Jess-/Desktop/Data_M4/Age_MSA.csv", 
                      ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                  grouping_mark = "."), trim_ws = TRUE) 


Total2 <- Total %>% 
  left_join(Inc_MSA, by = "area")   %>% 
  left_join(Race_MSA, by = "area")  %>% 
  left_join(Age_MSA, by ="area")    %>% 
  left_join(state_pop, by ="state") 
 

Total2 %>% count(area) %>% arrange(desc(n))
Total2 %>% count(median_income2018) %>% arrange(desc(n))
Total2 %>% count(White) %>% arrange(desc(n))

# ML ----------------------------------------------------------------------

#ctrl <- trainControl(method = "cv", 
#                     number = 10)
#
#index    <- createDataPartition(data_f$delic_binary, p = 0.75, list = FALSE)
#training <- data_f[index,] 
#test     <- data_f[-index,] 
#
#
#fit_ela <- train(price     ~ .,
#                 data      = training,
#                 trControl = cv, 
#                 tuneGrid  = expand.grid(alpha  = seq(0, 1,    by = 0.1),
#                                         lambda = seq(1, 1000, by = 100)),
#                 method    = "glmnet", 
#                 family    = "gaussian")
#
#model <- keras_model_sequential() %>% 
#  layer_dense(input_shape = dim(training)[2], units = 128, activation = "relu") %>% 
#  layer_dense(units = 64, activation = "relu") %>% 
#  layer_dropout(0.3) %>% 
#  layer_dense(units = 64, activation = "relu") %>% 
#  layer_dense(1, activation = "sigmoid")#