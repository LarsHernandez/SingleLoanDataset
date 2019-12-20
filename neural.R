

library(tidyverse)
library(patchwork)
library(openintro) #get state from abbr
library(scales)
library(tidygraph)
library(ggraph)
library(caret)
library(yardstick)
library(ggthemes)
library(mice)
library(recipes)
library(keras)

load("total.rdata")
data <- subset(total, !(state %in% c("VI", "GU", "PR")))


# Data --------------------------------------------------------------------
data_model <- subset(data, !is.na(data$delic_binary)) %>%
  select(delic_binary, credit_score, new_homeowner, state, channel, loan_purpose, debt_to_income, property_type, upb, seller, rate, units, ocu_status, loan_to_value, n_borrowers) %>%
  sample_n(400000)

# Mice imputation ---------------------------------------------------------
#temp <- mice(data_model, m = 1, maxit=50, meth='pmm', seed = 500)
#imp_data_model <- complete(temp, 1)
#sum(is.na(imp_data_model))
index    <- createDataPartition(data_model$delic_binary, p = 0.75, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

# Recipe ------------------------------------------------------------------

model_recipe_steps <-
  recipe(delic_binary ~ ., data = training) %>%
  step_string2factor(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>%
  step_dummy(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>%
  step_range(credit_score, debt_to_income, upb, rate, units, loan_to_value, min = 0, max = 1)

prepped_recipe <- prep(model_recipe_steps, training = training)
training       <- bake(prepped_recipe, training)
test           <- bake(prepped_recipe, test)

test[is.na(test)] <- 0

x_train  <- training %>% select(-delic_binary) %>% as.matrix()
x_test   <- test     %>% select(-delic_binary) %>% as.matrix()
y_train  <- training %>% select(delic_binary) %>% as.matrix()
y_test   <- test     %>% select(delic_binary) %>% as.matrix()

model <- keras_model_sequential() %>%
  layer_dense(input_shape = dim(x_train)[2], units = 128) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.3) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(optimizer = 'adamax',
                  loss      = 'binary_crossentropy',
                  metrics   = 'accuracy')

hist<-model %>% keras::fit(x_train, y_train,
                           batch_size = 256,
                           epochs = 35,
                           validation_split = 0.1)



confusionMatrix(table(predict(model, x_test) > 0.5, y_test))







install.packages("cloudml")
library(cloudml)
gcloud_install()

#gcloud_init()




cloudml_train("train.R")









































































