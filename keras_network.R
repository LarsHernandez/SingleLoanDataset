


library(tidyverse)
library(recipes)
library(keras)
library(caret)
#load("Total.rdata")

total <- load("Total2.rdata")
data <- Total2 %>% filter(!(state %in% c("VI", "GU", "PR")),
                         !(is.na(White) | is.na(median_income2018)))


# Data --------------------------------------------------------------------
data_model <- subset(data, !is.na(data$delic_binary)) %>%
  select(delic_binary, credit_score, new_homeowner, state, channel, loan_purpose, debt_to_income, 
         property_type, upb, seller, rate, units, ocu_status, loan_to_value, n_borrowers, White, median_income2018) %>% 
  sample_n(450000)

set.seed(1)
index    <- createDataPartition(data_model$delic_binary, p = 0.75, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

# Recipe ------------------------------------------------------------------

model_recipe_steps <-
  recipe(delic_binary ~ ., data = training) %>%
  step_string2factor(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>%
  step_dummy(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>%
  step_range(credit_score, debt_to_income, upb, rate, units, loan_to_value, White, median_income2018, min = 0, max = 1)

prepped_recipe <- prep(model_recipe_steps, training = training)
training       <- bake(prepped_recipe, training)
test           <- bake(prepped_recipe, test)

test[is.na(test)] <- 0

x_train  <- training %>% select(-delic_binary) %>% as.matrix()
x_test   <- test     %>% select(-delic_binary) %>% as.matrix()
y_train  <- training %>% select(delic_binary) %>% as.matrix()
y_test   <- test     %>% select(delic_binary) %>% as.matrix()

model <- keras_model_sequential() %>%
  layer_dense(units = 128, input_shape = dim(x_train)[2]) %>%
  layer_dropout(0.5) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.3) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 1,   activation = "sigmoid")

model %>% compile(optimizer = 'adam',
                  loss      = 'binary_crossentropy',
                  metrics   = 'accuracy')

model %>% keras::fit(x_train, y_train,
                     batch_size = 128,
                     epochs = 10,
                     validation_split = 0.1)


pred <- predict(model, x_test)

kcm <- table(pred>0.5, y_test)

confusionMatrix(table(pred>0.5, y_test))




