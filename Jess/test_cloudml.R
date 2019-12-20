library(recipes)
library(keras)
library(tidyverse)
library(cloudml)
library(caret)

load("../Total.rdata")
data <- subset(total, !(state %in% c("VI", "GU", "PR")))

# Data --------------------------------------------------------------------

data_model <- subset(data, !is.na(data$delic_binary)) %>% 
  select(delic_binary, credit_score, new_homeowner, state, channel, loan_purpose, debt_to_income, property_type, upb, seller, rate, units, ocu_status, loan_to_value, n_borrowers) %>% 

index    <- createDataPartition(data_model$delic_binary, p = 0.75, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

# Recipe ------------------------------------------------------------------
epochs =20
batch = 1000

model_recipe_steps <- recipe(delic_binary ~ ., data = training) %>% 
  step_string2factor(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>% 
  step_dummy(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>% 
  step_range(credit_score, debt_to_income, upb, rate, units, loan_to_value, min = 0, max = 1)

prepped_recipe <- prep(model_recipe_steps, training = training)
training       <- bake(prepped_recipe, training) 
test           <- bake(prepped_recipe, test)

x_train  <- training %>% select(-delic_binary) %>% as.matrix()
x_test   <- test     %>% select(-delic_binary) %>% as.matrix()
y_train  <- training %>% select(delic_binary) %>% as.matrix()
y_test   <- test     %>% select(delic_binary) %>% as.matrix()

model <- keras_model_sequential() %>% 
  layer_dense(input_shape = dim(x_train)[2], units = 512) %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 256, activation = "tanh", 
              kernel_regularizer = regularizer_l2(l = 0.001)) %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 128, activation = "tanh") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 64, activation = "tanh") %>%
  layer_dropout(0.2) %>% 
  layer_dense(units= 1, activation = "sigmoid")

model %>% compile(optimizer = 'adamax',
                  loss      = 'binary_crossentropy',
                  metrics   = 'accuracy') 

model %>% keras::fit(x_train, y_train,
                     batch_size = batch,
                     epochs = epochs,
                     validation_split = 0.1)

model %>% evaluate(x_test, y_test, verbose = 0)

pred <- predict(model, x_test)

table(pred>0.5, y_test)
confusionMatrix(table(pred>0.5, y_test)
)

pred_prop <- c(NA)
pred_prop <- ifelse(pred>0.5,TRUE,FALSE)

