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
library(keras)
library(tensorflow)
library(recipes)

load("Total2.rdata")
data <- subset(Total2, !(state %in% c("VI", "GU", "PR")))


#fit <- glm(delic_binary ~ delic_binary+credit_score+new_homeowner+state+channel+loan_purpose+debt_to_income, data = total, family = "binomial")



# Data --------------------------------------------------------------------
cv <- trainControl(method = "cv", number = 5)

data_model <- subset(data, !is.na(data$delic_binary)) %>% 
  select(delic_binary, credit_score, new_homeowner, state, channel, loan_purpose, income, debt_to_income, property_type, upb, rate, seller, servicer, com_loan_to_value) %>% 
  sample_n(140000)

#temp <- mice(data_model, m = 1, maxit=10, meth='pmm', seed = 500)
#
#imp_data_model <- complete(temp, 1)
#imp_data_model<-imp_data_model %>% na.omit()
#
#sum(is.na(imp_data_model))

index    <- createDataPartition(data_model$delic_binary, p = 0.75, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

model_recipe <- recipe(delic_binary ~ ., data = training)
model_recipe_steps <- model_recipe %>% 
  step_string2factor(new_homeowner, state, channel, loan_purpose, property_type, seller, servicer) %>% 
  step_dummy(new_homeowner, state, channel, loan_purpose, property_type, seller, servicer) %>% 
  step_range(credit_score, income, debt_to_income, upb, rate, com_loan_to_value, min = 0, max = 1)
prepped_recipe <- prep(model_recipe_steps, training) 
training <- bake(prepped_recipe, training) 
test     <- bake(prepped_recipe, test)
test[is.na(test)] <- 0

x_train  <- training %>% select(-delic_binary) %>% as.matrix()
x_test   <- test     %>% select(-delic_binary) %>% as.matrix()
y_train  <- training %>% select(delic_binary) %>% as.matrix()
y_test   <- test     %>% select(delic_binary) %>% as.matrix()

dimnames(x_train) <- NULL 
dimnames(y_train) <- NULL 
dimnames(x_test) <- NULL 
dimnames(y_test) <- NULL 

# Mice imputation ---------------------------------------------------------

epochs = 30
batch = 100

model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu", input_shape = dim(x_train)[2]) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(1, activation="sigmoid")


model %>% compile(optimizer = 'adam',
                  loss      = 'binary_crossentropy',
                  metrics   = 'accuracy') 

history <- model %>% keras::fit(
  x_train,
  y_train,
  epochs = epochs,
  batch_size = batch,
  validation_split = 0.2
)

pred <- predict(model, x_test)

table(pred>0.5, y_test)
table(y_test)
confusionMatrix(table(pred>0.5, y_test)
)
