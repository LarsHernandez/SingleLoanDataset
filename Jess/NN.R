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


load("../Total.rdata")
data <- subset(total, !(state %in% c("VI", "GU", "PR")))

# Data --------------------------------------------------------------------

data_model <- subset(data, !is.na(data$delic_binary)) %>% 
  select(delic_binary, credit_score, new_homeowner) %>% 
  sample_n(100000)

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
  step_string2factor(new_homeowner) %>% 
  step_dummy(new_homeowner) %>% 
  step_range(credit_score,  min = 0, max = 1)
prepped_recipe <- prep(model_recipe_steps, training) 
training <- bake(prepped_recipe, training) 
test     <- bake(prepped_recipe, test)
test[is.na(test)] <- 0

x_train  <- training %>% dplyr::select(-delic_binary) %>% as.matrix()
x_test   <- test     %>% dplyr::select(-delic_binary) %>% as.matrix()
y_train  <- training %>% dplyr::select(delic_binary) %>% as.matrix()
y_test   <- test     %>% dplyr::select(delic_binary) %>% as.matrix()

#dimnames(x_train) <- NULL 
#dimnames(y_train) <- NULL 
#dimnames(x_test) <- NULL 
#dimnames(y_test) <- NULL 

# Mice imputation ---------------------------------------------------------

epochs = 20
batch = 1000

model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = dim(x_train)[2]) %>%
  layer_dropout(0.2) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(1, activation="sigmoid")


model %>% compile(optimizer = 'RMSprop',
                  loss      = 'binary_crossentropy',
                  metrics   = 'accuracy') 

model %>% keras::fit(
  x_train,
  y_train,
  epochs = epochs,
  batch_size = batch,
  validation_split = 0.1
)

pred <- predict(model, x_test)

table(pred>0.5, y_test)
table(y_test)
confusionMatrix(table(pred>0.5, y_test)
)

pred_prop <- c(NA)
pred_prop <- ifelse(pred>0.5,TRUE,FALSE)

#library(PRROC)
#
#df<-cbind(y_test,pred_prop)
#
#plot(roc.curve(y_test,pred_prop, curve = TRUE))
