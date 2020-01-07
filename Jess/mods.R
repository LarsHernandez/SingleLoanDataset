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


load("../Total2.rdata")
data <- subset(Total2, !(state %in% c("VI", "GU", "PR","DC")))

# Data --------------------------------------------------------------------

data_model <- subset(data, !is.na(data$delic_binary)) %>% 
  select(delic_binary, credit_score, new_homeowner, state, channel, loan_purpose, debt_to_income, property_type, upb, seller, rate, units, ocu_status, loan_to_value, n_borrowers, White, median_income2018) %>% 
#  mutate(date = as.Date(date)) %>% 
#mutate(delic_binary = ifelse(delic_binary==TRUE,"yes","no")) %>% 
  mutate(delic_binary = as.factor(delic_binary)) %>%
  filter(!is.na(median_income2018)) %>% 
  filter(!is.na(White)) %>% 
  filter(!is.na(channel)) %>% 
  filter(!is.na(loan_purpose)) %>% 
  filter(!is.na(property_type)) %>% 
  filter(!is.na(seller)) %>% 
  filter(!is.na(ocu_status)) %>% 
  filter(!is.na(new_homeowner)) %>% 
  filter(!is.na(state)) %>% 
  filter(!is.na(n_borrowers)) 

index    <- createDataPartition(data_model$delic_binary, p = 0.75, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

# Recipe ------------------------------------------------------------------

cv <- trainControl(method = "cv", number = 5) # , classProbs = TRUE, summaryFunction = twoClassSummary

model_recipe_steps <- recipe(delic_binary ~ ., data = training) %>%
  step_string2factor(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>% 
  step_dummy(new_homeowner, state, channel, loan_purpose, property_type, seller, ocu_status, n_borrowers) %>% 
  step_range(credit_score, debt_to_income, upb, rate, units, loan_to_value, median_income2018, White, min = 0, max = 1)

prepped_recipe <- prep(model_recipe_steps, training = training)
training       <- bake(prepped_recipe, training) 
test           <- bake(prepped_recipe, test) 

#predict -----------------------------------------------------------------

t0 <- Sys.time()
fit_tre <- train(delic_binary ~ .,
                 data      = training,
                 trControl = cv, 
                 method    = "rpart", 
                 metric    = "Kappa")
t1 <- Sys.time()
fit_glm <- train(delic_binary ~ .,
               data      = training,
               trControl = cv, 
               method    = "glm",
               family    = "binomial",
                 metric    = "Kappa")
t2 <- Sys.time()
fit_bag <- train(delic_binary ~ .,
                 data      = training,
                 trControl = cv,
                 nbagg     = 5,
                 method    = "treebag",
                 metric    = "Kappa")
t3 <- Sys.time()
fit_raf <- train(delic_binary ~ .,
                 data      = training,
                 trControl = cv,
                 ntree     = 30, 
                 method    = "rf",
                 metric    = "Kappa")
t4 <- Sys.time()
#fit_lda <- train(delic_binary ~ .,
#                 data      = training,
#                 trControl = cv, 
#                 method    = "lda",
#                 metric    = "Kappa")
t5 <- Sys.time()
fit_net <- train(delic_binary ~ .,
                 data      = training,
                 trControl = cv, 
                 method    = "glmnet",
                 metric    = "Kappa")
t6 <- Sys.time()
#fit_knn <- train(delic_binary     ~ .,
#                 data      = training,
#                 trControl = cv, 
#                 method    = "kknn",
#                 metric    = "Kappa")
t7 <- Sys.time()
fit_xgb <- train(delic_binary     ~ .,
                 data      = training,
                 trControl = cv, 
                 method    = "xgbTree",
                 metric    = "Kappa")
t8 <- Sys.time()
#fit_svm <- train(delic_binary     ~ .,
              #   data      = training,
              #   trControl = cv, 
              #   method    = "svmLinear",
              #   metric    = "Kappa")
t9 <- Sys.time()


cat(paste0("Size of training set: ", nrow(training)," of 21613\n","\n",
           "Logistic:             ", round(difftime(t1,t0, units = "mins"),2),"\n",
           "Tree:                 ", round(difftime(t2,t1, units = "mins"),2),"\n",
           "Bagged:               ", round(difftime(t3,t2, units = "mins"),2),"\n",
           "Random Forrest:       ", round(difftime(t4,t3, units = "mins"),2),"\n",
           #"LDA:                  ", round(difftime(t5,t4, units = "mins"),2),"\n",
           "Elastic Net:          ", round(difftime(t6,t5, units = "mins"),2),"\n",
           #"KNNt:                 ", round(difftime(t7,t6, units = "mins"),2),"\n",
           "XGB:                  ", round(difftime(t8,t7, units = "mins"),2),"\n",
           "Total:                ", round(difftime(t9,t0, units = "mins"),2),"\n"))




sspec <- function(x,b) {rbind(spec(x), precision(x), accuracy(x), recall(x), npv(x)) %>% mutate(model = b)}





rd   <- rbinom(n = length(test$delic_binary), size = 1, prob = mean(as.logical(training$delic_binary), na.rm=T))
vec <- as.factor(rep(FALSE,length(test$delic_binary)))
levels(vec) <- c(FALSE, TRUE)

#nav <- sspec(table(vec, test$delic_binary),                               "Naive Model")
#ran <- sspec(table(as.factor(if_else(rd == 1, T, F)), test$delic_binary), "Assignment by prop")
#glm <- sspec(table(predict(fit_glm,  newdata=test), test$delic_binary),   "Logistic Regression")
#tre <- sspec(table(predict(fit_tre,  newdata=test), test$delic_binary),   "Classification Tree")
#bag <- sspec(table(predict(fit_bag,  newdata=test), test$delic_binary),   "Bagged Tree")
#raf <- sspec(table(predict(fit_raf,  newdata=test), test$delic_binary),   "Random Forrest")
#lda <- sspec(table(predict(fit_lda,  newdata=test), test$delic_binary),   "Linear Diskriminant Analysis")
net <- sspec(table(predict(fit_net,  newdata=test), test$delic_binary),   "Elastic Net")
xgb <- sspec(table(predict(fit_xgb,  newdata=test), test$delic_binary),   "Extreme Gradient Boosting")
#knn <- sspec(table(predict(fit_knn,  newdata=test), test$delic_binary),   "K nearest neightbour")
#svm <- sspec(table(predict(fit_svm,  newdata=test), test$delic_binary),   "Support Vector Classifier")

#ela$model <- "Elastic Net Regression"
#dfm <- rbind(net, xgb)
df <- rbind(net, xgb, nav, ran, glm, tre, bag, raf)

save(df,file="ml.final")
#save(dfm,file="df_two")

conf <- rbind(table(predict(fit_glm,  newdata=test), test$delic_binary),
                table(predict(fit_tre,  newdata=test), test$delic_binary),
                table(predict(fit_net,  newdata=test), test$delic_binary),
                table(predict(fit_raf,  newdata=test), test$delic_binary),
                table(predict(fit_xgb,  newdata=test), test$delic_binary),
              table(predict(fit_bag,  newdata=test), test$delic_binary))

save(conf,file="conf_final")
#save(confm,file="conf_two")

ggplot(df, aes(.metric, .estimate, fill = reorder(model, desc(.estimate)))) + 
  geom_col(position="dodge", width = 0.6) + 
  scale_fill_tableau(palette = "Classic Cyclic", type = "regular") + 
  labs(title=paste("Model performance on predicting delinquency from a sample of",nrow(training)), 
       subtitle="Crossvalidated 1/5 split - Normal sampeling", 
       fill="Predictive Models\nordered by performance")


df %>% filter(.metric == "accuracy") %>% arrange(desc(.estimate))

load("conf_final")
