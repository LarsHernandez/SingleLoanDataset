

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



load("Total.rdata")
data <- subset(total, !(state %in% c("VI", "GU", "PR")))


fit <- glm(delic_binary ~ delic_binary+credit_score+new_homeowner+state+channel+loan_purpose+debt_to_income, data = total, family = "binomial")



# Data --------------------------------------------------------------------

data_model <- subset(data, !is.na(data$delic_binary)) %>% 
  select(delic_binary, credit_score, new_homeowner, state, channel, loan_purpose, income, debt_to_income, property_type, upb) %>% 
  mutate(delic_binary = as.factor(delic_binary)) %>% 
  sample_n(30000)


# Mice imputation ---------------------------------------------------------

temp <- mice(data_model, m = 1, maxit=50, meth='pmm', seed = 500)

imp_data_model <- complete(temp, 1)
 

sum(is.na(imp_data_model))

#predict -----------------------------------------------------------------


index    <- createDataPartition(imp_data_model$delic_binary, p = 0.75, list = FALSE)
training <- imp_data_model[index,]
test     <- imp_data_model[-index,]

cv <- trainControl(method = "cv", number = 5)



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
                 method    = "treebag",
                 nbagg     = 30,
                 metric    = "Kappa",
                 trControl = cv)
t3 <- Sys.time()
fit_raf <- train(delic_binary ~ .,
                 data      = training, 
                 method    = "rf",
                 .mtry     = 5,
                 ntree     = 30,
                 metric    = "Kappa",
                 trControl = cv)
t4 <- Sys.time()
fit_lda <- train(delic_binary ~ .,
                 data      = training, 
                 method    = "lda",
                 metric    = "Kappa",
                 trControl = cv)
t5 <- Sys.time()
t6 <- Sys.time()
t7 <- Sys.time()
t8 <- Sys.time()
t9 <- Sys.time()


cat(paste0("Size of training set: ", nrow(training)," of 21613\n","\n",
           "LM:             ", round(difftime(t1,t0, units = "mins"),2),"\n",
           "LARS:           ", round(difftime(t2,t1, units = "mins"),2),"\n",
           "Elastic:        ", round(difftime(t3,t2, units = "mins"),2),"\n",
           "Tree:           ", round(difftime(t4,t3, units = "mins"),2),"\n",
           "Random Forrest: ", round(difftime(t5,t4, units = "mins"),2),"\n",
           "Neural Net:     ", round(difftime(t6,t5, units = "mins"),2),"\n",
           "XGB:            ", round(difftime(t7,t6, units = "mins"),2),"\n",
           "XGB:            ", round(difftime(t8,t7, units = "mins"),2),"\n",
           "XGB:            ", round(difftime(t9,t8, units = "mins"),2),"\n",
           "XGB:            ", round(difftime(t9,t0, units = "mins"),2),"\n"))




sspec <- function(x) {rbind(spec(x), precision(x), accuracy(x), recall(x), npv(x))}


glm <- sspec(table(predict(fit_glm,  newdata=test), test$delic_binary))
tre <- sspec(table(predict(fit_tre,  newdata=test), test$delic_binary))
bag <- sspec(table(predict(fit_bag,  newdata=test), test$delic_binary))
raf <- sspec(table(predict(fit_raf,  newdata=test), test$delic_binary))
lda <- sspec(table(predict(fit_lda,  newdata=test), test$delic_binary))
#ela <- sspec(conf_ela)
#ran <- sspec(conf_ran)
#svm <- sspec(conf_svm)

random   <- rbinom(n = length(test$delic_binary), size = 1, prob = mean(as.logical(training$delic_binary), na.rm=T))
ran <- sspec(table(as.factor(if_else(random == 1, TRUE, FALSE)), test$delic_binary))

vec <- as.factor(rep(FALSE,length(test$delic_binary)))
levels(vec) <- c(FALSE, TRUE)
nav <- sspec(table(vec, test$delic_binary))





ran$model <- "Assignment by prop"
nav$model <- "Naive Model"

glm$model <- "Logistic Regression"
tre$model <- "Classification Tree"
bag$model <- "Bagged Tree"
raf$model <- "Random Forrest"
lda$model <- "Linear Diskriminant Analysis"


#ela$model <- "Elastic Net Regression"



df <- rbind(ran, glm, tre, nav, bag, raf)

ggplot(df, aes(.metric, .estimate, fill = reorder(model, desc(.estimate)))) + 
  geom_col(position="dodge", width = 0.6) + 
  scale_fill_tableau(palette = "Classic Cyclic", type = "regular") + 
  labs(title=paste("Model performance on predicting delinquency from a sample of",nrow(training)), 
       subtitle="Crossvalidated 1/5 split - Normal sampeling", 
       fill="Predictive Models\nordered by performance")




