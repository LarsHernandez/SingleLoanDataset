
library(tidyverse)

# Advantages of Loan dataset
# 1. big data 80+ GB
# 2. use of models to predict delic
# 3. group by state to create delic index (compare with survey)
# 4. use in predicting home prices, plug into lstm? or just VAR
# 5. 

total <- NULL

for (i in 1999:2018) {

setwd("//student.aau.dk/Users/lbni13/Desktop")
# Data --------------------------------------------------------------------

orig_2006 <- read_delim(paste0("freddiemac/sample_orig_", i, ".txt"), 
                        "|", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE, col_types = cols(X26 = col_skip()))

orig_2006 %>% rename(id                = X20,
                     credit_score      = X1,
                     date              = X2,
                     maturity          = X4,
                     new_homeowner     = X3,
                     area              = X5,
                     insurance_pct     = X6,
                     units             = X7,
                     ocu_status        = X8,
                     com_loan_to_value = X9,
                     debt_to_income    = X10,
                     upb               = X11,
                     loan_to_value     = X12,
                     rate              = X13,
                     channel           = X14,
                     ppm               = X15,
                     loan_type         = X16,
                     state             = X17,
                     property_type     = X18,
                     postal_code       = X19,
                     loan_purpose      = X21,
                     term              = X22,
                     n_borrowers       = X23,
                     seller            = X24,
                     servicer          = X25) -> orig_2006

svcg_2006 <- read_delim(paste0("freddiemac/sample_svcg_", i, ".txt"), 
                        "|", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE, col_types = cols_only(X1 = col_guess(),
                                                              X2 = col_guess(),
                                                              X3 = col_guess(),
                                                              X4 = col_integer()))


svcg_2006 %>% rename(id                = X1,
                     date_info         = X2,
                     unpaid            = X3,
                     delic             = X4) -> svcg_2006


orig_2006 <- orig_2006 %>% select(id, date, everything()) %>% 
  mutate(date     = as.Date(paste0(as.character(date),"01"), format = "%Y%m%d"),
         maturity = as.Date(paste0(as.character(maturity),"01"), format = "%Y%m%d"))

svcg_2006 <- svcg_2006 %>% 
  mutate(date_info = as.Date(paste0(as.character(date_info),"01"), format = "%Y%m%d"))


# Combination -------------------------------------------------------------

svcg_2006$temp <- if_else(svcg_2006$delic == 1, svcg_2006$date_info, as.Date(NA))

extra <- svcg_2006 %>% 
  group_by(id) %>% 
  mutate(prev = lag(delic, order_by=id),
         survived = ifelse(prev != 0 & delic == 0, 1, 0)) %>%
  summarize(delic_mean   = mean(delic, na.rm=T),
            delic_binary = mean(delic, na.rm=T) > 0,
            delic_date   = first(na.omit(temp)),
            surv_binary  = mean(survived, na.rm=T) > 0,
            surv         = sum(survived, na.rm=T),
            max_unpaid   = max(unpaid, na.rm=T),
            recovered    = last(delic) == 0) %>% 
  mutate(first_complete_stop = ifelse(delic_binary == TRUE & surv_binary == FALSE, TRUE, FALSE),
         recovered           = ifelse(delic_binary == TRUE & recovered == TRUE, TRUE, FALSE))


data <- left_join(orig_2006, extra, by = "id")
data$dataset <- i

total <- bind_rows(total, data)

}




table(total$delic_binary, total$dataset)











# exploration -------------------------------------------------------------

data %>% count(state, sort = T)


total %>% count(state, sort = T)






total %>% group_by(delic_date, state) %>% summarize(n = n()) %>% ggplot(aes(delic_date, n)) + geom_point() + scale_y_continuous(limits=c(0,1000)) + geom_smooth(span=0.2) + facet_wrap(~state, scales = "free") + ylim(0,200)




table(total$delic_binary, total$dataset, useNA = c("always"))





dat %>% group_by(X24) %>% summarize(delic_mean = mean(mpay, na.rm=T),
                                    delic_binary = mean(mpay, na.rm=T) > 0,
                                    delic_date) %>% 
  arrange(desc(delic))

dat %>% count(X24, sort = T)


















