
library(tidyverse)

# Advantages of Loan dataset
# 1. big data 80+ GB
# 2. use of models to predict delic
# 3. group by state to create delic index (compare with survey)
# 4. use in predicting home prices, plug into lstm? or just VAR
# 5. 

total <- NULL

for (i in 1999:2017) {


# Data --------------------------------------------------------------------

orig_2006 <- read_delim(paste0("historical_data1_Q4", i, ".txt"), 
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

svcg_2006 <- read_delim(paste0("historical_data1_time_Q4", i, ".txt"), 
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
  mutate(status   = ifelse(delic == 1,1,0)) %>% 
  summarize(delic_date = first(na.omit(temp))) %>% 
  right_join(svcg_2006, by="id") %>% 
  group_by(id) %>% 
  mutate(prev     = lag(delic, order_by=id),
         survived = ifelse(prev != 0 & delic == 0, 1, 0),
         counter  = row_number(),
         status   = ifelse(delic_date == temp, 1, NA)) %>% 
  group_by(status, add = T) %>% 
  mutate(survival = max(counter)) %>% 
  ungroup() %>% 
  group_by(id) %>% 
  summarize(delic_mean     = mean(delic, na.rm=T),
            delic_binary   = mean(delic, na.rm=T) > 0,
            delic_binary0  = mean(delic == 1, na.rm=T) > 0,
            delic_binary3  = mean(delic == 3, na.rm=T) > 0,
            delic_binary6  = mean(delic == 6, na.rm=T) > 0,
            delic_binary12 = mean(delic == 12, na.rm=T) > 0,
            delic_date     = first(na.omit(temp)),
            surv_binary    = mean(survived, na.rm=T) > 0,
            surv           = sum(survived, na.rm=T),
            survival       = min(survival),
            max_unpaid     = max(unpaid, na.rm=T),
            recovered      = last(delic) == 0) %>% 
  mutate(first_complete_stop = ifelse(delic_binary == TRUE & surv_binary == FALSE, TRUE, FALSE),
         recovered           = ifelse(delic_binary == TRUE & recovered == TRUE, TRUE, FALSE))


data <- left_join(orig_2006, extra, by = "id")
data$dataset <- i
data$datasetq <- "q4"

total <- bind_rows(total, data)

}

total$income <- total$upb / total$term * 100 / ifelse(total$debt_to_income == 999, NA, total$debt_to_income)

table(total$delic_binary, total$dataset)

save(total, file="total_full_q4.rdata")



