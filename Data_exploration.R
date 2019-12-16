
library(tidyverse)
library(patchwork)
# Advantages of Loan dataset
# 1. big data 80+ GB
# 2. use of models to predict delic
# 3. group by state to create delic index (compare with survey)
# 4. use in predicting home prices, plug into lstm? or just VAR
# 5. 


load("Total.rdata")

data <- Total

# exploration -------------------------------------------------------------

th <- theme_bw() + 
      theme(plot.title              = element_text(size = 10), 
            panel.grid.major   = element_blank(), 
            panel.grid.minor   = element_blank(),
            plot.background    = element_rect(fill = "#f3f3f3", color = "#f3f3f3"),  
            panel.background   = element_rect(fill = "#f3f3f3", color = NA), 
            legend.background  = element_rect(fill = "#f3f3f3"),
            legend.key         = element_rect(fill = "#f3f3f3", color = NA))

an <- plot_annotation(theme = theme(plot.margin     = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                                    plot.background = element_rect(fill = "#f3f3f3", color = "#f3f3f3")))

p_date <- data %>% count(date) %>% ggplot(aes(date, n)) + geom_col() + labs(title="Origination", x = NULL, y = NULL) + th
p_matu <- data %>% count(maturity) %>% ggplot(aes(maturity, n)) + geom_col() + labs(title="Maturity", x = NULL, y = NULL) + th
p_stat <- data %>% count(state) %>% ggplot(aes(reorder(state,n), n)) + geom_col() + labs(title="State", x = NULL, y = NULL) + th

p1 <- ((p_date + p_matu + plot_layout(nrow=1)) / p_stat) + an




p_insu <- data %>% count(insurance_pct) %>% filter(n>20) %>% ggplot(aes(reorder(insurance_pct, as.numeric(insurance_pct)),n)) + geom_col() + labs(title="Insurance Percentage", x = NULL, y = NULL) + th
p_unit <- data %>% count(units) %>% ggplot(aes(units, n)) + geom_col() + labs(title="Number of Units", x = NULL, y = NULL) + th
p_ocus <- data %>% count(ocu_status) %>% ggplot(aes(ocu_status, n)) + geom_col() + labs(title="Ocupancy Status", x = NULL, y = NULL) + th
p_newh <- data %>% filter(new_homeowner != 9) %>% count(new_homeowner) %>% ggplot(aes(new_homeowner, n)) + geom_col() + labs(title="New Homeowner", x = NULL, y = NULL) + th
p_chan <- data %>% count(channel) %>% ggplot(aes(channel, n)) + geom_col()+ labs(title="Channel", x = NULL, y = NULL) + th
p_pmmm <- data %>% count(ppm) %>% ggplot(aes(ppm, n)) + geom_col()+ labs(title="PPM", x = NULL, y = NULL) + th
p_type <- data %>% count(loan_type) %>% ggplot(aes(loan_type, n)) + geom_col()+ labs(title="Loan Type", x = NULL, y = NULL) + th
p_ptyp <- data %>% count(property_type) %>% ggplot(aes(property_type, n)) + geom_col()+ labs(title="Property Type", x = NULL, y = NULL) + th
p_purp <- data %>% count(loan_purpose) %>% ggplot(aes(loan_purpose, n)) + geom_col()+ labs(title="Purpose of Loan", x = NULL, y = NULL) + th
p_term <- data %>% count(term) %>% filter(n>10) %>% ggplot(aes(as.factor(term), n)) + geom_col()+ labs(title="Number of Terms", x = NULL, y = NULL) + th
p_borr <- data %>% filter(n_borrowers != 99) %>% count(n_borrowers) %>% ggplot(aes(n_borrowers, n)) + geom_col()+ labs(title="Number of Borrowers", x = NULL, y = NULL) + th


p2 <- (p_chan + p_type + p_purp  + p_borr + p_ocus + p_newh + plot_layout(nrow=1)) / (p_term + p_insu + p_ptyp + plot_layout(nrow=1)) + an

p_pmmm + p_unit


p_cred <- data %>% filter(credit_score != 9999) %>% ggplot(aes(credit_score)) + geom_histogram(bins=100) + labs(title="Credit Score", x = NULL, y = NULL) + th
p_cltv <- data %>% filter(com_loan_to_value != 999) %>% ggplot(aes(com_loan_to_value)) + geom_histogram(bins=100) + labs(title="Complete Loan to Value", x = NULL, y = NULL) + th
p_nltv <- data %>% filter(loan_to_value != 999) %>% ggplot(aes(loan_to_value)) + geom_histogram(bins=100) + labs(title="Loan to Value", x = NULL, y = NULL) + th
p_dtin <- data %>% filter(debt_to_income != 999) %>% ggplot(aes(debt_to_income)) + geom_histogram(bins=60) + labs(title="Debt to Income", x = NULL, y = NULL) + th
p_upba <- data %>% ggplot(aes(upb)) + geom_histogram(bins=100) + labs(title="Unpaid Balance", x = NULL, y = NULL) + th
p_rate <- data %>% ggplot(aes(rate)) + geom_histogram(bins=40) + labs(title="Interestrate", x = NULL, y = NULL) + th
p3 <- p_cred + p_cltv + p_nltv + p_dtin + p_upba + p_rate + plot_layout(nrow=2) + an



p_sell <- data %>% count(seller) %>% top_n(20) %>% ggplot(aes(reorder(seller,n), n)) + geom_col() + coord_flip() + labs(title = "Seller", x = NULL) + th
p_serv <- data %>% count(servicer) %>% top_n(20) %>% ggplot(aes(reorder(servicer,n), n)) + geom_col() + coord_flip() + labs(title = "Servicer", x = NULL) + th
p4 <- p_sell + p_serv + an






m1 <- data %>% group_by(seller) %>% mutate(nn = n()) %>% 
  group_by(delic_binary, add=TRUE) %>% 
  summarize(per = mean(100*n()/nn)) %>% 
  select(seller, per, delic_binary) %>% 
  filter(delic_binary == TRUE) %>% 
  ggplot(aes(reorder(seller, per), per)) + geom_col() +
  coord_flip() +
  labs(title=NULL, x=NULL, y=NULL) + th + an

m2 <- data %>% filter(credit_score != 9999, credit_score != 300) %>% sample_n(50000) %>% 
  ggplot(aes(credit_score, rate)) + geom_jitter(alpha=0.2) + geom_smooth(method = "lm") +
  labs(title=NULL, x=NULL, y=NULL) + th + an



#data %>% group_by(delic_date, state) %>% summarize(n = sum(delic_binary)) %>% 
#  ggplot(aes(delic_date, n))  + geom_line() + facet_wrap(~state, scales="free")


data %>% group_by(delic_binary) %>% count(seller, sort = T)




#predict -----------------------------------------------------------------

library(caret)



index    <- createDataPartition(data$delic_binary, p = 0.75, list = FALSE)
training <- data[index,] %>% select(-delic_mean, -delic_date, -surv, -surv_binary, -recovered, -first_complete_stop, -loan_type)
test     <- data[-index,] %>% select(-delic_mean, -delic_date, -surv, -surv_binary, -recovered, -first_complete_stop, -loan_type)



cv <- trainControl(method = "cv", number = 5)

fit_glm <- train(as.factor(delic_binary)     ~ .,
                 data      = training,
                 trControl = cv, 
                 method    = "glmnet", 
                 metric    = "Accuracy",
                 na.action=na.exclude)

table(predict(fit_glm, newdata=test), test$delic_binary)



fit_lar <- train(price     ~ .,
                 data      = training,
                 trControl = cv, 
                 method    = '',
                 tuneGrid  = expand.grid(.fraction = seq(0.01, 0.99, length = 50)),
                 metric    = "RMSE")
t2 <- Sys.time()


# Network analsis ---------------------------------------------------------

library(tidygraph)
library(ggraph)

set.seed(1)
p_net1 <- data %>% group_by(seller, servicer) %>% 
  filter(n() > 1500) %>% 
  ungroup() %>% 
  distinct(seller, servicer) %>% 
  as_tbl_graph() %>% 
  mutate(Centrality = centrality_degree(mode = 'in')) %>% 
  ggraph(layout = "nicely") + 
  geom_edge_fan(color="grey") + 
  geom_node_point(aes(size = Centrality), color="#08306b") + 
  geom_node_text(aes(label=ifelse(Centrality >= 1, name, NA)), size=2, nudge_y = 0.35) + 
  scale_size_continuous(breaks=c(1,3,6,9)) + 
  labs(title="Centrality by servicer", x=NULL, y=NULL) +
  th + theme(legend.position = "bottom")

set.seed(1)
p_net2 <- data %>% group_by(seller, servicer) %>% 
  filter(n() > 1500) %>% 
  ungroup() %>% 
  distinct(seller, servicer) %>% 
  as_tbl_graph() %>% 
  mutate(Centrality = centrality_degree(mode = 'out')) %>% 
  ggraph(layout = "nicely") + 
  geom_edge_fan(color="grey") + 
  geom_node_point(aes(size = Centrality), color="#08306b") + 
  geom_node_text(aes(label=ifelse(Centrality >= 1, name, NA)), size=2, nudge_y = 0.35) + 
  scale_size_continuous(breaks=c(1,3,6,9)) + 
  labs(title="Centrality by seller", x=NULL, y=NULL) +
  th + theme(legend.position = "bottom")

n1 <- p_net1 + p_net2 + an

set.seed(1)





n2 <- data %>% 
  group_by(seller, servicer) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  filter(seller != "Other sellers", 
         servicer != "Other servicers", 
         n > 300) %>% 
  as_tbl_graph() %>% 
  ggraph(layout = "nicely") + 
  geom_edge_fan(aes(width=n), alpha=0.3) + 
  geom_node_point(color="#08306b", size=5) + 
  geom_node_text(aes(label=name), size=2, nudge_y = 0.7) + 
  labs(title="Edge size by number of connections", x=NULL, y=NULL) +
  th + theme(legend.position = "bottom") + an





















ggsave(p1, filename = "plots/p1.pdf", width=12, height=5, dpi=300)
ggsave(p2, filename = "plots/p2.pdf", width=12, height=5, dpi=300)
ggsave(p3, filename = "plots/p3.pdf", width=12, height=5, dpi=300)
ggsave(p4, filename = "plots/p4.pdf", width=12, height=5, dpi=300)

ggsave(m1, filename = "plots/m1.pdf", width=12, height=5, dpi=300)
ggsave(m2, filename = "plots/m2.pdf", width=12, height=3, dpi=300)

ggsave(n1, filename = "plots/n1.pdf", width=12, height=7, dpi=300)
ggsave(n2, filename = "plots/n2.pdf", width=12, height=5, dpi=300)

















