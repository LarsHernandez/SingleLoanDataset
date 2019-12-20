
library(tidyverse)
library(patchwork)
library(openintro) #get state from abbr
library(scales)
library(tidygraph)
library(ggraph)

library(caret)

# Advantages of Loan dataset
# 1. big data 80+ GB
# 2. use of models to predict delic
# 3. group by state to create delic index (compare with survey)
# 4. use in predicting home prices, plug into lstm? or just VAR
# 5. 


load("Total.rdata")

data <- subset(total, !(state %in% c("VI", "GU", "PR")))


# Theme -------------------------------------------------------------------

base <- "#08306b"

th <- theme(plot.title  = element_text(size = 10),
            plot.background   = element_rect(fill = "#f3f3f3",       color = "#f3f3f3"),
            panel.background  = element_rect(fill = "#f3f3f3",       color = NA), 
            legend.background = element_rect(fill = "#f3f3f3",       color = NA),
            legend.key        = element_rect(fill = "#f3f3f3",       color = NA),
            strip.background  = element_rect(fill = "#f3f3f3",       color = NA),
            panel.border      = element_rect(fill = NA,       color = "black", size = 0.3),
            panel.grid.major  = element_blank(), 
            panel.grid.minor  = element_blank(),
            title             = element_text(color = "black"),
            plot.subtitle     = element_text(color = "grey40"),
            plot.caption      = element_text(color = "grey70"),
            strip.text        = element_text(face  = "bold"),
            axis.text         = element_text(color = "black"),
            axis.ticks        = element_line(color = "black"))

thd <- theme(plot.title  = element_text(size = 10),
             plot.background   = element_rect(fill = "#f3f3f3",       color = "#f3f3f3"),
             panel.background  = element_rect(fill = "#f3f3f3",       color = NA), 
             legend.background = element_rect(fill = "#f3f3f3",       color = NA),
             legend.key        = element_rect(fill = "#f3f3f3",       color = NA),
             strip.background  = element_rect(fill = "#f3f3f3",       color = NA),
             panel.border      = element_rect(fill = NA,       color = "black", size = 0.3),
             panel.grid.major  = element_line(color="grey90"), 
             panel.grid.minor  = element_line(color="grey90"), 
             title             = element_text(color = "black"),
             plot.subtitle     = element_text(color = "grey40"),
             plot.caption      = element_text(color = "grey70"),
             strip.text        = element_text(face  = "bold"),
             axis.text         = element_text(color = "black"),
             axis.ticks        = element_line(color = "black"))


an <- plot_annotation(theme = theme(plot.margin     = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                                    plot.background = element_rect(fill = "#f3f3f3", color = "#f3f3f3")))



# Table -------------------------------------------------------------------

total %>% 
  as.data.frame() %>% 
  stargazer(digits = 1, decimal.mark = ",", digit.separator = ".")



# exploration -------------------------------------------------------------

p_date <- data %>% count(date) %>% ggplot(aes(date, n)) + geom_col(color=base, fill=base) + labs(title="Origination", x = NULL, y = NULL) + th
p_matu <- data %>% count(maturity) %>% ggplot(aes(maturity, n)) + geom_col(color=base, fill=base) + labs(title="Maturity", x = NULL, y = NULL) + th
p_stat <- data %>% count(state) %>% ggplot(aes(reorder(state,n), n)) + geom_col(fill=base) + labs(title="State", x = NULL, y = NULL) + th

p1 <- ((p_date + p_matu + plot_layout(nrow=1)) / p_stat) + an




p_insu <- data %>% count(insurance_pct) %>% filter(n>50) %>% ggplot(aes(reorder(insurance_pct, as.numeric(insurance_pct)),n)) + geom_col(fill=base) + labs(title="Insurance Percentage", x = NULL, y = NULL) + th
p_unit <- data %>% count(units) %>% ggplot(aes(units, n)) + geom_col(fill=base) + labs(title="Number of Units", x = NULL, y = NULL) + th
p_ocus <- data %>% count(ocu_status) %>% ggplot(aes(ocu_status, n)) + geom_col(fill=base) + labs(title="Ocupancy Status", x = NULL, y = NULL) + th
p_newh <- data %>% filter(new_homeowner != 9) %>% count(new_homeowner) %>% ggplot(aes(new_homeowner, n)) + geom_col(fill=base) + labs(title="New Homeowner", x = NULL, y = NULL) + th
p_chan <- data %>% count(channel) %>% ggplot(aes(channel, n)) + geom_col(fill=base)+ labs(title="Channel", x = NULL, y = NULL) + th
p_pmmm <- data %>% count(ppm) %>% ggplot(aes(ppm, n)) + geom_col(fill=base)+ labs(title="PPM", x = NULL, y = NULL) + th
p_type <- data %>% count(loan_type) %>% ggplot(aes(loan_type, n)) + geom_col(fill=base)+ labs(title="Loan Type", x = NULL, y = NULL) + th
p_ptyp <- data %>% count(property_type) %>% ggplot(aes(property_type, n)) + geom_col(fill=base)+ labs(title="Property Type", x = NULL, y = NULL) + th
p_purp <- data %>% count(loan_purpose) %>% ggplot(aes(loan_purpose, n)) + geom_col(fill=base)+ labs(title="Purpose of Loan", x = NULL, y = NULL) + th
p_term <- data %>% count(term) %>% filter(n>200) %>% ggplot(aes(as.factor(term), n)) + geom_col(fill=base)+ labs(title="Number of Terms", x = NULL, y = NULL) + th
p_borr <- data %>% filter(n_borrowers != 99) %>% count(n_borrowers) %>% ggplot(aes(n_borrowers, n)) + geom_col(fill=base)+ labs(title="Number of Borrowers", x = NULL, y = NULL) + th


p2 <- (p_chan + p_type + p_purp  + p_borr + p_ocus + p_newh + plot_layout(nrow=1)) / (p_term + p_insu + p_ptyp + plot_layout(nrow=1)) + an


p_cred <- data %>% filter(credit_score != 9999) %>% ggplot(aes(credit_score)) + geom_histogram(bins=100, fill=base) + labs(title="Credit Score", x = NULL, y = NULL) + th
p_cltv <- data %>% filter(com_loan_to_value != 999) %>% ggplot(aes(com_loan_to_value)) + geom_histogram(bins=100, fill=base) + labs(title="Complete Loan to Value", x = NULL, y = NULL) + th
p_nltv <- data %>% filter(loan_to_value != 999) %>% ggplot(aes(loan_to_value)) + geom_histogram(bins=100, fill=base) + labs(title="Loan to Value", x = NULL, y = NULL) + th
p_dtin <- data %>% filter(debt_to_income != 999) %>% ggplot(aes(debt_to_income)) + geom_histogram(bins=60, fill=base) + labs(title="Debt to Income", x = NULL, y = NULL) + th
p_upba <- data %>% ggplot(aes(upb)) + geom_histogram(bins=100, fill=base) + labs(title="Unpaid Balance", x = NULL, y = NULL) + th
p_rate <- data %>% ggplot(aes(rate)) + geom_histogram(bins=40, fill=base) + labs(title="Interestrate", x = NULL, y = NULL) + th
p3 <- p_cred + p_cltv + p_nltv + p_dtin + p_upba + p_rate + plot_layout(nrow=2) + an



p_sell <- data %>% count(seller) %>% top_n(20) %>% ggplot(aes(reorder(seller,n), n)) + geom_col(fill=base) + coord_flip() + labs(title = "Seller", x = NULL) + th
p_serv <- data %>% count(servicer) %>% top_n(20) %>% ggplot(aes(reorder(servicer,n), n)) + geom_col(fill=base) + coord_flip() + labs(title = "Servicer", x = NULL) + th
p4 <- p_sell + p_serv + an




total %>% ggplot(aes(income/1000)) + geom_histogram()
total %>% ggplot(aes(max_unpaid/1000)) + geom_histogram()





# wrangling ---------------------------------------------------------------





m1 <- data %>% group_by(seller) %>% filter(n() > 5000) %>% mutate(nn = n()) %>% 
  group_by(delic_binary, add=TRUE) %>% 
  summarize(per = mean(100*n()/nn)) %>% 
  select(seller, per, delic_binary) %>% 
  filter(delic_binary == TRUE) %>% 
  ggplot(aes(reorder(seller, per), per)) + geom_col(fill=basem1) +
  coord_flip() +
  labs(title=NULL, x=NULL, y=NULL) + th + an

m2 <- data %>% filter(credit_score != 9999, credit_score != 300, !is.na(delic_binary)) %>% sample_n(60000) %>% 
  ggplot(aes(credit_score, rate, color=delic_binary)) + geom_jitter(width = 0.2, height = 0.2, size=2) + geom_smooth(method = "lm", color="black") +
  scale_color_manual(values = c(basem1, basem2)) +
  labs(title=NULL, x=NULL, y=NULL) + th + an





m3 <- data %>% group_by(delic_date, state) %>% summarize(n = sum(delic_binary)) %>% 
  ggplot(aes(delic_date, n)) + 
  geom_smooth(span = 0.25, color=base) +
  scale_x_date(date_breaks = "4 year", labels = date_format("%Y")) +
  facet_wrap(~state, scales="free", nrow=9) + th + an


m4 <- data %>% group_by(delic_date, state) %>% 
  summarize(n = sum(delic_binary)) %>% 
  filter(state %in% c("CA", "TX", "FL", "NY", "PA", "IL")) %>% 
  ggplot(aes(delic_date, n)) + 
  scale_color_brewer(palette="Paired") +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  geom_smooth(aes(color=state), span = 0.25, fill="grey90") + thd + an

m5 <- data %>% group_by(delic_date, new_homeowner) %>% 
  summarize(n = sum(delic_binary)) %>% 
  ggplot(aes(delic_date, n)) + 
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  geom_smooth(span = 0.25, color=base) + 
  facet_wrap(~new_homeowner, scales="free", nrow=3) +
  thd + an

m6 <- data %>% group_by(delic_date, new_homeowner) %>% 
  summarize(n = sum(delic_mean)) %>% 
  ggplot(aes(delic_date, n)) + 
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  geom_smooth(span = 0.25, color=base) + 
  thd + an





p_sca1 <- data %>% group_by(delic_date, state) %>% 
  summarize(n = sum(delic_binary)) %>% 
  filter(state %in% c("CA", "TX", "FL", "NY", "PA", "IL")) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(n = scale(n)) %>% 
  ggplot(aes(delic_date, n, color=state)) + 
  scale_color_tableau(palette = "Miller Stone", type = "regular") + 
  scale_x_date(date_breaks = "4 year", labels = date_format("%Y")) +
  labs(title="scaled delicenquency binary", x=NULL,y=NULL) +
  ylim(-2,4) +
  geom_smooth(span = 0.25, fill="grey80", se=F) + thd 

p_sca2 <- data %>% group_by(delic_date, state) %>% 
  summarize(n = sum(delic_mean)) %>% 
  filter(state %in% c("CA", "TX", "FL", "NY", "PA", "IL")) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(n = scale(n)) %>% 
  ggplot(aes(delic_date, n, color=state)) + 
  scale_color_tableau(palette = "Miller Stone", type = "regular") + 
  scale_x_date(date_breaks = "4 year", labels = date_format("%Y")) +
  labs(title="scaled delicenquency sum", x=NULL,y=NULL) +
  ylim(-2,4) +
  geom_smooth(span = 0.25, fill="grey90", se=F) + thd 

p_sca3 <- data %>% group_by(delic_date, state) %>% 
  summarize(n = sum(delic_binary)) %>% 
  filter(state %in% c("CA", "TX", "FL", "NY", "PA", "IL")) %>% 
  ggplot(aes(delic_date, n, color=state)) + 
  scale_color_tableau(palette = "Miller Stone", type = "regular") + 
  scale_x_date(date_breaks = "6 year", labels = date_format("%Y"), date_minor_breaks = "2 year") +
  geom_point(alpha=0.3, show.legend = F) +
  geom_smooth(aes(color=state),show.legend = F, span = 0.25) + thd + 
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  facet_wrap(~state, nrow=1) + 
  scale_y_continuous(limits=c(0,150)) +
  labs(x=NULL, y=NULL, title = "Delinquency binary by state")

m7 <- (p_sca3 | (p_sca1 / p_sca2 + plot_layout(guides = "collect"))) + an + plot_layout(widths = c(2, 1))
m7










# Experiment --------------------------------------------------------------

basem1 <- "#4f6980"
basem2 <- "#849db1"


p_date <- data %>% count(date,delic_binary) %>% ggplot(aes(date, n/1000)) + geom_col(aes(color=delic_binary, fill=delic_binary), show.legend = F) + 
  labs(title="Origination", x = NULL, y = NULL) + th + scale_y_continuous(limits=c(0,6.5)) +
  scale_color_manual(values = c(basem1, basem2)) + scale_fill_manual(values = c(basem1, basem2))

p_matu <- data %>% count(maturity,delic_binary) %>% ggplot(aes(maturity, n/1000)) + geom_col(aes(color=delic_binary, fill=delic_binary), show.legend = F) + 
  labs(title="Maturity", x = NULL, y = NULL) + th + scale_y_continuous(limits=c(0,6.5)) +
  scale_color_manual(values = c(basem1, basem2)) + scale_fill_manual(values = c(basem1, basem2))

p_stat <- data %>% count(state,delic_binary) %>% filter(!is.na(delic_binary)) %>% ggplot(aes(reorder(state,n), n/1000)) + geom_col(aes(fill=delic_binary)) + 
  labs(title="State", x = NULL, y = NULL, fill="Delinquent") + th + 
  scale_color_manual(values = c(basem1, basem2)) + scale_fill_manual(values = c(basem1, basem2))

a1 <- p_date + p_matu + p_stat + plot_layout(guides="collect", design = "12\n33") + an 
a1







p_insu <- data %>% count(insurance_pct,delic_binary) %>% filter(n>50) %>% ggplot(aes(reorder(insurance_pct, as.numeric(insurance_pct)),n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F) + labs(title="Insurance Percentage", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_unit <- data %>% count(units,delic_binary) %>% ggplot(aes(units, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F) + labs(title="Number of Units", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_ocus <- data %>% count(ocu_status,delic_binary) %>% ggplot(aes(ocu_status, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F) + labs(title="Ocupancy Status", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_newh <- data %>% filter(new_homeowner != 9) %>% count(new_homeowner,delic_binary) %>% ggplot(aes(new_homeowner, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F) + labs(title="New Homeowner", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_chan <- data %>% count(channel,delic_binary) %>% ggplot(aes(channel, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F)+ labs(title="Channel", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_pmmm <- data %>% count(ppm,delic_binary) %>% ggplot(aes(ppm, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F)+ labs(title="PPM", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_type <- data %>% count(loan_type,delic_binary) %>% ggplot(aes(loan_type, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F)+ labs(title="Loan Type", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_ptyp <- data %>% count(property_type,delic_binary) %>% ggplot(aes(property_type, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F)+ labs(title="Property Type", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_purp <- data %>% count(loan_purpose,delic_binary) %>% ggplot(aes(loan_purpose, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F)+ labs(title="Purpose of Loan", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_term <- data %>% count(term,delic_binary) %>% filter(n>200) %>% ggplot(aes(as.factor(term), n/1000)) + geom_col(aes(fill=delic_binary))+ labs(title="Number of Terms", x = NULL, y = NULL, fill="Delinquent") + th+ scale_fill_manual(values = c(basem1, basem2))
p_borr <- data %>% filter(n_borrowers != 99) %>% count(n_borrowers,delic_binary) %>% ggplot(aes(n_borrowers, n/1000)) + geom_col(aes(fill=delic_binary), show.legend = F) + labs(title="Number of Borrowers", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))

a2 <- p_chan + p_type + p_purp  + p_borr + p_ocus + p_newh + p_term + p_insu + p_ptyp + plot_layout(guides="collect", design = "123456\n778899") + an
a2




p_cred <- data %>% filter(credit_score != 9999,!is.na(delic_binary)) %>% ggplot(aes(credit_score)) + geom_histogram(bins=100, aes(fill=delic_binary)) + labs(fill = "Delinquent", title="Credit Score", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_cltv <- data %>% filter(com_loan_to_value != 999,!is.na(delic_binary)) %>% ggplot(aes(com_loan_to_value)) + geom_histogram(bins=100, aes(fill=delic_binary)) + labs(fill = "Delinquent", title="Complete Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_nltv <- data %>% filter(loan_to_value != 999,!is.na(delic_binary)) %>% ggplot(aes(loan_to_value)) + geom_histogram(bins=100, aes(fill=delic_binary)) + labs(fill = "Delinquent", title="Loan to Value", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_dtin <- data %>% filter(debt_to_income != 999,!is.na(delic_binary)) %>% ggplot(aes(debt_to_income)) + geom_histogram(bins=60, aes(fill=delic_binary)) + labs(fill = "Delinquent", title="Debt to Income", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_upba <- data %>% filter(!is.na(delic_binary)) %>% ggplot(aes(upb)) + geom_histogram(bins=100, aes(fill=delic_binary)) + labs(fill = "Delinquent", title="Unpaid Balance", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))
p_rate <- data %>% filter(!is.na(delic_binary)) %>% ggplot(aes(rate)) + geom_histogram(bins=40, aes(fill=delic_binary)) + labs(fill = "Delinquent", title="Interestrate", x = NULL, y = NULL) + th+ scale_fill_manual(values = c(basem1, basem2))

a3 <- p_cred + p_cltv + p_nltv + p_dtin + p_upba + p_rate + plot_layout(nrow=2, guides = "collect") + an
a3

topsell <- data %>% count(seller) %>% top_n(20)
topserv <- data %>% count(servicer) %>% top_n(20)

p_sell <- data %>% filter(!is.na(delic_binary))%>% count(seller, delic_binary) %>% filter(seller %in% topsell$seller) %>% ggplot(aes(reorder(seller,n), n)) + 
  geom_col(aes(fill=delic_binary)) + coord_flip() + labs(title = "Seller", x = NULL,fill = "Delinquent") + th + scale_fill_manual(values = c(basem1, basem2))

p_serv <- data %>% filter(!is.na(delic_binary)) %>% count(servicer, delic_binary) %>% filter(servicer %in% topserv$servicer) %>% ggplot(aes(reorder(servicer,n), n)) + 
  geom_col(aes(fill=delic_binary)) + coord_flip() + labs(title = "Servicer", x = NULL,fill = "Delinquent") + th + scale_fill_manual(values = c(basem1, basem2))

a4 <- p_sell + p_serv + an + plot_layout(guides = "collect")
a4









#p_stat <- data %>% count(state,delic_binary) %>% ggplot(aes(reorder(state,n), n)) + geom_col(aes(fill=delic_binary), position = "fill") + labs(title="State", x = NULL, y = NULL) + th + scale_color_brewer(palette="Paired") + scale_fill_brewer(palette="Paired")






# Network analsis ---------------------------------------------------------

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





p_n2 <- data %>% 
  group_by(seller, servicer) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  filter(seller != "Other sellers", 
         servicer != "Other servicers", 
         n > 500) %>% 
  as_tbl_graph() %>% 
  ggraph(layout = "nicely") + 
  geom_edge_fan(aes(width=n), alpha=0.3) + 
  geom_node_point(color=base, size=5) + 
  geom_node_text(aes(label=name), size=2, nudge_y = 0.7) + 
  labs(title="Edge size by number of connections", x=NULL, y=NULL) +
  th



p_n3 <- data %>% group_by(seller) %>% mutate(n = n()) %>% 
  filter(n>7000, !is.na(income)) %>% 
  ungroup() %>% 
  mutate(seller = as.factor(seller)) %>% 
  ggplot(aes(x = reorder(seller, income, FUN = median), y = income)) + 
  geom_boxplot(outlier.shape = NA) + coord_flip() + 
  labs(title="Income by seller", x=NULL, y=NULL) + 
  ylim(0,5000) +
  th

n2 <- p_n2 + p_n3 + an + plot_layout(widths = c(2.8, 1))

# map ---------------------------------------------------------------------




states_map <- map_data("state")
dat <- data %>% group_by(state) %>% summarize(n=n()) %>% 
  mutate(region = tolower(abbr2state(state)))

ggplot(dat, aes(map_id = region)) +
  geom_map(aes(fill = n), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) + th +
  labs(title="US" , x=NULL, y=NULL, fill="Loans")





# Save --------------------------------------------------------------------


ggsave(p1, filename = "plots/p1.pdf", width=12, height=5, dpi=300)
ggsave(p2, filename = "plots/p2.pdf", width=12, height=5, dpi=300)
ggsave(p3, filename = "plots/p3.pdf", width=12, height=5, dpi=300)
ggsave(p4, filename = "plots/p4.pdf", width=12, height=5, dpi=300)

ggsave(m1, filename = "plots/m1.pdf", width=12, height=5, dpi=300)
ggsave(m2, filename = "plots/m2.pdf", width=12, height=3, dpi=300)
ggsave(m3, filename = "plots/m3.pdf", width=20, height=12, dpi=300)
ggsave(m4, filename = "plots/m4.pdf", width=12, height=5, dpi=300)
ggsave(m7, filename = "plots/m7.pdf", width=14, height=6, dpi=300)

ggsave(n1, filename = "plots/n1.pdf", width=12, height=7, dpi=300)
ggsave(n2, filename = "plots/n2.pdf", width=12, height=6, dpi=300)

ggsave(a1, filename = "plots/a1.pdf", width=12, height=5, dpi=300)
ggsave(a2, filename = "plots/a2.pdf", width=12, height=5, dpi=300)
ggsave(a3, filename = "plots/a3.pdf", width=12, height=5, dpi=300)
ggsave(a4, filename = "plots/a4.pdf", width=12, height=5, dpi=300)













