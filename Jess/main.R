


# https://www.ffiec.gov/Medianincome.htm (MSA) 

# https://freddiemac.embs.com/FLoan/Data/download3.php (FMac)

# https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-metro-and-micro-statistical-areas.html#par_textimage (POP) 

# https://www.census.gov/library/publications/2012/dec/c2010sr-01.html (Race) 

library(magrittr)
library(readxl)
library(caret)
library(tidyverse)
library(keras)
library(tensorflow) 
library(patchwork)
library(VIM)
library(mice)

load("../Total.rdata")
#write.csv(total, file = "total.csv")

#sub <- total %>% select(c(1:25))
#aggr(sub, cex.axis =0.8)

Inc_MSA <- read_delim("MedianIncome_MSA.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE, 
                      skip = 1) %>% select(c(1,3,4))%>% rename( area = `MSA/MD FIPS CODE NO.` ,
                                                                median_income2018 = `2018 FFIEC EST. MSA/MD MEDIAN FAMILY INCOME**`,
                                                                median_income2015 = `2015 MSA/MD MED* FAMILY INCOME`) 


ARP_MSA <- read_delim("ARP_MSA.csv", 
                      ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                  grouping_mark = "."), trim_ws = TRUE) %>% 
  select(-c(20:22))

ARP_MSA$area %<>% as.numeric("area")

state_pop <- read_delim("state_pop.csv", 
                        ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                    grouping_mark = "."), trim_ws = TRUE, 
                        skip = 3) %>% 
  na.omit(state) %>% 
  select(c(1,10)) %>% 
  rename(Spop2018 = "2018") %>% 
  mutate(Spop2018 = as.numeric(Spop2018))

extra <- tibble(state = c("PR","GU","VI"), Spop2018 = c(3195153,162742,106405))  

state_pop <- rbind(state_pop, extra)

Total2 <- total %>% 
  left_join(Inc_MSA, by = "area")   %>% 
  left_join(ARP_MSA, by = "area") %>% 
  left_join(state_pop, by ="state") 

Total2 %>% select() %>% arrange(Spop2018)

Total2 %>% count(area) %>% arrange(desc(n))
Total2 %>% count(median_income2018) %>% arrange(desc(n))
Total2 %>% count(White) %>% arrange(desc(n))

#save(Total2, file = "Total2.rdata")


# Plots -------------------------------------------------------------------

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

area_p<-Total2 %>% group_by(area) %>% filter(!is.na(area)) %>%   count() %>%  arrange(desc(n)) %>% head(20) %>% 
  ggplot(aes(reorder(as.factor(area),n),n)) + geom_col() +
  coord_flip() +
  labs(title=NULL, x=NULL, y=NULL) + th + an

ggsave(area_p,filename = "area_p.pdf", width=12, height=5, dpi=300)

Total2 %>%  filter(!is.na(Non_Hispanic)) %>% ggplot(aes(Non_Hispanic)) +  geom_histogram(bins=50) + labs(title="% white ethnicity", x = NULL, y = NULL) + th + geom_vline(xintercept = 83.65, linetype ="dashed", color ="black") 

Total2 %>% group_by(area) %>% filter(!is.na(area)) %>% arrange(White)

Total2 %>% filter(delic_binary == 1) %>% filter(!is.na(Hispanic)) %>%
  summarise(mean(Hispanic))
Total2 %>% filter(delic_binary == 0) %>% filter(!is.na(Hispanic)) %>%
  summarise(mean(Hispanic))
Total2 %>% filter(delic_binary == 1) %>% filter(!is.na(Black)) %>%
  summarise(mean(Black))
Total2 %>% filter(delic_binary == 0) %>% filter(!is.na(Black)) %>%
  summarise(mean(Black))
Total2 %>% filter(delic_binary == 1) %>% filter(!is.na(median_income2015)) %>%
  summarise(mean(median_income2015))
Total2 %>% filter(delic_binary == 0) %>% filter(!is.na(median_income2015)) %>%
  summarise(mean(median_income2015))
Total2 %>% filter(delic_binary == 1) %>% filter(!is.na(median_income2018)) %>%
  summarise(mean(median_income2018))
Total2 %>% filter(delic_binary == 0) %>% filter(!is.na(median_income2018)) %>%
  summarise(mean(median_income2018))



His_p<-Total2 %>% filter(!is.na(Hispanic), !is.na(delic_binary)) %>% ggplot(aes(Hispanic)) + 
  geom_histogram(aes(y=..density..),bins=50) + facet_wrap(~delic_binary, nrow=2)  +
  geom_vline(data = subset(Total2, delic_binary), aes(xintercept = mean(Hispanic, na.rm=T)), linetype ="dashed", color ="black") + 
  geom_vline(data = subset(Total2, !delic_binary), aes(xintercept = mean(Hispanic, na.rm=T)), linetype ="dashed", color ="black") + th +
  labs(title = "Density plot of % latino population in MSA by delic status ")

Bl_p <- Total2 %>% filter(!is.na(Black), !is.na(delic_binary)) %>% ggplot(aes(Black)) + 
  geom_histogram(aes(y=..density..),bins=50) + facet_wrap(~delic_binary, nrow=2)  +
  geom_vline(data = subset(Total2, delic_binary), aes(xintercept = mean(Black, na.rm=T)), linetype ="dashed", color ="black") + 
  geom_vline(data = subset(Total2, !delic_binary), aes(xintercept = mean(Black, na.rm=T)), linetype ="dashed", color ="black") + th +
  labs(title = "Density plot of % black population in MSA by delic status ")

Inc_p <- Total2 %>% filter(!is.na(median_income2015), !is.na(delic_binary)) %>% ggplot(aes(median_income2015)) + 
  geom_histogram(aes(y=..density..),bins=50) + facet_wrap(~delic_binary, nrow=2)  +
  geom_vline(data = subset(Total2, delic_binary), aes(xintercept = mean(median_income2015, na.rm=T)), linetype ="dashed", color ="black") + 
  geom_vline(data = subset(Total2, !delic_binary), aes(xintercept = mean(median_income2015, na.rm=T)), linetype ="dashed", color ="black") + th +
  labs(title = "Density plot of median income in MSA by delic status ")

etni_p <- (His_p + Bl_p + Inc_p + plot_layout(nrow=1)) + an
etni_p
#ggsave(etni_p,filename = "etni_p.pdf", width=12, height=5, dpi=300)
 


# US map ------------------------------------------------------------------

states_map <- map_data("state")
states <- data.frame(state.center, state.abb) %>% 
  rename(state = "state.abb")

Total2 %<>% filter(!(state %in% c("VI","PR","GU","DC"))) 

library(openintro)
library(maps)


dat <- Total2 %>% group_by(state) %>% summarize(n=n()) %>% 
  mutate(region = tolower(abbr2state(state)))%>% 
  left_join(states , by= "state")

ggplot(dat, aes(map_id = region)) +
  geom_map(aes(fill = n), map = states_map, color = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) + th +
  labs(title="US" , x=NULL, y=NULL, fill="Loans")+ 
  geom_text(aes(x = x, y = y, label = n), color="white")

dat2 <- Total2 %>% group_by(state) %>% filter(!is.na(delic_binary)) %>% filter(delic_binary==1) %>% summarise(n=n()) %>% 
  mutate(region = tolower(abbr2state(state)))%>% 
  left_join(states , by= "state")

ggplot(dat2, aes(map_id = region)) +
  geom_map(aes(fill = n), map = states_map, color = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) + th +
  labs(title="US" , x=NULL, y=NULL, fill="delic=1") + 
  geom_text(aes(x = x, y = y, label = n), color="white")

dat3 <- Total2 %>% group_by(state, Spop2018) %>% summarize(n=n()) %>% mutate(ratio = n/Spop2018) %>% 
  mutate(region = tolower(abbr2state(state))) %>% 
  left_join(states , by= "state")

ggplot(dat3, aes(map_id = region)) +
  geom_map(aes(fill = ratio), map = states_map, color = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) + th +
  labs(title="US" , x=NULL, y=NULL, fill="loans/state pop") + 
  geom_text(aes(x = x, y = y, label = round(ratio,3)), color="white")

dat4 <- Total2 %>% group_by(state, Spop2018) %>% filter(!is.na(delic_binary)) %>% filter(delic_binary==1) %>% summarise(n=n()) %>% mutate(ratio = n/Spop2018) %>%
  mutate(region = tolower(abbr2state(state))) %>% 
  left_join(states , by= "state")

ggplot(dat4, aes(map_id = region)) +
  geom_map(aes(fill = ratio), map = states_map, color = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) + th +
  labs(title="US" , x=NULL, y=NULL, fill="delic=1 / state pop") + 
  geom_text(aes(x = x, y = y, label = round(ratio,4)), color="white")

ratio_delic = dat2$n/dat$n

dat5 <- Total2 %>% group_by(state) %>% filter(!is.na(delic_binary)) %>% summarise(n=n()) %>% cbind(ratio_delic) %>% 
  mutate(region = tolower(abbr2state(state)))  %>% 
  left_join(states , by= "state")

ggplot(dat5, aes(map_id = region)) +
  geom_map(aes(fill = ratio_delic), map = states_map, color = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) + th +
  labs(title="US" , x=NULL, y=NULL, fill="delic=1 / delic= 0") + 
  geom_text(aes(x = x, y = y, label = round(ratio_delic,3)), color="white")

u <- unique(Total2$area)
length(u)

