library(readr)
library(tidyverse)

data <- read_csv("C:/Users/eriliu/Desktop/Svarbus failai/Univeras/6 semestras/Programavimas duomenu tvarkymui ir vizualizavimui/2 lab/KTU-duomenu-vizualizacija/laboratorinis/data/lab_sodra.csv")
summary(data)

#1uzd
data1 <- data %>%
  filter(ecoActCode == 452000)
data1 %>%
  ggplot(aes(x=avgWage)) +
  theme_minimal() +
  geom_histogram(fill = "blue", col = "red", bins = 100) +
  labs(title = "Awerage wage of employees")

#2uzd

data1 <- data1 %>% mutate(month_value=as.integer(substr(month, 5 ,7)))
top5 <- data1 %>% 
  group_by(name) %>% 
  slice_max(avgWage, n=1) %>% 
  ungroup() %>%
  top_n(avgWage, n=5) %>% 
  select(name)
data2 <- data1 %>% filter(name %in% top5$name)
data2 %>%
  ggplot(aes(x = month_value, y = avgWage, group = name)) +
  theme_minimal() +
  geom_point(aes(colour = name)) +
  scale_x_continuous("month",breaks=1:12,limits=c(1,12)) + 
  geom_line(aes(colour = name)) +
  labs(title = "Average wage of employees", x = "Month", y = "Average wage")

#3 uzd

data2 %>%
  group_by(name) %>%
  slice_max(numInsured, with_ties = FALSE) %>%
  head(5) %>%
  ggplot(aes(x = reorder(name, -numInsured), y = numInsured)) +
  geom_col(aes(fill = name)) +
  theme(axis.text.x = element_blank()) +
  theme_minimal() +
  labs(title = "Number of insured employees", x = "Company", y = "Count")