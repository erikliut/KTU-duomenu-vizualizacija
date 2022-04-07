library(ggplot2)
library(readr)
library(tibble)
library(tidyr)
library(tidyverse)
data <- read_csv("lab_sodra.csv")
summary(data)
#Filtravimas
data1 <- filter(data, ecoActCode==452000)

#1uzd.
data1 %>%
  ggplot(aes(x=avgWage)) +
  theme_minimal() +
  geom_histogram(bins=100)

#2uzd
data2 <- data1 %>%
  group_by(name) %>%
  summarise(avg = mean(avgWage)) %>%
  arrange(desc(avg))
data3 <- merge(data1, data2)
data3 %>%
  arrange(desc(avg)) %>%
  head(60) %>%
  ggplot(aes(x = month, y = avgWage, group = name)) +
  theme_minimal()+
  theme(axis.text.x = element_blank()) +
  geom_line(aes(colour = name))