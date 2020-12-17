
### SWP 2 Part A
### author: VG

setwd("/Users/VincentGurgul/Documents/BWL/msc_03/caci/swp_02")

library(data.table)
library(klaR)
library(ggplot2)
library(dplyr)
library(wooldridge)
library(corrplot)
library(tidyverse)
library(magrittr)
library(ggpubr)
library(tibble)
library(MASS)
library(reshape2)
library(datasets)
library(bayesm)

data.long <- read.csv("data.cities.long.csv")
data.wide <- read.csv("QuestionaireData_CityTrips_csv.csv")

## choosing subset

data.wide$drop <- NA

data.wide$drop <- ifelse(data.wide$Age <= 11, 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Age >= 30, 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Nationality != "German", 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Nationality == "Deutsch", 0, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Occupation == "Diploma", 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Occupation == "Employed", 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Occupation == "Going to be a Me", 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Occupation == "Schoolboy", 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Occupation == "Self-Employed", 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Occupation == "state examinatio", 1, data.wide$drop)
data.wide$drop <- ifelse(data.wide$Occupation == "Unemployed", 1, data.wide$drop)

data.wide$drop <- ifelse(is.na(data.wide$drop), 0, data.wide$drop)

data.wide <- subset(data.wide, drop == 0)

selected <- data.wide[, c("Sample", "ID")]
data.long <- merge(selected, data.long, by = c("Sample", "ID"), all.x = TRUE)

## MDS

names(data.long)[names(data.long) == "Pref"] <- "preference"
names(data.long)[names(data.long) == "V1"] <- "friendly"
names(data.long)[names(data.long) == "V2"] <- "historical"
names(data.long)[names(data.long) == "V3"] <- "affordable"
names(data.long)[names(data.long) == "V4"] <- "trendy"
names(data.long)[names(data.long) == "V5"] <- "nightlife"
names(data.long)[names(data.long) == "V6"] <- "delicious_food"
names(data.long)[names(data.long) == "V7"] <- "transportation"
names(data.long)[names(data.long) == "V8"] <- "shopping"
names(data.long)[names(data.long) == "V9"] <- "cultural_events"
names(data.long)[names(data.long) == "V10"] <- "interesting_museums"
names(data.long)[names(data.long) == "V11"] <- "clean"
names(data.long)[names(data.long) == "V12"] <- "green"
names(data.long)[names(data.long) == "V13"] <- "international"
names(data.long)[names(data.long) == "V14"] <- "too_touristic"
names(data.long)[names(data.long) == "V15"] <- "fun"
names(data.long)[names(data.long) == "V16"] <- "noisy"
names(data.long)[names(data.long) == "V17"] <- "romantic"
names(data.long)[names(data.long) == "V18"] <- "safe"
names(data.long)[names(data.long) == "V19"] <- "beautiful"
names(data.long)[names(data.long) == "V20"] <- "english_speaker_friendly"

attribute <- aggregate(data.long[,-c(1,2,3)], by=list(data.long$City), mean, na.rm=TRUE)

## metric

dist <- dist(attribute) 
fit <- cmdscale(dist, k = 2)
x <- fit[,1]
y <- fit[,2]

ggplot(as.data.frame(fit), aes(V1, V2, label = attribute$Group.1)) +
  labs(title = "Metric MDS") +
  coord_cartesian(ylim = c(-3, 3), xlim = c(-3, 3)) +
  geom_point() + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(size=3, hjust = 1, vjust = 1.7) + theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


# with property fitting

profit <- lm(cbind(preference,
                   friendly,
                   historical,
                   affordable,
                   trendy,
                   nightlife,
                   delicious_food,
                   transportation,
                   shopping,
                   cultural_events,
                   interesting_museums,
                   clean,
                   green,
                   international,
                   too_touristic,
                   fun,
                   noisy,
                   romantic,
                   safe,
                   beautiful,
                   english_speaker_friendly) ~ -1 + x + y, data = attribute)

coef(summary(profit))

d = data.frame(V1 = coef(profit)[1,]*10,
               V2 = coef(profit)[2,]*10)

ggplot(as.data.frame(fit)) +
  labs(title = "Metric MDS") +
  coord_cartesian(ylim = c(-4, 5.1), xlim = c(-4.1, 3)) +
  geom_point(aes(V1, V2)) + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(V1, V2, label = attribute$Group.1), size=4, hjust = 1, vjust = 1.7) + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_segment(data = d, aes(x = 0, y = 0, xend = d$V1, yend = d$V2), arrow = arrow(length = unit(3, "mm")), colour = "darkred") +
  geom_text(data = d, aes(V1, V2, label = rownames(d)), size=3.1, hjust = 1.1, vjust = -0.5, colour = "darkred")

## non metric

dist2 <- dist(attribute) 
fit2 <- isoMDS(dist2, k = 2)
x2 <- fit2$points[,1]
y2 <- fit2$points[,2]

ggplot(as.data.frame(fit2$points), aes(V1, V2, label = attribute$Group.1)) +
  labs(title = "Non-Metric MDS") +
  coord_cartesian(ylim = c(-3, 3), xlim = c(-3, 3)) +
  geom_point() + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(size=3, hjust = 1, vjust = 1.7) + theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# with property fitting

profit2 <- lm(cbind(preference,
                   friendly,
                   historical,
                   affordable,
                   trendy,
                   nightlife,
                   delicious_food,
                   transportation,
                   shopping,
                   cultural_events,
                   interesting_museums,
                   clean,
                   green,
                   international,
                   too_touristic,
                   fun,
                   noisy,
                   romantic,
                   safe,
                   beautiful,
                   english_speaker_friendly) ~ -1 + x2 + y2, data = attribute)

coef(summary(profit))

d2 = data.frame(V1 = coef(profit2)[1,]*10,
               V2 = coef(profit2)[2,]*10)

ggplot(as.data.frame(fit2$points)) +
  labs(title = "Non-Metric MDS") +
  coord_cartesian(ylim = c(-4, 5.1), xlim = c(-4, 3)) +
  geom_point(aes(V1, V2)) + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(V1, V2, label = attribute$Group.1), size=4, hjust = 1, vjust = 1.7) + 
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_segment(data = d2, aes(x = 0, y = 0, xend = d2$V1, yend = d2$V2), arrow = arrow(length = unit(3, "mm")), colour = "darkred") +
  geom_text(data = d2, aes(V1, V2, label = rownames(d2)), size=3.1, hjust = 1.1, vjust = -0.5, colour = "darkred")

