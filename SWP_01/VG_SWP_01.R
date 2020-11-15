## title: CACI SWP 1 
## author: Vincent Gurgul

rm(list=ls())

setwd("/Users/VincentGurgul/Documents/bwl/msc_03/caci/swp_01")

library(data.table)
library(ggplot2)
library(dplyr)
library(klaR)

data = read.csv("QuestionaireData_CityTrips_csv.csv", stringsAsFactors = FALSE)


#### Personal Data ####

mean(data$Age, na.rm = TRUE)
sum(data$Gender == "Male")
sum(data$Gender == "Female")

occ = data.frame(data$Occupation)
plot(occ)

partnership = data.frame(data$PartnershipStatus)
plot(partnership)


## preferences based on origin ##

LB1 = data.frame(Berlin_friendly = data$Berlin_Att1,
                 current_city = data$CurrentCity)

LB1$current_city <- fifelse(LB1$current_city != "Berlin", "Not Berlin", "Berlin")

LB2 = LB1 %>%
  group_by(current_city) %>%
  summarize(Berlin_friendly = mean(Berlin_friendly, na.rm = T))

rm(LB1)
LB2  
# Non-berliners find Berlin more friendly than Berliners


#### Part 1 ####

## Question 1 ##

# Which of the following cities have you visited?

visited = data.frame(city = c("Berlin", "Paris", "London", "Barcelone", "Madrid", "Rome",
                              "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                              "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                              "Athens", "Dublin"),
                     visitors = c(sum(data$Berlin), sum(data$Paris), sum(data$London), sum(data$Barcelona), sum(data$Madrid), sum(data$Rome),
                     sum(data$Stockholm), sum(data$Amsterdam), sum(data$Prague), sum(data$Budapest), sum(data$Lisbon), sum(data$Brussels),
                     sum(data$Vienna), sum(data$StPetersburg), sum(data$Krakow), sum(data$Riga), sum(data$Istanbul), sum(data$Geneva),
                     sum(data$Athens), sum(data$Dublin)))

visited$city = factor(visited$city, levels = visited$city[order(-visited$visitors)])
ggplot(data = visited, aes(city, visitors)) + geom_col(fill = "grey", color = "black")


## Question 2 ##

# With what purpose do you usually go on a weekend trip?

purpose = data.frame(purpose = c("visit family", "visit friends", "partying", "exploring new city", "cultural events"),
                     share = c(sum(data$Purpose1)/266, sum(data$Purpose2)/266, sum(data$Purpose3)/266, sum(data$Purpose4)/266, 
                               sum(data$Purpose5)/266))

purpose$purpose = factor(purpose$purpose, levels = purpose$purpose[order(-purpose$share)])
ggplot(data = purpose, aes(purpose, share)) + geom_col(fill = "grey", color = "black")


## Question 3 ##

# With whom do you usually go on a weekend trip?

whom = data.frame(whom = c("family", "friends", "partner", "colleagues", "alone"),
                  share = c(sum(data$With_Whom_1)/266, sum(data$With_Whom_2)/266, sum(data$With_Whom_3)/266, sum(data$With_Whom_4)/266,
                            sum(data$With_Whom_5)/266))

whom$whom = factor(whom$whom, levels = whom$whom[order(-whom$share)])
ggplot(data = whom, aes(whom, share)) + geom_col(fill = "grey", color = "black")


## Question 4 ##

# How many times during 2015 have you been on a weekend trip?

times = data.frame(times = c("none", "once", "2-3", "4-5", ">5"),
                   share = c(sum(data$Number_of_Trips == 1)/266, sum(data$Number_of_Trips == 2)/266, sum(data$Number_of_Trips == 3)/266,
                             sum(data$Number_of_Trips == 4)/266, sum(data$Number_of_Trips == 5)/266))      

times$times = factor(times$times, levels = times$times)
ggplot(data = times, aes(times, share)) + geom_col(fill = "grey", color = "black")


## Question 5 ##

# What is your average budget for a weekend trip?

budget = data.frame(budget = c("<200", "200-300", "300-400", "400-500", ">500"),
                    share = c(sum(data$Avg_Budget == 1)/266, sum(data$Avg_Budget == 2)/266, sum(data$Avg_Budget == 3)/266,
                              sum(data$Avg_Budget == 4)/266, sum(data$Avg_Budget == 5)/266))

budget$budget = factor(budget$budget, levels = budget$budget)
ggplot(data = budget, aes(budget, share)) + geom_col(fill = "grey", color = "black")


## Question 6 ##

# Please rate the following cities in terms of your preference to visit them on a 7point scale

pref = data.frame(city = c("Berlin", "Paris", "London", "Barcelone", "Madrid", "Rome",
                           "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                           "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                           "Athens", "Dublin"),
                  avg_score = c(mean(data$Pref_Berlin, na.rm = TRUE), mean(data$Pref_Paris, na.rm = TRUE),
                                mean(data$Pref_London, na.rm = TRUE), mean(data$Pref_Barcelona, na.rm = TRUE),
                                mean(data$Pref_Madrid, na.rm = TRUE), mean(data$Pref_Rome, na.rm = TRUE),
                                mean(data$Pref_Stockholm, na.rm = TRUE), mean(data$Pref_Amsterdam, na.rm = TRUE),
                                mean(data$Pref_Prague, na.rm = TRUE), mean(data$Pref_Budapest, na.rm = TRUE),
                                mean(data$Pref_Lisbon, na.rm = TRUE), mean(data$Pref_Brussels, na.rm = TRUE),
                                mean(data$Pref_Vienna, na.rm = TRUE), mean(data$Pref_StPetersburg, na.rm = TRUE),
                                mean(data$Pref_Krakow, na.rm = TRUE), mean(data$Pref_Riga, na.rm = TRUE),
                                mean(data$Pref_Istanbul, na.rm = TRUE), mean(data$Pref_Geneva, na.rm = TRUE),
                                mean(data$Pref_Athens, na.rm = TRUE), mean(data$Pref_Dublin, na.rm = TRUE)
                                ))

pref$city = factor(pref$city, levels = pref$city[order(-pref$avg_score)])
ggplot(data = pref, aes(city, avg_score)) + geom_col(fill = "grey", color = "black") + coord_cartesian(ylim=c(3.5, 5.5))


#### Part 2 ####

# t.b.c.









