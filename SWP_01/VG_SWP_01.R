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


# occupation

occ = data.frame(occ = data$Occupation)
occ$occ = ifelse(occ$occ %in% c("Diploma", "diploma student", "Going to be a Me", "Schoolboy", "state examinatio"), "Other", as.character(occ$occ))
levels(as.factor(occ$occ))
occupation = data.frame(occupation = c("Bachelor student", "Employed", "Master student", "Other", "PhD Student", "Self-employed", "Unemployed"),
                        share = c(sum(occ$occ == "Bachelor student")/2.66, sum(occ$occ == "Employed")/2.66, 
                                  sum(occ$occ == "Master student")/2.66, sum(occ$occ == "Other")/266, sum(occ$occ == "PhD student")/2.66, 
                                  sum(occ$occ == "Self-employed")/2.66, sum(occ$occ == "Unemployed")/2.66)
                        )
occupation$occupation = factor(occupation$occupation, levels = occupation$occupation[order(-occupation$share)])
ggplot(data = occupation, aes(occupation, share)) + 
  labs(title = "Occupation of Participants", 
       y = "Share in %") +
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.46), 
        axis.text.x = element_text(size = 10, angle = 20, hjust = 0.6, vjust = 0.85),
        axis.title.x = element_blank()) 
  # export as 600 by 200


# partnership status

partnership = data.frame(part = c("don't trust no hoe", "in a relationship", "married", "single"),
                         share = c(sum(data$PartnershipStatus == "don't trust no hoe")/2.66,
                                   sum(data$PartnershipStatus == "in a relationship.")/2.66,
                                   sum(data$PartnershipStatus == "married.")/2.66,
                                   sum(data$PartnershipStatus == "single")/2.66)
                                   )
partnership$part = factor(partnership$part, levels = partnership$part[order(-partnership$share)])
ggplot(data = partnership, aes(part, share)) + 
  labs(title = "Partnership Status of Participants", 
       y = "Share in %") +
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.46), 
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.6, vjust = 0.9),
        axis.title.x = element_blank())
  # export as 600 by 150

#### Part 1 ####

## Question 1 ##

# Which of the following cities have you visited?

visited = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                              "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                              "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                              "Athens", "Dublin"),
                     visitors = c(sum(data$Berlin), sum(data$Paris), sum(data$London), sum(data$Barcelona), sum(data$Madrid), sum(data$Rome),
                     sum(data$Stockholm), sum(data$Amsterdam), sum(data$Prague), sum(data$Budapest), sum(data$Lisbon), sum(data$Brussels),
                     sum(data$Vienna), sum(data$StPetersburg), sum(data$Krakow), sum(data$Riga), sum(data$Istanbul), sum(data$Geneva),
                     sum(data$Athens), sum(data$Dublin)))

visited$city = factor(visited$city, levels = visited$city[order(-visited$visitors)])
ggplot(data = visited, aes(city, visitors)) + 
  labs(title = "Question 1: Which of the following cities have you visited?", 
       y = "Number of visitors") +
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.35), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.15),
        axis.title.x = element_blank())
  # export as 600 by 300


## Question 2 ##

# With what purpose do you usually go on a weekend trip?

purpose = data.frame(purpose = c("visit family", "visit friends", "partying", "exploring new city", "cultural events"),
                     share = c(sum(data$Purpose1)/2.66, sum(data$Purpose2)/2.66, sum(data$Purpose3)/2.66, sum(data$Purpose4)/2.66, 
                               sum(data$Purpose5)/2.66))

purpose$purpose = factor(purpose$purpose, levels = purpose$purpose[order(-purpose$share)])
ggplot(data = purpose, aes(purpose, share)) + 
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  labs(title = "Question 2: With what purpose do you usually go on a weekend trip?", 
       y = "Share in %") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.35), 
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 3),
        axis.title.x = element_blank())
# export as 600 by 180


## Question 3 ##

# With whom do you usually go on a weekend trip?

whom = data.frame(whom = c("family", "friends", "partner", "colleagues", "alone"),
                  share = c(sum(data$With_Whom_1)/2.66, sum(data$With_Whom_2)/2.66, sum(data$With_Whom_3)/2.66, sum(data$With_Whom_4)/2.66,
                            sum(data$With_Whom_5)/2.66))

whom$whom = factor(whom$whom, levels = whom$whom[order(-whom$share)])
ggplot(data = whom, aes(whom, share)) + 
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  labs(title = "Question 3: With whom do you usually go on a weekend trip?", 
       y = "Share in %") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.35), 
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 3),
        axis.title.x = element_blank())
# export as 600 by 180


## Question 4 ##

# How many times during 2015 have you been on a weekend trip?

times = data.frame(times = c("none", "once", "2-3", "4-5", ">5"),
                   share = c(sum(data$Number_of_Trips == 1)/2.66, sum(data$Number_of_Trips == 2)/2.66, sum(data$Number_of_Trips == 3)/2.66,
                             sum(data$Number_of_Trips == 4)/2.66, sum(data$Number_of_Trips == 5)/2.66))      

times$times = factor(times$times, levels = times$times)
ggplot(data = times, aes(times, share)) + 
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  labs(title = "Question 4: How many times during 2015 have you been on a weekend trip?", 
       y = "Share in %") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.35), 
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 3),
        axis.title.x = element_blank())
# export as 600 by 180


## Question 5 ##

# What is your average budget for a weekend trip?

budget = data.frame(budget = c("<200", "200-300", "300-400", "400-500", ">500"),
                    share = c(sum(data$Avg_Budget == 1)/2.66, sum(data$Avg_Budget == 2)/2.66, sum(data$Avg_Budget == 3)/2.66,
                              sum(data$Avg_Budget == 4)/2.66, sum(data$Avg_Budget == 5)/2.66))

budget$budget = factor(budget$budget, levels = budget$budget)
ggplot(data = budget, aes(budget, share)) + 
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  labs(title = "Question 5: What is your average budget for a weekend trip?", 
       y = "Share in %") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.35), 
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 3),
        axis.title.x = element_blank())
# export as 600 by 180



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
ggplot(data = pref, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(3.5, 5.5)) +
  theme_minimal() +
  labs(title = "Question 6: Please rate the following cities in terms of \n your preference to visit them on a 7-point scale", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())
# export as 600 by 250


#### Part 2 ####

# preferences based on origin:

LB = data.frame(Berlin_friendly = data$Berlin_Att16,
                 current_city = data$CurrentCity)

LB$current_city <- fifelse(LB$current_city %in% c("Berlin", "BERLIN"), "Berlin", "Not Berlin")

LB1 = LB %>%
  group_by(current_city) %>%
  summarize(Berlin_friendly = mean(Berlin_friendly, na.rm = T))

rm(LB)
LB1    # Non-berliners find Berlin more friendly than Berliners


# plotting attributes #

# Attribute 1 : friendliness

friendly = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                               "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                               "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                               "Athens", "Dublin"),
                      avg_score = c(mean(data$Berlin_Att1, na.rm = TRUE), mean(data$Paris_Att1, na.rm = TRUE),
                               mean(data$London_Att1, na.rm = TRUE), mean(data$Barcelona_Att1, na.rm = TRUE),
                               mean(data$Madrid_Att1, na.rm = TRUE), mean(data$Rome_Att1, na.rm = TRUE),
                               mean(data$Stockholm_Att1, na.rm = TRUE), mean(data$Amsterdam_Att1, na.rm = TRUE),
                               mean(data$Prague_Att1, na.rm = TRUE), mean(data$Budapest_Att1, na.rm = TRUE),
                               mean(data$Lisbon_Att1, na.rm = TRUE), mean(data$Brussels_Att1, na.rm = TRUE),
                               mean(data$Vienna_Att1, na.rm = TRUE), mean(data$StPetersburg_Att1, na.rm = TRUE),
                               mean(data$Krakow_Att1, na.rm = TRUE), mean(data$Riga_Att1, na.rm = TRUE),
                               mean(data$Istanbul_Att1, na.rm = TRUE), mean(data$Geneva_Att1, na.rm = TRUE),
                               mean(data$Athens_Att1, na.rm = TRUE), mean(data$Dublin_Att1, na.rm = TRUE)
                      ))

friendly$city = factor(friendly$city, levels = friendly$city[order(-friendly$avg_score)])
ggplot(data = friendly, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(3, 4.2)) +
  theme_minimal() +
  labs(title = "Attribute 1: Friendliniess", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())
# export as 600 by 250


## the night is dark and full of errors ##

        
        


