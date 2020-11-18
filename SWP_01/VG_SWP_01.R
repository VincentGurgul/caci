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


# Attribute 4: Trendiness

trendy = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                               "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                               "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                               "Athens", "Dublin"),
                      avg_score = c(mean(data$Berlin_Att4, na.rm = TRUE), mean(data$Paris_Att4, na.rm = TRUE),
                                    mean(data$London_Att4, na.rm = TRUE), mean(data$Barcelona_Att4, na.rm = TRUE),
                                    mean(data$Madrid_Att4, na.rm = TRUE), mean(data$Rome_Att4, na.rm = TRUE),
                                    mean(data$Stockholm_Att4, na.rm = TRUE), mean(data$Amsterdam_Att4, na.rm = TRUE),
                                    mean(data$Prague_Att4, na.rm = TRUE), mean(data$Budapest_Att4, na.rm = TRUE),
                                    mean(data$Lisbon_Att4, na.rm = TRUE), mean(data$Brussels_Att4, na.rm = TRUE),
                                    mean(data$Vienna_Att4, na.rm = TRUE), mean(data$StPetersburg_Att4, na.rm = TRUE),
                                    mean(data$Krakow_Att4, na.rm = TRUE), mean(data$Riga_Att4, na.rm = TRUE),
                                    mean(data$Istanbul_Att4, na.rm = TRUE), mean(data$Geneva_Att4, na.rm = TRUE),
                                    mean(data$Athens_Att4, na.rm = TRUE), mean(data$Dublin_Att4, na.rm = TRUE)
                      ))

trendy$city = factor(trendy$city, levels = trendy$city[order(-trendy$avg_score)])
ggplot(data = trendy, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(2.5, 4.5)) +
  theme_minimal() +
  labs(title = "Attribute 4: Trendiness", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())
# export as 600 by 250


# Attribute 5: Nightlife

night = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                             "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                             "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                             "Athens", "Dublin"),
                    avg_score = c(mean(data$Berlin_Att5, na.rm = TRUE), mean(data$Paris_Att5, na.rm = TRUE),
                                  mean(data$London_Att5, na.rm = TRUE), mean(data$Barcelona_Att5, na.rm = TRUE),
                                  mean(data$Madrid_Att5, na.rm = TRUE), mean(data$Rome_Att5, na.rm = TRUE),
                                  mean(data$Stockholm_Att5, na.rm = TRUE), mean(data$Amsterdam_Att5, na.rm = TRUE),
                                  mean(data$Prague_Att5, na.rm = TRUE), mean(data$Budapest_Att5, na.rm = TRUE),
                                  mean(data$Lisbon_Att5, na.rm = TRUE), mean(data$Brussels_Att5, na.rm = TRUE),
                                  mean(data$Vienna_Att5, na.rm = TRUE), mean(data$StPetersburg_Att5, na.rm = TRUE),
                                  mean(data$Krakow_Att5, na.rm = TRUE), mean(data$Riga_Att5, na.rm = TRUE),
                                  mean(data$Istanbul_Att5, na.rm = TRUE), mean(data$Geneva_Att5, na.rm = TRUE),
                                  mean(data$Athens_Att5, na.rm = TRUE), mean(data$Dublin_Att5, na.rm = TRUE)
                    ))

night$city = factor(night$city, levels = night$city[order(-night$avg_score)])
ggplot(data = night, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(2.6, 4.6)) +
  theme_minimal() +
  labs(title = "Attribute 5: Nightlife", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())


# Attribute 11: Cleanliness

clean = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                            "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                            "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                            "Athens", "Dublin"),
                   avg_score = c(mean(data$Berlin_Att11, na.rm = TRUE), mean(data$Paris_Att11, na.rm = TRUE),
                                 mean(data$London_Att11, na.rm = TRUE), mean(data$Barcelona_Att11, na.rm = TRUE),
                                 mean(data$Madrid_Att11, na.rm = TRUE), mean(data$Rome_Att11, na.rm = TRUE),
                                 mean(data$Stockholm_Att11, na.rm = TRUE), mean(data$Amsterdam_Att11, na.rm = TRUE),
                                 mean(data$Prague_Att11, na.rm = TRUE), mean(data$Budapest_Att11, na.rm = TRUE),
                                 mean(data$Lisbon_Att11, na.rm = TRUE), mean(data$Brussels_Att11, na.rm = TRUE),
                                 mean(data$Vienna_Att11, na.rm = TRUE), mean(data$StPetersburg_Att11, na.rm = TRUE),
                                 mean(data$Krakow_Att11, na.rm = TRUE), mean(data$Riga_Att11, na.rm = TRUE),
                                 mean(data$Istanbul_Att11, na.rm = TRUE), mean(data$Geneva_Att11, na.rm = TRUE),
                                 mean(data$Athens_Att11, na.rm = TRUE), mean(data$Dublin_Att11, na.rm = TRUE)
                   ))

clean$city = factor(clean$city, levels = clean$city[order(-clean$avg_score)])
ggplot(data = clean, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(2.6, 4.3)) +
  theme_minimal() +
  labs(title = "Attribute 11: Cleanliness", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())


# Attribute 14: Tourism

tour = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                          "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                          "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                          "Athens", "Dublin"),
                 avg_score = c(mean(data$Berlin_Att14, na.rm = TRUE), mean(data$Paris_Att14, na.rm = TRUE),
                               mean(data$London_Att14, na.rm = TRUE), mean(data$Barcelona_Att14, na.rm = TRUE),
                               mean(data$Madrid_Att14, na.rm = TRUE), mean(data$Rome_Att14, na.rm = TRUE),
                               mean(data$Stockholm_Att14, na.rm = TRUE), mean(data$Amsterdam_Att14, na.rm = TRUE),
                               mean(data$Prague_Att14, na.rm = TRUE), mean(data$Budapest_Att14, na.rm = TRUE),
                               mean(data$Lisbon_Att14, na.rm = TRUE), mean(data$Brussels_Att14, na.rm = TRUE),
                               mean(data$Vienna_Att14, na.rm = TRUE), mean(data$StPetersburg_Att14, na.rm = TRUE),
                               mean(data$Krakow_Att14, na.rm = TRUE), mean(data$Riga_Att14, na.rm = TRUE),
                               mean(data$Istanbul_Att14, na.rm = TRUE), mean(data$Geneva_Att14, na.rm = TRUE),
                               mean(data$Athens_Att14, na.rm = TRUE), mean(data$Dublin_Att14, na.rm = TRUE)
                 ))

tour$city = factor(tour$city, levels = tour$city[order(-tour$avg_score)])
ggplot(data = tour, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(2.4, 4.4)) +
  theme_minimal() +
  labs(title = "Attribute 14: Tourism", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())


# Attribute 19: Beauty

bt = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                          "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                          "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                          "Athens", "Dublin"),
                 avg_score = c(mean(data$Berlin_Att19, na.rm = TRUE), mean(data$Paris_Att19, na.rm = TRUE),
                               mean(data$London_Att19, na.rm = TRUE), mean(data$Barcelona_Att19, na.rm = TRUE),
                               mean(data$Madrid_Att19, na.rm = TRUE), mean(data$Rome_Att19, na.rm = TRUE),
                               mean(data$Stockholm_Att19, na.rm = TRUE), mean(data$Amsterdam_Att19, na.rm = TRUE),
                               mean(data$Prague_Att19, na.rm = TRUE), mean(data$Budapest_Att19, na.rm = TRUE),
                               mean(data$Lisbon_Att19, na.rm = TRUE), mean(data$Brussels_Att19, na.rm = TRUE),
                               mean(data$Vienna_Att19, na.rm = TRUE), mean(data$StPetersburg_Att19, na.rm = TRUE),
                               mean(data$Krakow_Att19, na.rm = TRUE), mean(data$Riga_Att19, na.rm = TRUE),
                               mean(data$Istanbul_Att19, na.rm = TRUE), mean(data$Geneva_Att19, na.rm = TRUE),
                               mean(data$Athens_Att19, na.rm = TRUE), mean(data$Dublin_Att19, na.rm = TRUE)
                 ))

bt$city = factor(bt$city, levels = bt$city[order(-bt$avg_score)])
ggplot(data = bt, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(3.4, 4.5)) +
  theme_minimal() +
  labs(title = "Attribute 19: Beauty", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())



# Attribute 20: English speaker friendliness

eng = data.frame(city = c("Berlin", "Paris", "London", "Barcelona", "Madrid", "Rome",
                            "Stockholm", "Amsterdam", "Prague", "Budapest", "Lisbon", "Brussels",
                            "Vienna", "St. Petersburg", "Krakow", "Riga", "Istanbul", "Geneva",
                            "Athens", "Dublin"),
                   avg_score = c(mean(data$Berlin_Att20, na.rm = TRUE), mean(data$Paris_Att20, na.rm = TRUE),
                                 mean(data$London_Att20, na.rm = TRUE), mean(data$Barcelona_Att20, na.rm = TRUE),
                                 mean(data$Madrid_Att20, na.rm = TRUE), mean(data$Rome_Att20, na.rm = TRUE),
                                 mean(data$Stockholm_Att20, na.rm = TRUE), mean(data$Amsterdam_Att20, na.rm = TRUE),
                                 mean(data$Prague_Att20, na.rm = TRUE), mean(data$Budapest_Att20, na.rm = TRUE),
                                 mean(data$Lisbon_Att20, na.rm = TRUE), mean(data$Brussels_Att20, na.rm = TRUE),
                                 mean(data$Vienna_Att20, na.rm = TRUE), mean(data$StPetersburg_Att20, na.rm = TRUE),
                                 mean(data$Krakow_Att20, na.rm = TRUE), mean(data$Riga_Att20, na.rm = TRUE),
                                 mean(data$Istanbul_Att20, na.rm = TRUE), mean(data$Geneva_Att20, na.rm = TRUE),
                                 mean(data$Athens_Att20, na.rm = TRUE), mean(data$Dublin_Att20, na.rm = TRUE)
                   ))

eng$city = factor(eng$city, levels = eng$city[order(-eng$avg_score)])
ggplot(data = eng, aes(city, avg_score)) + 
  geom_col(fill = "grey", color = "black") + 
  coord_cartesian(ylim=c(2.6, 4.8)) +
  theme_minimal() +
  labs(title = "Attribute 20: English speaker friendliness", 
       y = "Average score") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.45), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.05, vjust = 1.1),
        axis.title.x = element_blank())














## the night is dark and full of errors ##

        
        


