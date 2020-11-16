##SWP1
##Author: Luisa Hahn 


rm(list=ls())

setwd("/Users/hahnl/Desktop/Master/Kurse/Customer Analytics and Customer Insights/SWP1")

library(data.table)
library(ggplot2)
library(dplyr)
library(klaR)
library(psych)
library(lattice)
library(gridExtra)

data = read.csv("QuestionaireData_CityTrips_csv.csv", stringsAsFactors = FALSE)


##demographics##

#age
mean(data$Age, na.rm = TRUE)
ag <- data.frame(data$Age, data$Gender)
plot(ag$data.Age ~ ag$data.Gender , horizontal = TRUE, ylab = "Gender", xlab = "Age")
mean(data$Age[data$Gender == "Female"], na.rm = TRUE)
mean(data$Age[data$Gender == "Male"], na.rm = TRUE)

#Gender 
sum(data$Gender == "Male")
100/nrow(data)*sum(data$Gender == "Male")
sum(data$Gender == "Female")
100/nrow(data)*sum(data$Gender == "Female")



#Partnership
partnership = data.frame(data$PartnershipStatus)
plot(partnership)
sum(data$PartnershipStatus == "single")
sum(data$PartnershipStatus == "in a relationship.")
sum(data$PartnershipStatus == "married.")

#Occupation 
occ = data.frame(data$Occupation)
plot(occ)
occg <- data.frame(data$Occupation, data$Gender)
plot(occg) 




## Part 2 ##

## Berlin and Rome 



# Do you think Berlin/Rome is save? (by gender)

BS <- data.frame(Berlin_save = data$Berlin_Att18, gender = data$Gender )
BS <- na.omit(BS)
BS_g <- data.frame(BS_g = c("Female", "Male"), agree = c(mean(BS$Berlin_save[BS$gender == "Female"]), mean(BS$Berlin_save[BS$gender == "Male"])))
plot1 <- ggplot(data = BS_g, aes(BS_g, agree)) + 
  labs(title = "Do you think Berlin is save?", 
         y = "Level of Agreement" , x= "gender")  + ylim(1,5) + geom_point(size = 5, shape = 10)

RS <- data.frame(Rome_save = data$Rome_Att18, gender = data$Gender )
RS <- na.omit(RS)
RS_g <- data.frame(RS_g = c("Female", "Male"), agree = c(mean(RS$Rome_save[RS$gender == "Female"]), mean(RS$Rome_save[RS$gender == "Male"])))
plot2 <- ggplot(data = RS_g, aes(RS_g, agree)) + 
  labs(title = "Do you think Rome is save?", 
       y = "Level of Agreement" , x= "gender")  + ylim(1,5) + geom_point(size = 5, shape = 10)

grid.arrange(plot1, plot2, nrow=1, ncol=2)




#is Berlin too touristic? by from_Berlin / other_city 


Bt = data.frame(Berlin_touristic= data$Berlin_Att14,
                current_city = data$CurrentCity)

Bt$current_city <- fifelse(Bt$current_city %in% c("Berlin", "BERLIN"), "Berlin", "Not Berlin")
Bt1 = Bt %>%
  group_by(current_city) %>%
  summarize(Berlin_touristic = mean(Berlin_touristic, na.rm = T))
na.omit(Bt)
Bt1
plot3 <- ggplot(data = Bt1, aes(current_city, Berlin_touristic))  + theme_bw() + 
              labs(title = "Is Berlin too touristic?", y = "Level of Agreement", x = "Current City") + ylim(1,5) + geom_point(size = 5, shape = 10)


# compare to Paris by current_city = Paris


Pt = data.frame(Paris_touristic= data$Paris_Att14,
                current_city = data$CurrentCity)

Pt$current_city <- fifelse(Pt$current_city %in% "Paris", "Paris", "Not Paris")
Pt1 = Pt %>%
  group_by(current_city) %>%
  summarize(Paris_touristic = mean(Paris_touristic, na.rm = T))
na.omit(Pt)
Pt1
plot4 <- ggplot(data = Pt1, aes(current_city, Paris_touristic))  + theme_bw() + 
  labs(title = "Is Paris too touristic?", y = "Level of Agreement", x = "Current City")  + ylim(1,5) + geom_point(size = 5, shape = 10) 

grid.arrange(plot3, plot4, nrow=1, ncol=2)





#Is Berlin clean? by avg_budget 


mean(data$Berlin_Att11, na.rm = T)
mean(data$Rome_Att11, na.rm = T)

CB <- data.frame(b_clean = data$Berlin_Att11, bud = data$Avg_Budget)

budget <-  data.frame(budget= c("<200", "200-300", "300-400", "400-500", ">500"), 
                         agree = c(mean(CB$b_clean[CB$bud == 1], na.rm = T), mean(CB$b_clean[CB$bud == 2],na.rm = T),
                                   mean(CB$b_clean[CB$bud == 3],na.rm = T), mean(CB$b_clean[CB$bud == 4],na.rm = T),
                                   mean(CB$b_clean[CB$bud == 5],na.rm = T)))
budget
budget$budget = factor(budget$budget, levels = budget$budget[order(-budget$agree)])
ggplot(data = budget, aes(budget, agree)) + geom_col() + labs( title = "Is Berlin clean?") + theme_minimal()




# which city is more beautiful? by budget 
library(lattice)

#Berlin
BB <- data.frame(b_beautiful = data$Berlin_Att19, budg = data$Avg_Budget)

BBB <-  data.frame(budget= c("<200", "200-300", "300-400", "400-500", ">500"), 
                   agree = c(mean(BB$b_beautiful[BB$budg == 1], na.rm = T), mean(BB$b_beautiful[BB$budg == 2],na.rm = T),
                             mean(BB$b_beautiful[BB$budg == 3],na.rm = T), mean(BB$b_beautiful[BB$budg == 4],na.rm = T),
                             mean(BB$b_beautiful[BB$budg == 5],na.rm = T)))
BBB
BBB$budget = factor(BBB$budget, levels = BBB$budget[order(-BBB$agree)])
B <- ggplot(data = BBB, aes(budget, agree)) + geom_col()  + labs(title = "Berlin") 



#Rome 

RB <-  data.frame(R_beautiful = data$Rome_Att19, budg = data$Avg_Budget)
RBB <-  data.frame(budget= c("<200", "200-300", "300-400", "400-500", ">500"), 
                   agree = c(mean(RB$R_beautiful[RB$budg == 1], na.rm = T), mean(RB$R_beautiful[RB$budg == 2],na.rm = T),
                             mean(RB$R_beautiful[RB$budg == 3],na.rm = T), mean(RB$R_beautiful[RB$budg == 4],na.rm = T),
                             mean(RB$R_beautiful[RB$budg == 5],na.rm = T)))

RBB$budget = factor(RBB$budget, levels = RBB$budget[order(-RBB$agree)])
R <- ggplot(data = RBB, aes(budget, agree)) + geom_col()  + labs(title = "Rome") 


grid.arrange(B, R, nrow=1, ncol=2 )





#what's the most favourite city among the people who traveled more than 5 times in 2015



