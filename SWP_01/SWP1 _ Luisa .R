## CACI Winter Term 20/21

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
library(grid)
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
plot1

RS <- data.frame(Rome_save = data$Rome_Att18, gender = data$Gender )
RS <- na.omit(RS)
RS_g <- data.frame(RS_g = c("Female", "Male"), agree = c(mean(RS$Rome_save[RS$gender == "Female"]), mean(RS$Rome_save[RS$gender == "Male"])))
plot2 <- ggplot(data = RS_g, aes(RS_g, agree)) + 
  labs(title = "Do you think Rome is save?", 
       y = "Level of Agreement" , x= "gender")  + ylim(1,5) + geom_point(size = 5, shape = 10)

grid.arrange(plot1, plot2, nrow=1, ncol=2)




## Too touristic?


#is Berlin too touristic? by from_Berlin / other_city 

Bt = data.frame(Berlin_touristic= data$Berlin_Att14,
                current_city = data$CurrentCity)
Bt$current_city <- fifelse(Bt$current_city %in% c("Berlin", "BERLIN"), "Berlin", "Not Berlin")
Bt1 = Bt %>%
  group_by(current_city) %>%
  summarize(Berlin_touristic = mean(Berlin_touristic, na.rm = T))
na.omit(Bt)
Bt1


#London 
Lt = data.frame(London_touristic= data$London_Att14,
                current_city = data$CurrentCity)
Lt$current_city <- fifelse(Lt$current_city %in% "London", "London", "Not London")
Lt1 = Lt %>%
  group_by(current_city) %>%
  summarize(London_touristic = mean(London_touristic, na.rm = T))
na.omit(Lt)
Lt1


#Paris


Pt = data.frame(Paris_touristic= data$Paris_Att14,
                current_city = data$CurrentCity)

Pt$current_city <- fifelse(Pt$current_city %in% "Paris", "Paris", "Not Paris")
Pt1 = Pt %>%
  group_by(current_city) %>%
  summarize(Paris_touristic = mean(Paris_touristic, na.rm = T))
na.omit(Pt)
Pt1


#Barcelona  

Bat = data.frame(Barcelona_touristic= data$Barcelona_Att14,
                current_city = data$CurrentCity)
Bat$current_city <- fifelse(Bat$current_city %in% "Barcelona", "Barcelona", "Not Barcelona")
Bat1 = Bat %>%
  group_by(current_city) %>%
  summarize(Barcelona_touristic = mean(Barcelona_touristic, na.rm = T))
na.omit(Bat)
Bat1


# Stockholm

St = data.frame(Stockholm_touristic= data$Stockholm_Att14,
                 current_city = data$CurrentCity)
St$current_city <- fifelse(St$current_city %in% "Stockholm", "Stockholm", "Not Stockholm")
St1 = St %>%
  group_by(current_city) %>%
  summarize(Stockholm_touristic = mean(Stockholm_touristic, na.rm = T))
na.omit(St)
St1


# Budapest 

But = data.frame(Budapest_touristic= data$Budapest_Att14,
                current_city = data$CurrentCity)
But$current_city <- fifelse(But$current_city %in% "Budapest", "Budapest", "Not Budapest")
But1 = But %>%
  group_by(current_city) %>%
  summarize(Budapest_touristic = mean(Budapest_touristic, na.rm = T))
na.omit(But)
But1

# Vienna 

Vt = data.frame(Vienna_touristic= data$Vienna_Att14,
                 current_city = data$CurrentCity)
Vt$current_city <- fifelse(Vt$current_city %in% "Vienna", "Vienna", "Not Vienna")
Vt1 = Vt %>%
  group_by(current_city) %>%
  summarize(Vienna_touristic = mean(Vienna_touristic, na.rm = T))
na.omit(Vt)
Vt1

# overall mean 

Ctm <- data.frame(City_touristic = c(Bt1$Berlin_touristic, Pt1$Paris_touristic, Lt1$London_touristic, Bat1$Barcelona_touristic,
                                     St1$Stockholm_touristic, But1$Budapest_touristic, Vt1$Vienna_touristic),
                  current_city = c(Bt1$current_city, Pt1$current_city, Lt1$current_city, Bat1$current_city, St1$current_city,
                                   But1$current_city, Vt1$current_city))



# get the mean over all cities and plot

Ctm$current_city <- fifelse(Ctm$current_city %in% c("Not Berlin", "Not Paris", "Not London", "Not Barcelona",
                                                    "Not Stockholm", "Not Budapest", "Not Vienna"), "Tourists", "Locals")
Ctm1 = Ctm %>% 
        group_by(current_city) %>%
        summarize(City_touristic = mean(City_touristic))
na.omit(Ctm)
Ctm1


plot_Ctm1 <- ggplot(data = Ctm1, aes(current_city, City_touristic))  + theme_bw() + 
  labs(title = "Is the city too touristic?", y = "Level of Agreement", x="") + ylim(1,5) + geom_point(size = 5, shape = 10)
plot_Ctm1




#plotting diff. cities 

plotB <- ggplot(data = Bt1, aes(current_city, Berlin_touristic))  + theme_bw() + 
  labs(y="Level of Agreement", x = "") + ylim(1,5) + geom_point(size = 5, shape = 10)

plotP <- ggplot(data = Pt1, aes(current_city, Paris_touristic))  + theme_bw() + 
  labs(y="", x = "")  + ylim(1,5) + geom_point(size = 5, shape = 10) 

plotL <- ggplot(data = Lt1, aes(current_city, London_touristic))  + theme_bw() + 
  labs(y="", x = "")  + ylim(1,5) + geom_point(size = 5, shape = 10) 

plotBa <- ggplot(data = Bat1, aes(current_city, Barcelona_touristic))  + theme_bw() + 
  labs(y="", x = "")  + ylim(1,5) + geom_point(size = 5, shape = 10) 

plotS <- ggplot(data = St1, aes(current_city, Stockholm_touristic))  + theme_bw() + 
  labs(y="", x = "")  + ylim(1,5) + geom_point(size = 5, shape = 10) 

plotBu <- ggplot(data = But1, aes(current_city, Budapest_touristic))  + theme_bw() + 
  labs(y="",x = "")  + ylim(1,5) + geom_point(size = 5, shape = 10)

plotV <- ggplot(data = Vt1, aes(current_city, Vienna_touristic))  + theme_bw() + 
  labs(y="", x = "")  + ylim(1,5) + geom_point(size = 5, shape = 10) 


grid.arrange(plotB, plotP, plotL, plotBa, plotS, plotBu, plotV, nrow=1, ncol=7, top = "Is the city too touristic?") 
        





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
B <- ggplot(data = BBB, aes(budget, agree)) + geom_col()  + labs(title = "Berlin", x= "Budget", y= "Level of Agreement") 
B


#Rome 

RB <-  data.frame(R_beautiful = data$Rome_Att19, budg = data$Avg_Budget)
RBB <-  data.frame(budget= c("<200", "200-300", "300-400", "400-500", ">500"), 
                   agree = c(mean(RB$R_beautiful[RB$budg == 1], na.rm = T), mean(RB$R_beautiful[RB$budg == 2],na.rm = T),
                             mean(RB$R_beautiful[RB$budg == 3],na.rm = T), mean(RB$R_beautiful[RB$budg == 4],na.rm = T),
                             mean(RB$R_beautiful[RB$budg == 5],na.rm = T)))

RBB$budget = factor(RBB$budget, levels = RBB$budget[order(-RBB$agree)])
R <- ggplot(data = RBB, aes(budget, agree)) + geom_col()  + labs(title = "Rome", x= "Budget", y= "Level of Agreement") 


#Paris

PB <-  data.frame(P_beautiful = data$Paris_Att19, budg = data$Avg_Budget)
PBB <-  data.frame(budget= c("<200", "200-300", "300-400", "400-500", ">500"), 
                   agree = c(mean(PB$P_beautiful[PB$budg == 1], na.rm = T), mean(PB$P_beautiful[PB$budg == 2],na.rm = T),
                             mean(PB$P_beautiful[PB$budg == 3],na.rm = T), mean(PB$P_beautiful[PB$budg == 4],na.rm = T),
                             mean(PB$P_beautiful[PB$budg == 5],na.rm = T)))
PBB

PBB$budget = factor(PBB$budget, levels = PBB$budget[order(-PBB$agree)])
P <- ggplot(data = PBB, aes( budget, agree)) + geom_col()  + labs(title = "Paris", x= "Budget", y= "Level of Agreement") 
P

#London

LB <-  data.frame(L_beautiful = data$London_Att19, budg = data$Avg_Budget)
LBB <-  data.frame(budget= c("<200", "200-300", "300-400", "400-500", ">500"), 
                   agree = c(mean(LB$L_beautiful[LB$budg == 1], na.rm = T), mean(LB$L_beautiful[LB$budg == 2],na.rm = T),
                             mean(LB$L_beautiful[LB$budg == 3],na.rm = T), mean(LB$L_beautiful[LB$budg == 4],na.rm = T),
                             mean(LB$L_beautiful[LB$budg == 5],na.rm = T)))

LBB$budget = factor(LBB$budget, levels = LBB$budget[order(-LBB$agree)])
L <- ggplot(data = LBB, aes( budget, agree)) + geom_col()  + labs(title = "London", x= "Budget", y= "Level of Agreement") 
L


grid.arrange(B, R, P, L,  nrow=1, ncol=4, top= "Is the city beautiful?")









