library(data.table)
library(ggplot2)
library(dplyr)
library(klaR)
library(psych)
library(lattice)
library(gridExtra)

setwd("C:/Users/Kerle/Desktop/Uni/CACI")
data = read.csv("QuestionaireData_CityTrips_csv.csv", stringsAsFactors = FALSE)

lowbudget=subset.data.frame(data,Avg_Budget==1)


#Eindeutige Stadtbesuche ?ber alle Budgetklassen#

totalvisits=sum(data[,4:23])
uniquepervisits=(totalvisits/ncol(data))
uniquepervisits


#Eindeutige Stadtbesuche Budgetklasse 1#

totalvisitslb=sum(lowbudget[,4:23])
uniquepervisitslb=(totalvisitslb/ncol(lowbudget))
uniquepervisitslb


#Beusche Pro Jahr ?ber alle Budgetklassen hinweg#

overallvisits=sum(data[,35])
overallpervisits=(overallvisits/ncol(data))
overallpervisitsweight=(overallpervisits*2.8)
overallpervisitsweight


#Besuche pro Jahr f?r Budgetklasse 1#

overallvisitslb=sum(lowbudget[,35])
overallpervisitslb=(overallvisitslb/ncol(lowbudget))
overallpervisitslbweight=overallpervisitslb*2.8
overallpervisitslbweight


#####


dd = data.frame("unique cities visited" = c(uniquepervisitslb, uniquepervisits),
                "overall cities visited" = c(overallpervisitslbweight, overallpervisitsweight),
                row.names = c("lowbudget travellers", "all travellers")
                )

gg = data.frame(x = c("all city visits", "   ", "unique city visits", "  "),
                y = c(overallpervisitsweight, overallpervisitslbweight, uniquepervisits, uniquepervisitslb),
                col = c("pink", "skyblue", "pink", "skyblue"))

gg$x = factor(gg$x, levels = gg$x)
ggplot(data = gg, aes(x,y, fill = col)) + geom_col(color = "black") +
  theme_minimal() +
  labs(title = "Comparing City Visits of All Travellers and Lowbudget Travellers", 
       y = "cities visited", fill = "Type of traveller") +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 3.5), 
        axis.text.x = element_text(size = 10, hjust = -0.8, vjust = 4),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_identity(guide = "legend",
                       labels = c("All travellers", "Lowbudget travellers", "All travellers", "Lowbudget travellers"))
       
