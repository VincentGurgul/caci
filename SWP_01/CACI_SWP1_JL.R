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


#Eindeutige Stadtbesuche über alle Budgetklassen#

totalvisits=sum(data[,4:23])
uniquepervisits=(totalvisits/ncol(data))
uniquepervisits


#Eindeutige Stadtbesuche Budgetklasse 1#

totalvisitslb=sum(lowbudget[,4:23])
uniquepervisitslb=(totalvisitslb/ncol(lowbudget))
uniquepervisitslb


#Beusche Pro Jahr über alle Budgetklassen hinweg#

overallvisits=sum(data[,35])
overallpervisits=(overallvisits/ncol(data))
overallpervisitsweight=(overallpervisits*2.8)
overallpervisitsweight


#Besuche pro Jahr für Budgetklasse 1#

overallvisitslb=sum(lowbudget[,35])
overallpervisitslb=(overallvisitslb/ncol(lowbudget))
overallpervisitslbweight=overallpervisitslb*2.8
overallpervisitslbweight
