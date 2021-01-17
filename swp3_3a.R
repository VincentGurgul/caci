## SWP 3 
## 3a

setwd("/Users/hahnl/Desktop/Master/Kurse/Customer Analytics and Customer Insights/SWP3")
data<-read.csv("indivData.csv")

library(plyr)
library(data.table)
library(ggplot2)
library(dplyr)
library(psych)
library(lattice)
library(gridExtra)
library(tidyverse)
library(klaR)
library(base)
library(labelled)

data <- data[data$Gender!= "3" & data$Income != "8", ] #drop rows where gender=3 and rows where income=8
view(data) 

#Demograpics - had to adjust them due to dropping data where peeps didnt state their gender or income
nrow(data) #506 entries left 
nrow(data[data$Gender == "1",]) #230 -> 45,45%
nrow(data[data$Gender == "2",]) #276 -> 54,55%
nrow(data[data$Residence == "Germany",]) #289 -> 57.11%
nrow(data[data$Age == "2",]) #216 
nrow(data[data$Age == "3",]) #202+ 216 = 418 -> 82.60% 
nrow(data[data$Age == "4",]) #36
nrow(data[data$Income== "1",]) #108
nrow(data[data$Income== "2",]) #185 + 108 = 293 -> 57,90%
nrow(data[data$Occupation== "1",]) #161 Employed peeps -> 31.82%
nrow(data[data$Occupation== "3",]) #293 Students -> 57.90%

# 1 was ist den befragten allgemein am wichtigten von den 4 punkten 
# 2 gibt es signifikante Unterschiede bei den Geschlechtern / Alter? 
# 3 wie sieht es mit den leuten aus die ownen vs nicht ownen?
# 4 ganz wichtig: wer intented to buy, worauf legen sie viel wert und wie schätzen diese leute sich ein? 
# 5 intent to buy vs wer besitzt schon welche? 

#battery
mb <- mean(data$RelImp_battery)  
mb
                     
#price 
mp <- mean(data$RelImp_price)
mp

#sound
ms <- mean(data$RelImp_sound)
ms

#weight
mw <- mean(data$RelImp_weight)
mw

df.prop <- data.frame(property = c("battery","price", "sound", "weight"), means = c(mb,mp,ms,mw))
df.prop


#gender 

df.mb.gender <- data.frame(gender = c("female", "male"), 
                           battery_mean = c(mean(data$RelImp_battery[data$Gender=="1"]), mean(data$RelImp_battery[data$Gender=="2"])))
df.mb.gender


df.mp.gender <- data.frame(gender = c("female", "male"), 
                           price_mean = c(mean(data$RelImp_price[data$Gender=="1"]),
                                          mean(data$RelImp_price[data$Gender=="2"])))
df.mp.gender


df.ms.gender <- data.frame(gender = c("female", "male"), 
                           sound_mean = c(mean(data$RelImp_sound[data$Gender=="1"]),
                                          mean(data$RelImp_sound[data$Gender=="2"])))
df.ms.gender


df.mw.gender <- data.frame(gender = c("female", "male"), 
                           weight_mean = c(mean(data$RelImp_weight[data$Gender=="1"]),
                                           mean(data$RelImp_weight[data$Gender=="2"])))
df.mw.gender


means.gender <- data.frame(gender = c("female", "male"), battery = c(df.mb.gender$battery_mean), 
                           price = c(df.mp.gender$price_mean),
                           sound = c(df.ms.gender$sound_mean),
                           weight = c(df.mw.gender$weight_mean))
means.gender


# trying to plot each property by gender 
df.mw.gender$gender = factor(df.mw.gender$gender, levels = df.mw.gender$gender[order(-df.mw.gender$weight_mean)])

w <- ggplot(data= df.mw.gender, mapping=aes(gender, weight_mean)) + geom_point(shape = 10, size=3)+ scale_y_continuous(limits = c(10, 40)) +labs(title = "Weight")+ theme(axis.title.y=element_blank(),axis.title.x = element_blank())
b <- ggplot(data= df.mb.gender, mapping=aes(gender, battery_mean)) + geom_point(shape = 10, size=3)+ scale_y_continuous(limits = c(10, 40)) +labs(title = "Battery", y = "Relative Importance in %") + theme(axis.title.x = element_blank())
p <- ggplot(data= df.mp.gender, mapping=aes(gender, price_mean)) + geom_point(shape = 10, size=3)+ scale_y_continuous(limits = c(10, 40)) +labs(title = "Price")+ theme(axis.title.y=element_blank(),axis.title.x = element_blank())
s <- ggplot(data= df.ms.gender, mapping=aes(gender, sound_mean)) + geom_point(shape = 10, size=3)+ scale_y_continuous(limits = c(10, 40)) +labs(title = "Sound")+ theme(axis.title.y=element_blank(),axis.title.x = element_blank())

grid.arrange(b,p,s,w, nrow=1, ncol=4)
?grid.arrange

df.mb.age <- data.frame(age = c("18 - 24 years", "25 - 29 years"), 
                           battery_mean = c(mean(data$RelImp_battery[data$Age=="2"]), mean(data$RelImp_battery[data$Age=="3"])))
df.mb.age


df.mp.age <- data.frame(age = c("18 - 24 years", "25 - 29 years"), 
                        price_mean = c(mean(data$RelImp_price[data$Age=="2"]), mean(data$RelImp_price[data$Age=="3"])))
df.mp.age


df.ms.age <- data.frame(age = c("18 - 24 years", "25 - 29 years"), 
                        sound_mean = c(mean(data$RelImp_sound[data$Age=="2"]), mean(data$RelImp_sound[data$Age=="3"])))
df.ms.age


df.mw.age <- data.frame(age = c("18 - 24 years", "25 - 29 years"), 
                        weight_mean = c(mean(data$RelImp_weight[data$Age=="2"]), mean(data$RelImp_weight[data$Age=="3"])))
df.mw.age


means.age <- data.frame(age = c("18 - 24 years", "25 - 29 years"), battery = c(df.mb.age$battery_mean), 
                           price= c(df.mp.age$price_mean),
                           sound = c(df.ms.age$sound_mean),
                           weight = c(df.mw.age$weight_mean))
means.age


#Intent to buy & Own 

sum(data$Own) #234 -> 506-234 =  272 haben keine!
sum(data$Gender[data$Gender=="1"])#230 Frauen in der GG (276 Männer)
sum(data$Own[data$Gender=="1"]) #90 von 230 der Frauen besitzen welche = 39,13% (149 besitzen keine)
sum(data$Own[data$Gender=="2"]) #144 von 276 der Männer besitzen welche = 52,17% (132 besitzen keine)

sum(data$Own[data$Age=="2"|data$Age=="3"]) #191 -> 43 Ü29 haben einen 
sum(data$Own[(data$Age=="2"|data$Age=="3") & data$Gender=="1"]) #79
sum(data$Own[(data$Age=="2"|data$Age=="3") & data$Gender=="2"]) #112 


sum(data$IntentToBuy) #174
sum(data$IntentToBuy[data$Own=="0"]) #116 -> von den x die keine haben wollen nur 116 welche kaufen 
sum(data$IntentToBuy[data$Own=="1"]) #58 Leute wollen neue 

sum(data$IntentToBuy[data$Own=="0" & data$Gender=="1"]) #57 Frauen die noch keine haben (von ) wollen welche kaufen ()
sum(data$IntentToBuy[data$Own=="0" & data$Gender=="2"]) #59 Männer 

sum(data$IntentToBuy[data$Own=="1" & data$Gender=="1"]) #19 von den 
sum(data$IntentToBuy[data$Own=="1" & data$Gender=="2"]) #39 von den

sum(data$IntentToBuy[data$Age=="2"|data$Age=="3"]) #151
sum(data$IntentToBuy[(data$Age=="2"|data$Age=="3") & data$Own=="0"]) #100
sum(data$IntentToBuy[(data$Age=="2"|data$Age=="3") & data$Own=="1"]) #51



#worauf legen die Leute die kaufen wollen wert hinsichtlich preis...? 

df.buy <-  data.frame( property = c("battery", "price", "sound", "weight"), mean = c(mean(data$RelImp_battery[data$IntentToBuy=="1"]),
                  mean(data$RelImp_price[data$IntentToBuy=="1"]),
                  mean(data$RelImp_sound[data$IntentToBuy=="1"]),
                  mean(data$RelImp_weight[data$IntentToBuy=="1"])))
df.buy




## 2 Seiten tiefere Analyse 
#sk und pii - wie sieht es mit intent to buy / not intent to buy leuten? 


## gender

# SubKnow
mean(sk$SKMean[sk$Gender=="1"])
mean(sk$SKMean[sk$Gender=="2"])

# PII
mean(pii$PIIMean[pii$Gender=="1"])
mean(pii$PIIMean[pii$Gender=="2"])

## age 

# SubKnow
mean(sk$SKMean[sk$Age=="2"])
mean(sk$SKMean[sk$Age=="3"])
mean(sk$SKMean[sk$Age=="8"])

# PII
mean(pii$PIIMean[pii$Age=="2"])
mean(pii$PIIMean[pii$Age=="3"])
mean(pii$PIIMean[pii$Age=="8"])


## Subjective Knowledge 

df.sk <- data.frame(data$id, data$Gender, data$Age, data$IntentToBuy, data$SubjKnow_r1, data$SubjKnow_r2, data$SubjKnow_r3, data$SubjKnow_r4, data$SubjKnow_r5)
df.sk
sk <- data.frame(ID = df.sk[,1], Gender = df.sk[,2], Age = df.sk[,3], IntentToBuy = df.sk[,4], SKMean = rowMeans(df.sk[,-(1:4)]))
sk

mean(sk$SKMean) #3.83 -> mean wert über alle Leute was das Subjective Knowledge angeht 

# Gruppe 1: Low subjective Knowledge: Werte von 1-3
group1 <- data.frame(ID = sk$ID[sk$SKMean <= 3], Gender = sk$Gender[sk$SKMean <= 3], Age = sk$Age[sk$SKMean <= 3],IntentToBuy = sk$IntentToBuy[sk$SKMean <= 3], Mean =sk$SKMean[sk$SKMean <= 3])
group1

# Gruppe 2: medium subjective knowledge 3-5
group2 <- data.frame(ID = sk$ID[sk$SKMean >3 & sk$SKMean <= 5], Gender = sk$Gender[sk$SKMean >3 & sk$SKMean <= 5], Age = sk$Age[sk$SKMean >3 & sk$SKMean <= 5],IntentToBuy = sk$IntentToBuy[sk$SKMean >3 & sk$SKMean <= 5], Mean =sk$SKMean[sk$SKMean >3 & sk$SKMean <= 5])
group2

# Gruppe 3: high subjective knowledge 5-7 
group3 <- data.frame(ID = sk$ID[sk$SKMean > 5], Gender = sk$Gender[sk$SKMean > 5], Age = sk$Age[sk$SKMean >5], IntentToBuy = sk$IntentToBuy[sk$SKMean >5], Mean =sk$SKMean[sk$SKMean > 5])
group3

view(group3) #111 Leute schätzen sich mit high knowledge ein 

sum(group3$Gender[group3$Gender=="1"]) #22
sum(group3$Gender[group3$Gender=="2"]) #89 are men 


# intent to buy among sk groups

g3.buy <- sum(group3$IntentToBuy) #42 from group 3 wanna buy
g2.buy <- sum(group2$IntentToBuy) #81 from group 2 wanna buy
g1.buy <- sum(group1$IntentToBuy) #51 from g1 


df.gender.g3 <-  data.frame(gender = c("female", "male"), ITB3= c(sum(group3$IntentToBuy[group3$Gender=="1"]),sum(group3$IntentToBuy[group3$Gender=="2"])))
df.gender.g3

df.gender.g2 <-  data.frame(gender = c("female", "male"), ITB2= c(sum(group2$IntentToBuy[group2$Gender=="1"]),sum(group2$IntentToBuy[group2$Gender=="2"])))
df.gender.g2

df.gender.g1 <-  data.frame(gender = c("female", "male"), ITB1= c(sum(group1$IntentToBuy[group1$Gender=="1"]),sum(group1$IntentToBuy[group1$Gender=="2"])))
df.gender.g1

df <- data.frame(Gender = c("female", "male"), Group1 = c(df.gender.g1$ITB1), Group2 = c(df.gender.g2$ITB2), Group3 = c(df.gender.g3$ITB3))
df 

G1 <- ggplot(data = df, aes(Gender, Group1))+ geom_point(shape = 10, size=3) + ylim(0,50) + labs(title = "Intention to buy Group 1", y = "Frequency")+ theme(plot.title =  element_text(size = 12),axis.title.y =element_text(size=12), axis.title.x = element_blank())
G2 <- ggplot(data = df, aes(Gender, Group2))+ geom_point(shape = 10, size=3) + ylim(0,50) + labs(title = "Intention to buy Group 2",y = "Frequency")+ theme(plot.title =  element_text(size = 12),axis.title.y =element_text(size=12), axis.title.x = element_blank())
G3 <- ggplot(data = df, aes(Gender, Group3))+ geom_point(shape = 10, size=3) + ylim(0,50) + labs(title = "Intention to buy Group 3", y = "Frequency")+ theme(plot.title =  element_text(size = 12),axis.title.y =element_text(size=12), axis.title.x = element_blank())

grid.arrange(G1,G2,G3, nrow=1, ncol=3)


# die werte in combi mit den property werten am anfang analysieren 
# worauf wird Wert gelegt was die properties angeht regarding alter und geschlecht 




## PII  - product category involvement aka personal relevance 

df.pii <- data.frame(data$id,data$Gender, data$Age,data$IntentToBuy, data$PII_1, data$PII_2, data$PII_3, data$PII_4, data$PII_5)
df.pii
pii <- data.frame(ID = df.pii[,1], Gender = df.pii[,2],  Age = df.sk[,3], IntentToBuy = df.sk[,4],  PIIMean = rowMeans(df.pii[,-(1:3)]))
pii

mean(pii$PIIMean) #3.65

# Gruppe 1: werte 1-3
group1_p <- data.frame(ID = pii$ID[pii$PIIMean <= 3], Gender = pii$Gender[pii$PIIMean <= 3], Age = pii$Age[pii$PIIMean <= 3],IntentToBuy = pii$IntentToBuy[pii$PIIMean <= 3], Mean =pii$PIIMean[pii$PIIMean <= 3])
group1_p

# Gruppe 2: 3-5
group2_p <- data.frame(ID = pii$ID[pii$PIIMean >3 & pii$PIIMean <= 5], Gender = pii$Gender[pii$PIIMean >3 & pii$PIIMean <= 5], Age = pii$Age[pii$PIIMean >3 & pii$PIIMean <= 5],IntentToBuy = pii$IntentToBuy[pii$PIIMean >3 & pii$PIIMean <= 5], Mean =pii$PIIMean[pii$PIIMean >3 & pii$PIIMean <= 5])
group2_p

# Gruppe 3: 5-7 
group3_p <- data.frame(ID = pii$ID[pii$PIIMean > 5], Gender = pii$Gender[pii$PIIMean > 5], Age = pii$Age[pii$PIIMean > 5],IntentToBuy = pii$IntentToBuy[pii$PIIMean > 5], Mean =pii$PIIMean[pii$PIIMean > 5])
group3_p

#intent to buy 
G3_ITB <- sum(group3_p$IntentToBuy) #21
G3_ITB_age <- sum(group3_p$IntentToBuy[(group3_p$Age=="2" | group3_p$Age=="3" )]) #15
G3_ITB_f <- sum(group3_p$IntentToBuy[(group3_p$Age=="2" | group3_p$Age=="3" ) & group3_p$Gender=="1"]) #5
G3_ITB_m <- sum(group3_p$IntentToBuy[(group3_p$Age=="2" | group3_p$Age=="3" ) & group3_p$Gender=="2"]) #10

G2_ITB <- sum(group2_p$IntentToBuy) #133 --> marketing peeps sollten sich auf dieses Involvement level einstellen, how to increase the involvement (maybe thru creating a more emotional ad?)
G2_ITB_age <-  sum(group2_p$IntentToBuy[group2_p$Age=="2" | group2_p$Age=="3"]) #119 -> es sind voarllem 19-29 age - marketing auf diese Zielgruppe
G2_ITB_f <- sum(group2_p$IntentToBuy[(group2_p$Age=="2" | group2_p$Age=="3") & group2_p$Gender=="1"]) #57
G2_ITB_m <- sum(group2_p$IntentToBuy[(group2_p$Age=="2" | group2_p$Age=="3") & group2_p$Gender=="2"]) #62

G1_ITB <- sum(group1_p$IntentToBuy) #20
G1_ITB_age <- sum(group1_p$IntentToBuy[(group1_p$Age=="2" | group1_p$Age=="3" )]) #17
G1_ITB_f <- sum(group1_p$IntentToBuy[(group1_p$Age=="2" | group1_p$Age=="3" ) & group1_p$Gender=="1"])  #7 
G1_ITB_m <- sum(group1_p$IntentToBuy[(group1_p$Age=="2" | group1_p$Age=="3" ) & group1_p$Gender=="2"])  #10 

# girl bastel einfach selbst ne tabelle in word