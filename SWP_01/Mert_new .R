PRS <- data.frame(Paris_romantic = data$Paris_Att17, relship = data$PartnershipStatus)        #Tried to check if single people and not single people would rate cities romanticness differently
PRS <- na.omit(PRS)
table(PRS)
PRS_r <- data.frame(PRS_r = c("single", "in a relationship"), agree= c(mean(PRS$Paris_romantic[PRS$relship == "single"]), mean(PRS$Paris_romantic[PRS$relship == "in a relationship."]))) 
plot12 <- ggplot(data = PRS_r, aes(PRS_r, agree)) + 
  labs(title = "Do you find Paris romantic?", 
       y = "Level of Agreement" , x= "relationship status")  + ylim(1,5) + geom_point(size = 5, shape = 10)
plot12


PRG <- data.frame(Prague_romantic = data$Prague_Att17, relship = data$PartnershipStatus)
PRG <- na.omit(PRG)
PRG
table(PRG)
data$PartnershipStatus<-as.factor(data$PartnershipStatus)

PRG_r <- data.frame(PRG_r = c("single", "in a relationship."), agree = c(mean(PRG$Prague_romantic[PRG$relship == "single"]), mean(PRG$Prague_romantic[PRG$relship == "in a relationship."])))
plot22 <- ggplot(data = PRG_r, aes(PRG_r, agree)) + 
  labs(title = "Do you find Prague romantic?", 
       y = "Level of Agreement" , x= "relationship status")  + ylim(1,5) + geom_point(size = 5, shape = 10)

PP <- grid.arrange(plot12, plot22, nrow=1, ncol=2)