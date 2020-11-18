
mean(data$Age, na.rm = TRUE)

lowbudget=subset.data.frame(data,Avg_Budget==1)
str(lowbudget)

n=0

citynames = colnames(data[,4:23])


citynames

berlinlowb=citynames[,1],sum[lowbudget$citynames[1]]





berlinlb=sum(lowbudget$Berlin)

berlinlb

lbvisit=data.frame
parislb=sum(lowbudget$Paris)
londonlb=sum(lowbudget$London)
barcelonalb=sum(lowbudget$Barcelona)
madridlb=sum(lowbudget$Madrid)
romelb=sum(lowbudget$Rome)
stockholmlb=sum(lowbudget$Stockholm)
amsterdamlb=sum(lowbudget$Amsterdam)
praguelb=sum(lowbudget$Prague)
budapestlb=sum(lowbudget$Budapest)
lisbonlb=sum(lowbudget$Lisbon)
brusselslb=sum(lowbudget$Brussels)
viennalb=sum(lowbudget$Vienna)
stpetersburglb=sum(lowbudget$StPetersburg)
krakowlb=sum(lowbudget$Krakow)
rigalb=sum(lowbudget$Riga)
istanbullb=sum(lowbudget$Istanbul)
genevalb=sum(lowbudget$Geneva)
athenslb=sum(lowbudget$Athens)
dublinlb=sum(lowbudget$Dublin)


lbcount=data.frame(citynames)
lbcount[2,]=berlinlb
citynames
str(citynames)
colnames(citynames)=c("Cityname")
citynames2
str(citynames)
citynames$Testcol=c("test1")
citynames
str(citynames)


df = data.frame(city = citynames,
                visits = c(berlinlb, parislb, londonlb, barcelonalb, madridlb, romelb, stockholmlb, amsterdamlb, praguelb, budapestlb,
                           lisbonlb, brusselslb, viennalb, stpetersburglb, krakowlb, rigalb, istanbullb, genevalb, athenslb, dublinlb))

df$city = factor(df$city, levels = df$city[order(-df$visits)])
ggplot(data = df, aes(city, visits)) +
  geom_col(color = "black", fill = "grey") +
  labs(title = "Number of city visits of lowbudget travellers", y = "number of visits") +
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  theme(text = element_text(size=10),
        plot.title = element_text(size = 11, hjust = 0.46), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1.1, vjust = 1.2),
        axis.title.x = element_blank())


