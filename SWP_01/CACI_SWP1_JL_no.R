
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