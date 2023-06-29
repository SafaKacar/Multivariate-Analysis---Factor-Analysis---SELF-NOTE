library("gamlss.data")

data(usair)
usair
pairs(usair[,-1])
cor(usair[,-1])

eigen(cor(usair[,-1]))

usairpc<-princomp(usair[,-1],cor=TRUE)
summary(usairpc,loadings=TRUE)
screeplot(usairpc,type="l")

par(pty="s")
plot(usairpc$scores[,1],usairpc$scores[,2],
ylim=range(usairpc$scores[,1]),xlab = "PC1",ylab="PC2",type="n")
text(usairpc$scores[,1],usairpc$scores[,2],
labels = abbreviate(row.names(usair)),cex=0.7,lwd=2)


usair_reg=lm(y~usairpc$scores,data=usair)
summary(usair_reg)

usair_reg=lm(y~usairpc$scores[,1]+usairpc$scores[,4]+usairpc$scores[,5]+usairpc$scores[,6],data=usair)
summary(usair_reg)

library("cluster.datasets")
data(life.expectancy.1971)

lif=read.table("life.txt")
lif
life=lif[,2:9]
row.names(life)<-lif$V1
names(life)=c("m0","m25","m50","m75","w0","w25","w50","w75")   # assign variable names
attach(life)

life.fa1<-factanal(life,factors=1,method="mle")
life.fa1

life.fa2<-factanal(life,factors=2,method="mle")
life.fa2

life.fa3<-factanal(life,factors=3,method="mle")
life.fa3


life.fa<-factanal(life,factors=3,method="mle",rotation="promax")
life.fa
scores<-factanal(life,factors=3,method="mle",scores="regression")$scores
scores

lifex<-data.frame(life,scores)
attach(lifex)

#In order to use “scatterplot3d” function, we should install the “Hmisc” package:

scatterplot3d(Factor1,Factor2,Factor3,type="n")
text(Factor1,Factor2,Factor3,labels= row.names(life),cex=0.7)

