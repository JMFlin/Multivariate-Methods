data1<-data.frame(read.table('bank.dat'))
 length<-data1[,1]
 left<-data1[,2]
 right<-data1[,3]
 bottom<-data1[,4]
 top<-data1[,5]
 diag<-data1[,6]
 names(data1)<-c('length','left','right','bottom','top','diag') 


data2<-data.frame(read.table('iris.dat'))
 species<-data2[,1]
 sepall<-data2[,2]
 sepalw<-data2[,3]
 petall<-data2[,4]
 petalw<-data2[,5]
 names(data2)<-c('species','sepall','sepalw','petall','petalw')


source("own_functions.r")




# SCATTER PLOTS
z<-c(rep(1,100),rep(2,100))
plot(data1,col=c("red","blue")[z])
plot(data2,col=c("red","blue","green")[species])

# SIMULATED NORMAL  DATA
x<-rnorm(500)
summary(x)
b1(x)
b2(x)
hist.with.normal(x)
boxplot.with.normal(x)


# SIMULATED DATA FROM A MIXTURE OF TWO NORMAL DISTRIBUTIONS
x<-rmixture(500,0,5,1,1,0.1)
summary(x)
b1(x)
b2(x)
hist.with.normal(x)
boxplot.with.normal(x)


#SIMULATING AND PLOTTING BIVARIATE DATA
X<-bvn(100,0,1,4,2,2)
bivariateplot(X[,1],X[,2])


# HOTELLING'S TEST AND MANOVA
MANOVA(c(rep(1,100),c(rep(2,100))),data1)
MANOVA(data2[,1],data2[,2:5])

# PRINCIPAL COMPONENT ANALYSIS
results1<-prcomp(data1)
summary(results1)
plot(results1)
PCdata1<-data.frame(as.matrix(data1)%*%results1$rotation)
plot(PCdata1,col=c(rep(1,100),rep(2,100)))


results2<-prcomp(data2)
summary(results2)
plot(results2)
PCdata2<-data.frame(as.matrix(data2)%*%results2$rotation)
plot(PCdata2,col=data2[,1])



#CANONICAL CORRELATION
#WHOLE SWISS BANK NOTES DATA SET
x<-data1[,1:3]
y<-data1[,4:6]
results<- cancor(x,y)
results
en<-c(rep(1,nrow(x)))
CCdata1x<-data.frame((as.matrix(x)-en%*%t(results$xcenter)) %*%results$xcoef)
CCdata1y<-data.frame((as.matrix(y)-en%*%t(results$ycenter))%*%results$ycoef)
CCData1<-cbind(CCdata1x,CCdata1y)
plot(CCData1)
cor(CCData1)

# GENUINE BILLS
x<-data1[1:100,1:3]
y<-data1[1:100,4:6]
results<- cancor(x,y)
results
en<-c(rep(1,nrow(x)))
CCdata1x<-data.frame((as.matrix(x)-en%*%t(results$xcenter)) %*%results$xcoef)
CCdata1y<-data.frame((as.matrix(y)-en%*%t(results$ycenter))%*%results$ycoef)
CCData1<-cbind(CCdata1x,CCdata1y)
plot(CCData1)
cor(CCData1)

