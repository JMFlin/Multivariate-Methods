
###  DESCRIPTION AND PLOTTING TOOLS  #####


stand<-function(x) {(x-mean(x))/sd(x)}

Q1<-function(x)
   {quantile(x,0.25)}

Q2<-function(x)
   {quantile(x,0.50)}

Q3<-function(x)
   {quantile(x,0.75)}

b1<-function(x) 
   {n<-length(x)
    z<-stand(x)
    b1<-mean(z**3)
    p<-pchisq(1,n*b1*b1/6)
    list(sqrt_b1=b1,p=p)}


b2<-function(x) 
   {n<-length(x)
    z<-stand(x)
    b2<-mean(z**4)
    p<-pchisq(1,n*(b2-3)**2/24)
    list(b2=b2, p=p)}
 
hist.with.normal<-function(y, main = NULL)
#    Dalgaard(2003)
    {m<-mean(y)
     s<-sd(y)
     hist(y, freq=F, main = main)
     density<-function(x) dnorm(x,m,s)
     curve(density, add=TRUE)}

boxplot.with.normal<-function(x, main = NULL)
     {m<-mean(x)
     s<-sd(x)
     n<-length(x)  
     x.new<-c(x,rnorm(n,m,s))
     w<-c(rep(1,n),rep(2,n))
     boxplot(x.new ~ w, names=c("real data","normal data"), main = main)}




bivariateplot<-function(x,y, col = "black")
{
 xl<-min(x)-0.1*(max(x)-min(x))
 xu<-max(x)+0.1*(max(x)-min(x))
 xlim=c(xl,xu)
 yl<-min(y)-0.1*(max(y)-min(y))
 yu<-max(y)+0.1*(max(y)-min(y))
 ylim=c(yl,yu)
 m1<-mean(x)
 m2<-mean(y)
 Sigma<-var(cbind(x,y))
 P<-eigen(Sigma)$vectors
 phi<-2*pi*c(1:100)/100
 r1<-sqrt(eigen(Sigma)$values[1])
 r2<-sqrt(eigen(Sigma)$values[2])
 x1<-P[1,1]*r1*cos(phi)+P[1,2]*r2*sin(phi)
 y1<-P[2,1]*r1*cos(phi)+P[2,2]*r2*sin(phi)
 plot(x,y,xlim=xlim,ylim=ylim, col=col)
 c1<-sqrt(qchisq(0.5,2))
 c2<-sqrt(qchisq(0.90,2))
 lines(c1*x1+m1,c1*y1+m2)
 lines(c2*x1+m1,c2*y1+m2)
}


bivariateplot2<-function(x,y,xlim,ylim, col = "black")
{ 
 m1<-mean(x)
 m2<-mean(y)
 Sigma<-var(cbind(x,y))
 P<-eigen(Sigma)$vectors
 phi<-2*pi*c(1:100)/100
 r1<-sqrt(eigen(Sigma)$values[1])
 r2<-sqrt(eigen(Sigma)$values[2])
 x1<-P[1,1]*r1*cos(phi)+P[1,2]*r2*sin(phi)
 y1<-P[2,1]*r1*cos(phi)+P[2,2]*r2*sin(phi)
 plot(x,y,xlim=xlim,ylim=ylim, col=col)
 c1<-sqrt(qchisq(0.5,2))
 c2<-sqrt(qchisq(0.90,2))
 lines(c1*x1+m1,c1*y1+m2)
 lines(c2*x1+m1,c2*y1+m2)
}


bvn<-function(n,m1,m2,s11,s22,s12)
{z1<-rnorm(n)
 z2<-rnorm(n)
 S<-matrix(c(s11,s12,s12,s22),nrow=2)
 U<-eigen(S)$vectors
 D<-diag(eigen(S)$values**(1/2))
 A<-U%*%D
 cbind(z1,z2)%*%t(A)+cbind(c(rep(m1,n)),c(rep(m2,n)))} 



rmixture<-function(n,m1,m2,s1,s2,p)
{z<-rnorm(n)
 b<-rbinom(n,1,p)
 x<-m1+b*(m2-m1)+(s1+b*(s2-s1))*z
 x}

    






#### HOTELLING'S TEST
#### TWO INDePENDENT SAMPLES FROM p-VARIATE DISTRIBUTIONS 


Hotelling2<-function(X1,X2)
{X1<-as.matrix(X1)
 X2<-as.matrix(X2)
 n1<-nrow(X1)
 n2<-nrow(X2)
 p<-ncol(X1)
 m1<-apply(X1,2,mean)
 S1<-var(X1)
 m2<-apply(X2,2,mean)
 S2<-var(X2)
 S<-((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
# if (p>1) {mah<-t(m1-m2)%*%solve(S)%*%(m1-m2)}
# if (p==1) {mah<-(m1-m2)**2/S}
 mah<-t(m1-m2)%*%solve(S)%*%(m1-m2)
 T2<-n1*n2*mah/(n1+n2)
 p.value<-1-pchisq(T2,p)
 list(MahD=mah,T2=T2,df=p,p.value=p.value)}





MANOVA<-function(g,X)
{
p<-ncol(X)
n<-nrow(X)
gmax<-max(g)
m<-colMeans(X)
S<-matrix(rep(0,p*p),nrow=p)
for (i in 1:gmax)
{
ni<-sum(g==i)
Xi<-X[g==i,]
S<-S+(ni-1)*cov(Xi)
}
S<-S/(n-gmax)
Sinv<-solve(S)
Q<-0
for (i in 1:gmax)
{
Xi<-X[g==i,]
mi<-colMeans(Xi)
ni<-sum(g==i)
Q<-Q+ni*t(mi-m)%*%Sinv%*%(mi-m)
}
df<-(gmax-1)*p
p.value=1-pchisq(Q,df)
list(Q=Q,df=df,p.value=p.value)
}





