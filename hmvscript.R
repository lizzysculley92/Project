z1<-read.table("hmvdata.txt", header=TRUE)
(length(z1[,3])-500):length(z1[,3])
z<-z1[,3]
par(mfrow=c(2,1))
hist(z, main="Histogram for HMV log returns 05/2002-05/2003 ", xlab="composite return")
e<-ecdf(z)
y=sort(z)
plot(y, type= "l",xlab="", ylab="Log Return")
abline(h=quantile(y,0.025), col="red")
quantile(z,0.025)
quantile(z,0.01)
source("historicvar.txt")
historic<-historic.var(z,250)
plot(z[250:(length(z1[,3]))], type="l", xlab="t", ylab="Daily Log return", main="HMV daily Log-returns 2003-2013 \n with the daily Value at risk caculated \n by the Monte Carlo simulation approach")
lines(historic, type="l", col="blue")
legend("topleft", ,c("Value at Risk on day t","Log return on day t"),lty=c(1,1),lwd=c(2.5,2.5),col=c( "red","black")) 


historic2<-historic.var(z,500)
historic3<-historic.var(z,750)
plot(historic[500:length(historic)], type="l", xlab="t",ylim=c(-0.25,0.1), ylab="Daily Log return", main="The daily Value at risk for HMV stocks calculated \n by the historical simulation approach \n with different values of N")
lines(historic2[250:length(historic2)], type="l", col="red")
lines(historic3, type="l", col="green")
legend("bottomleft", ,c("N=250","N=500", "N=750"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("black", "red","green")) 

day<-read.table("HMV5day.txt", header=TRUE)

#delta normal
x<-sqrt(var(z))
x
qnorm(0.025)*x

source("deltavar.txt")
deltaz<-delta.var(z,250)
delta<-deltaz*qnorm(0.025)
plot(z[500:(length(z1[,2]))], type="l", xlab="t", ylab="Daily Log return", main="HMV daily Log-returns 2003-2013 \n with the daily Value at risk caculated \n by all methods")
lines(delta[250:length(delta)], type="l", col="red")
legend("topleft", ,c("Value at Risk on day t","N=Log return on day t"),lty=c(1,1),lwd=c(2.5,2.5),col=c( "red","black")) 



w=seq(-4,4,length=200)
p=1/sqrt(2*pi*x)*exp(-w^2/(2*x))
plot(p, type="l")

 
y<-window.comp(z,500)
s<-window.comp(z,750)
plot(t,col="red", type="l", xlab="", ylab="Daily Log return", main="Daily value at risk for HMV 2003-2013")
lines(y, type="l", col="black")
lines(s, type="l", col="green")
y<-window.comp(z,750)
lines(y, type="l", col="black")

#bootstrap
z<-z1[,3]

l.B<-1000
q.b<-rep(0,l.B)
alpha<-0.025
lf<-250 
for (j in 1:l.B){
f.B<-sample(z,lf,replace=TRUE)
q.b[j]<-quantile(na.omit(f.B),alpha)}
mean(q.b)

window.var<-function(data,ws){
ll<-length(data)
lll<-ll-ws+1
bv<-rep(0,lll)

for (i in 1:lll)
{
#bootstrap
l.B<-1000
q.b<-rep(0,l.B)
alpha<-0.025
lf<-250
for (j in 1:l.B){
f.B<-sample(data[i:(ws+i-1)],lf,replace=TRUE)
q.b[j]<-quantile(na.omit(f.B),alpha)
}
bv[i]<-mean(q.b)
}
bootsr.var=bv
}
btsp<-window.var(z,500)

plot(z[500:(length(z1[,3]))], type="l", xlab="t", ylab="Daily Log return", main="HMV daily Log-returns May 2004- January 2013 \n with the daily Value at risk calculated \n by the Bootstrap method")
lines(btsp, col="red")
legend("topleft", ,c("Log return on day t","Value at Risk"),lty=c(1,1),lwd=c(2.5,2.5),col=c( "black", "red")) 







#Monte Carlo simulation
z<-z1[2454:2704,2]
pr.path<-function(mu,sigma,S0,K,D){
#K is the desired number of paths
#D is the length of the paths
#mu, sigma and S0 must be specified
delta.t<-1/D
price.paths<-matrix(0,D,K)
for (j in 1:K){
S<-S0
for (i in 1:D){
eps<-rnorm(1,0,1)
S<-S+S*(mu*delta.t+sigma*eps*sqrt(delta.t))
price.paths[i,j]<-S
}
}
price.paths
}
output<-pr.path(0,0.1,z[i],100,250)
quant<-quantile(output[250,], 0.025)
quant
log.return<-log(quant/z1[i])
log.return
log.return*1000000



#Monte Carlo simulation with moving window
z<-z1[500:2704,2]
pr.path<-function(mu,sigma,S0,K,D){
#K is the desired number of paths
#D is the length of the paths
#mu, sigma and S0 must be specified
delta.t<-1/D
price.paths<-matrix(0,D,K)
for (j in 1:K){
S<-S0
for (i in 1:D){
eps<-rnorm(1,0,1)
S<-S+S*(mu*delta.t+sigma*eps*sqrt(delta.t))
price.paths[i,j]<-S
}
}
price.paths
}
wmc.var<-function(data,ws){
lll<-length(data)
mcv<-rep(0,lll)
for (i in 1:lll)
{

output<-pr.path(0,0.1,z[i],1000,250)
quant<-quantile(output[250,], 0.025)
quant
log.return<-log(quant/z[i])
mcv[i]<-log.return
}
mcv
}
monte<-wmc.var(z,1)
plot(z[500:(length(z1[,3]))], type="l", xlab="t", ylab="Daily Log return", main="HMV daily Log-returns May 2004- January 2013 \n with the daily Value at risk calculated \n by the Monte Carlo simulation method")
lines(monte, col="red")
legend("topleft", ,c("Log return on day t","Value at Risk"),lty=c(1,1),lwd=c(2.5,2.5),col=c( "black", "red")) 




plot(z[500:(length(z1[,3]))], type="l", xlab="t", ylab="Daily Log return", main="HMV daily Log-returns May 2004- January 2013 \n with the daily Value at Risk calculated \n by all Value at Risk methods")
lines(historic[250:length(historic)], type="l", col="blue")
lines(delta[250:length(delta)], type="l", col="red")
lines(btsp, col="green")
lines(monte, col="orange")
legend("topleft", ,c("Log return on day t","Historical", "Delta Normal", "Bootstrap", "Monte Carlo"),lty=c(1,1,1,1,1),lwd=c(2.5,2.5,2.5,2.5,2.5),col=c( "black","blue", "red", "green", "orange")) 



mean(z)
var(z)
skewness(z)
kurtosis(z)


 
# kupiec
#find number of exceptions
y<-z1[250:length(z1[,3]),3]
test<-ifelse(y<delta, 1, 0)
table(test)



alpha<-0.025
T<-(length(z1[,3])-500)
#x is the number of exceptions
x<-83
x.t<-x/T
lr.top<- ((1-alpha)^(T-x))*(alpha^(x))
lr.bottom<-((1-x.t)^(T-x))*(x.t^(x))
lr.pof<-(-2)*log(lr.top/lr.bottom)
lr.pof
lr.top
lr.bottom

#christoffersen interval forcast
#find n00 and n11
source("windowcomp.txt")
s<-window.comp(test,1)
table(s)
#value for 0 is n00, value for 2 is n11

source("windowcomp2.txt")
h<-window.comp(test,1)
table(h)
#value for -1 is n01, value for 1 is n10

#Sub in tha values found above
n00<-2051
n01<-71
n10<-71
n11<-12

pi<-((n01+n11)/(n00+n01+n10+n11))
pi0<-(n01/(n00+n01))
pi1<-(n11/(n10+n11))
pi
pi0
pi1
lr.top<-((1-pi)^(n00+n10))*(pi^(n01+n11))
lr.bottom<-((1-pi)^(n00))*(pi0^(n01))*((1-pi1)^(n10))*(pi1^(n11))
lr.ind<-(2)*(log(lr.top/lr.bottom))
lr.ind
