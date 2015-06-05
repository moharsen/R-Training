dat<- read.csv(".csv") # data given to us
datout<- read.csv(".csv") # data on which you need to validate.
wt=0.5 #[it depends on bank] the amount of importance given to if a person he is actually bad we are classifying him as good to if a person is actually good but classifying him as bad. 
dat<- read.csv("a.csv")
samp<- read.csv("train.csv")
samp=as.vector(samp[,2])
# First we create a new indicator variable whether a person is charged off
d=rep(0,nrow(dat))
d[which(dat$Status_2yrs=="Charged-off")]=1
dat<-cbind(dat,d)
 
k=vector()
k<- as.POSIXct(dat$app_date,format='%m/%d/%Y')
k=as.Date(k)
d=min(k)
d=rep(d,nrow(dat))
k=difftime(k,d,units="days")
l=vector()
l<- as.POSIXct(dat$earliest_cr_line,format='%m/%d/%Y')
l=as.Date(l)
e=min(l)
e=rep(e,nrow(dat))
l=difftime(l,e,units="days")
dat$app_date=k
dat$earliest_cr_line=l
dat$delinq_2yrs=dat$delinq_2yrs/dat$open_acc
dat$inq_last_6mths=dat$inq_last_6mths/dat$open_acc
ncol(dat)
# imputing missing values by median
for(i in 1:ncol(dat))
{
  if(is.numeric(dat[[i]])=="TRUE")
    { 
 m<- median(na.omit(dat[[i]]))
 dat[[i]][is.na(dat[[i]])==TRUE]<- m
  }
}
# creating new indicator variables for grade
 a=rep(0,nrow(dat))
a[which(dat$grade=="A")]=1
b=rep(0,nrow(dat))
b[which(dat$grade=="B")]=1
dt<- cbind(dat,a,b)
# model
dts<- dt[samp,]
lgs <- glm(d ~ loan_amnt+app_date+dti +inq_last_6mths + open_acc+pub_rec+revol_util+fico+a+b,data=dts,family="binomial")
summary(lgs)
#so we get the coefficients from lgs for fixed sample
#we observed that AIC is more or less same. so we continue with second model.
#to decide cutoff
pk<- predict(lgs,newdata=dt,type="response")
s=vector()
for(p in seq(0.01,1,0.01)){
  s[p*100]=(length(which(pk > p & dt$d==0))/length(which(dt$d==0)))+(length(which(pk <= p & dt$d==1))/length(which(dt$d==1)))
}
ct= 0.01*which(s==min(na.omit(s)))




# we need to clean the data to be validated on.
dat<- datout
d=rep(0,nrow(dat))
d[which(dat$Status_2yrs=="Charged-off")]=1
dat<-cbind(dat,d)

k=vector()
k<- as.POSIXct(dat$app_date,format='%m/%d/%Y')
k=as.Date(k)
d=min(k)
d=rep(d,nrow(dat))
k=difftime(k,d,units="days")
l=vector()
l<- as.POSIXct(dat$earliest_cr_line,format='%m/%d/%Y')
l=as.Date(l)
e=min(l)
e=rep(e,nrow(dat))
l=difftime(l,e,units="days")
dat$app_date=k
dat$earliest_cr_line=l
dat$delinq_2yrs=dat$delinq_2yrs/dat$open_acc
dat$inq_last_6mths=dat$inq_last_6mths/dat$open_acc
ncol(dat)
# imputing missing values by median
for(i in 1:ncol(dat))
{
  if(is.numeric(dat[[i]])=="TRUE")
  { 
    m<- median(na.omit(dat[[i]]))
    dat[[i]][is.na(dat[[i]])==TRUE]<- m
  }
}
# creating new indicator variables for grade
a=rep(0,nrow(dat))
a[which(dat$grade=="A")]=1
b=rep(0,nrow(dat))
b[which(dat$grade=="B")]=1
dtout<- cbind(dat,a,b)


pd<- predict(lgs,newdata=dtout,type="response")
head(pd)
choff<- rep(0,nrow(dtout))
choff[which(pd>ct[1])]<- 1 # this variable gives whether the customer will chargeoff withinn 2 yrs.

error1<- (length(which(pd > ct[1] & dtout$d==0))/length(which(dtout$d==0)))+(length(which(pd <= ct[1] & dtout$d==1))/length(which(dtout$d==1)))

error<- 0.5*error1
