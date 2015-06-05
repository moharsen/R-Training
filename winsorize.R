winsorize<-function(x,floor,cap){

 y=x;
 y[x<floor]=floor;
 y[x>cap]=cap;

 y
}

cap<-function(x,cap){

 y=x;
 y[x>cap]=cap;

 y
}



winsorizeAll<-function(data,floor,cap){
wData=data;

len=length(data[1,]);

 for (i in 1:len){

   wData[wData[,i]<floor[i]& !is.na(wData[,i]),i]=floor[i];
   wData[wData[,i]>cap[i] & !is.na(wData[,i]),i]=cap[i];

 }
wData

}

winsorizeLabel<-function(data,params){
wData=data;
len=length(wData[1,]);
names1=names(wData);
for (i in 1:len){
 
   print(i);
   wData[wData[,i]<params[names1[i],"Floor"] & !is.na(wData[,i]),i]=params[names1[i],"Floor"];
   wData[wData[,i]>params[names1[i],"Cap"] & !is.na(wData[,i]),i]=params[names1[i],"Cap"]
    
   wData[,i]=as.double(wData[,i]);
 }
  names1=paste(names(data),"w",sep="");
  mData=data.frame(wData);
  names(mData)=names1;

  mData;
}


pow<-function(x,y){

z=x^y;
z
}
 


nplot<-function(x,y,floor,cap,var)
{

x1=x[x<=cap & x>=floor];
y1=y[x<=cap & x>=floor];
plot(x1,y1)
}

DRplot1<-function(data,varname,decile=0.1){

x1=data[,varname];
y1=data$StressInd;
DRplot(x1,y1,decile,varname,"")
}

DRplot<-function(x1,y1,decile=0.1,title1="",subtitle1="")
{
if(is.na(decile)) decile=0.1;
data=cbind(x1,y1);

deciles=seq(0,1,decile);
x=data[,1];
quantiles=quantile(x,probs=deciles,na.rm=TRUE);
defaultCount=matrix(0.0,length(deciles)-2);

y=na.omit(data);

for (i in 2:length(deciles)){

 defaultCount[i-1]=length(which(y[y[,1]<quantiles[i] & y[,1]>=quantiles[i-1],2]==1));

}

defaultRate=defaultCount/length(y[,1])

plot(quantiles[1:length(quantiles)-1],defaultRate,xlab=title1,main="Empirical Distribution",sub=subtitle1)
cbind(quantiles[1:length(quantiles)-1],defaultCount,defaultRate)

}

negativeSplitter<-function(data,vname)
{
zeroDat=rep(0,length(data[,vname]))
wData=cbind(data[,vname],zeroDat);

mean1=mean(data[,vname],na.rm=TRUE)

wData[wData[,1]<0,2]=1;
wData[wData[,1]<0,1]=mean1;

colnames(wData)=c(paste(vname,"_neg",sep=""),paste(vname,("_negInd"),sep=""));
data.frame(wData)

}

negativeSplitter1<-function(data,vname)
{
zeroDat=rep(0,length(data))
wData=cbind(data,zeroDat);

wData[wData[,1]<0,2]=1;
wData[wData[,1]<0,1]=0;

colnames(wData)=c(paste(vname,"_neg",sep=""),paste(vname,("_negInd"),sep=""));
data.frame(wData)

}

thresSplitter<-function(data,vname,threshold)
{
zeroDat=rep(0,length(data))
wData=cbind(data,zeroDat);

wData[wData[,1]<threshold,2]=1;
wData[wData[,1]<threshold,1]=0;

colnames(wData)=c(paste(vname,"_t",sep=""),paste(vname,("_tInd"),sep=""));
data.frame(wData)
}

negativeSplitterRep<-function(data,replace,vname)
{
zeroDat=rep(0,length(data))
wData=cbind(data,zeroDat);

if(replace==1)
mean1=mean(data,na.rm=TRUE)
else
mean1=0;

wData[wData[,1]<0,2]=1;
wData[wData[,1]<0,1]=mean1;

colnames(wData)=c(vname,"negInd");
data.frame(wData)

}

negShift<-function(data,floor){
#remember floor is negative
data=(data-floor+1);
data
}

negShift1<-function(data){
#remember floor is negative
data=(data-min(data,na.rm=TRUE)+1);
data
}



LOplot<-function(x1,y1,decile,title1,subtitle1)
{
if(is.na(decile)) decile=0.1;
data=cbind(x1,y1);

deciles=seq(0,1,decile);
x=data[,1];
quantiles=quantile(x,probs=deciles,na.rm=TRUE);
defaultCount=matrix(0.0,length(deciles)-2);

y=na.omit(data);

for (i in 2:length(deciles)){

 defaultCount[i-1]=length(which(y[y[,1]<quantiles[i] & y[,1]>=quantiles[i-1],2]==1));

}

defaultRate=defaultCount/length(y[,1])
defaultRate[defaultCount!=0]=log(defaultRate[defaultCount!=0]/(1-defaultRate[defaultCount!=0]))



plot(quantiles[1:length(quantiles)-1],defaultRate,main=title1,sub=subtitle1)
cbind(quantiles[1:length(quantiles)-1],defaultCount,defaultRate)

}


cuPlot<- function(x1,y1,decile=0.1,title1="",subtitle1="")
{

if(is.na(decile)) 
	decile=0.1;
	data=cbind(x1,y1);

deciles=seq(0,1,decile);
deciles[1]=0.01;

x=data[,1];

quantiles=quantile(x,probs=deciles,na.rm=TRUE);
defaultCount=matrix(0.0,length(deciles));
count=matrix(0.0,length(deciles));

y=na.omit(data);
defaultCount[1]=length(which(y[y[,1]<=quantiles[1],2]==1));
count[1]=length(y[y[,1]<=quantiles[1],2]);

i=2;
defaultCount[i]=length(which(y[y[,1]<quantiles[i],2]==1))-length(which(y[y[,1]<=quantiles[i-1],2]==1));
count[i]=length(y[y[,1]<quantiles[i] & y[,1]>quantiles[i-1],2]);


for (i in 3:length(deciles)){

 defaultCount[i]=length(which(y[y[,1]<quantiles[i],2]==1))-length(which(y[y[,1]<quantiles[i-1],2]==1));
 count[i]=length(y[y[,1]<quantiles[i] & y[,1]>=quantiles[i-1],2]);

}

i=length(deciles);
defaultCount[i]=length(which(y[y[,1]>=quantiles[i-1],2]==1));
count[i]=length(y[y[,1]>=quantiles[i],2]);


defaultRate=defaultCount/count;
names=quantiles;
names1=paste("<",quantiles[1],sep="");
names=c(names1,quantiles[1:length(quantiles)-1])
names[length(names)]=paste(">=",quantiles[length(quantiles)-1],sep="");


plot(quantiles,defaultRate,xlab=title1,main="Empirical Distribution",sub=subtitle1)
x=cbind(count,defaultCount,defaultRate)

row.names(x)<-names;
x
}
 




 cuPlot1<- function(x1,y1,decile=0.1,title1="",subtitle1="")
{

if(is.na(decile)) 
decile=0.1;
data=cbind(x1,y1);

deciles=seq(0,1,decile);
deciles[1]=0.01;

x=data[,1];

quantiles=quantile(x,probs=deciles,na.rm=TRUE);
defaultCount=matrix(0.0,length(deciles));
count=matrix(0.0,length(deciles));

y=na.omit(data);
defaultCount[1]=length(which(y[y[,1]<=quantiles[1],2]==1));
count[1]=length(y[y[,1]<=quantiles[1],2]);
for (i in 2:length(deciles)){

 defaultCount[i]=length(which(y[y[,1]<=quantiles[i],2]==1))-length(which(y[y[,1]<=quantiles[i-1],2]==1));
 count[i]=length(y[y[,1]<=quantiles[i] & y[,1]>quantiles[i-1],2]);

}

defaultRate=defaultCount/count;

plot(quantiles,defaultRate,xlab=title1,main="Empirical Distribution",sub=subtitle1)
cbind(quantiles,count,defaultCount,defaultRate)

}
 


dist<-function(x,step=0.1,title1=""){

q1=quantile(x,probs=seq(0,1,step),na.rm=TRUE);
q1
}

somersD<-function(x,y,z=0,na.rm=FALSE){

if (na.rm){
  q=somers2(x[-which(is.na(z))],y[-which(is.na(z))]);
 }
 else
  q=somers2(x,y,na.rm=TRUE)

q
}

inverse<-function(x,cap=9999,floor=-9999){
	y=1/x;
	y[x==0]=1/min(x[x>0],na.rm=TRUE);
	if (cap!=9999) { y[y>cap]=cap;}
        if (floor!=-9999) { y[y<floor]=floor;}
	y
}

binFCC3<-function(rf){

# DR=c(0.1299,0.1095,0.1012,0.0977,0.0942,0.0607,0.0479,0.0422,0.0290,0.0244,0.0218,0.0206,0.0194);
# bins=c(-3.89,0,0.45,0.85,1,1.79,2.43,3.28,4.25,6,8,11); 
  bins=c(-3.89,-1,0,0.5,0.75,1,1.25,1.5,1.75,2,2.4,2.8,3.4,4,5,6,8,11);


# logOdds= log((DR/(1-DR))); 
logOdds=c(-1.912011632,-2.034623233,-2.147333038,-2.251161901,-2.327001411,-2.403463932,-2.574365575,-2.672686986,-2.781173986,-2.923110157,-3.081455179,
-3.211981439,-3.253186574,-3.44100855,-3.648582047,-3.73483832,-3.841965488,-3.946859448,-3.987415366);

 binning(rf,bins,logOdds);

}




binFCC4<-function(rf){
 # Solver optimization- See FCC4-Final
  bins=c(0,0.45,0.8,1.1,1.6,2,2.25,2.9,3.5,4.1,4.9,6,8,12);


# logOdds= log((DR/(1-DR))); 
logOdds=c(-2.060688301,-2.202685223,-2.342077475,-2.475852368,-2.648291461,-2.825610009,-3.006078377,-3.134773326,-3.208588044,-3.311449414,
-3.390836487,-3.445832788,-3.51868808,-3.684925645,-3.98267426);

 binning(rf,bins,logOdds);

}


binFCC4A<-function(rf){
  # Solver optimization- See FCC4-Final-1
  bins=c(0,0.45,0.8,1.1,1.6,1.9,2.4,2.97,3.5,4.1,4.9,6,8,12);


# logOdds= log((DR/(1-DR))); 
logOdds=c(-2.058388132,-2.202043864,-2.389199566,-2.397895273,-2.680514043,-2.692546015,
-3.128782781,-3.135494216,-3.152319008,-3.362976169,-3.433987204,-3.479039868,-3.493421139,
-3.515269838,-4.11414719);

 binning(rf,bins,logOdds);

}

binFCC4B<-function(rf){
 
 # Montonic - see FCC4-BinFCC4B tab
  bins=c(0,0.43,0.79,1.04,1.64,2,2.35,2.97,3.42,4.1,4.9,6,8,12);


# logOdds= log((DR/(1-DR))); 
logOdds=c(-2.058388132,-2.292534757,-2.302585093,-2.319114395,-2.573702149,-3.070164868,-3.140313502,
-3.141686186,-3.183248647,-3.277144733,-3.433987204,-3.479039868,-3.481240089,-3.515269838,-4.11414719);

 binning(rf,bins,logOdds);

}
binFCC4C<-function(rf){
 # Same as 4B except FCC4w<1 , is set as 0. To be used with indicator variable for FCC4w<1
  bins=c(0,0.43,0.79,1,1.04,1.64,2,2.35,2.97,3.42,4.1,4.9,6,8,12);

logOdds=c(0,0,0,0,0,-2.319114395,-2.573702149,-3.070164868,-3.140313502,
-3.141686186,-3.183248647,-3.277144733,-3.433987204,-3.479039868,-3.481240089,-3.515269838,-4.11414719);


# logOdds= log((DR/(1-DR))); 

 binning(rf,bins,logOdds);

}

binFCC5<-function(rf){
 
  bins=c(-1,0,0.48,0.8,1,1.6,1.9,2.23,2.6,3.25,4,4.75,7,8,12);


# logOdds= log((DR/(1-DR))); 
logOdds=c(-1.99902649,-2.13796497,-2.228876393,-2.277427595,-2.354634214,-2.539872578,-2.762501133,
-2.959725567,-3.074031867,-3.17467809,-3.37288134,-3.585172755,-3.630637831,-3.736800212,-3.917018766,-4.067747986);

 binning(rf,bins,logOdds);

}




binFCC1<-function(rf){

bins=c(-3,0,0.4,0.8,1,1.4,1.75,2,2.25,3,3.5,4,5,6,7,9,11);
logOdds=c(-1.940594904,-2.045143961,-2.155009947,-2.258518745,-2.380911234,-2.584331639,-2.740686062,-2.857654467,-2.982153766,-3.112858834,
-3.274456964,-3.51428372,-3.663938673,-3.709428686,-3.749420571,-3.910834248,-4.399821268,-4.605438224);

binning(rf,bins,logOdds);
}

binFCC2<-function(rf){
bins=c(-4,0,0.5,0.8,1,1.25,1.5,1.75,2,2.4,2.8,3.4,4,5.5,6.5,8,11);
logOdds=c(-2.151591905,-2.186692925,-2.227423863,-2.249947232,-2.325397017,-2.488721748,-2.633547843,-2.818030666,-2.951693318,-2.973850186,-3.007627268,-3.142693234,
-3.318527553,-3.488927725,-3.677793969,-3.896209544,-4.221080302,-4.382732489);

binning(rf,bins,logOdds);
}

binGrowth<-function(rf){

bins=c(-0.20,-0.10,-0.05,0.00,0.02,0.05,0.07,0.10,0.12,0.15,0.20,0.25,0.30,0.50);

logOdds=c(-2.313707807,-2.436770337,-2.555479012,-2.681414614,-2.823815349,-3.014095052,-3.242261277,-3.45526249,-3.593337258,-3.622767553,
-3.495509379,-3.262431872,-2.975716793,-2.653626497,-2.307275828);

binning(rf,bins,logOdds);

}

binGrowthA<-function(rf){

bins=c(-0.2,-0.1,-0.06,0.02,0.04,0.065,0.1,0.14,0.19,0.25,0.35);

logOdds=c(-2.384497429,-2.450191898,-2.531608486,-2.633002081,-2.798455901,-3.093091576,-3.417539398,-3.648953649,-3.539269287,-3.237716414,-2.838856984,-2.413721605);

binning(rf,bins,logOdds);

}
binGrowthB<-function(rf){

bins=c(-0.2,-0.1,-0.06,0.02,0.04,0.065,0.1,0.14,0.19,0.25,0.35);

logOdds=c(-2.355694918,-2.509120908,-2.51999797,-2.68127996,-2.690350624,-2.866274206,-3.715602524,-3.837299459,-3.589059119,-3.115533473,-2.898882448,-2.36364535);

binning(rf,bins,logOdds);

}

binFCC3A<-function(rf){
# monotonous - see FCC3Final-binFCC3A tab
# DR=c(0.1299,0.1095,0.1012,0.0977,0.0942,0.0607,0.0479,0.0422,0.0290,0.0244,0.0218,0.0206,0.0194);
# bins=c(-3.89,0,0.45,0.85,1,1.79,2.43,3.28,4.25,6,8,11); 
  bins=c(-1.000001,0,0.45,0.86,1.01,1.3,1.6,2.1,2.4,3.1,3.75,4.5,6.05,8.8,11);


# logOdds= log((DR/(1-DR))); 
logOdds=c(-1.990610328,-2.122001156,-2.223541886,-2.327277706,-2.351375257,-2.584990108,-2.678449731,-2.904885801,-3.33729358,-3.351252705,
-3.061386244,-3.375879574,-3.789855371,-3.880089958,-4.027899463,-4.119037175);

 binning(rf,bins,logOdds);

}

binning<-function(rf,bins,logOdds){

 N=length(rf);
 Nbin= length(bins);

 rfScore=matrix(0,N,1);
 for(i in 1:N){
        if (is.na(rf[i])) {
	  rfScore[i]=NA;
	  next;
	}
	 rfScore[i]=logOdds[1]; # by Default, lowest bin. 
 
	 if(rf[i] > bins[1] & rf[i] < bins[2]) {
	   rfScore[i]=logOdds[2];
	   next;
	 }

	 if(rf[i] >= bins[length(bins)]) { # special case for > 11;
	   rfScore[i]=logOdds[length(logOdds)];
   	   next;
	 }
     
     for (j in 3:Nbin-1){
       if ((rf[i] >= bins[j]) & (rf[i] < bins[j+1])){
	  rfScore[i]=logOdds[j+1];
	  next;
       }
     }

 }

rfScore

}
