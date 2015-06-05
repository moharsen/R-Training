cuPlot2<- function(x1,y1,decile=0.1,title1="",subtitle1="")
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
  odds=defaultRate/(1-defaultRate);
  logodds=log(odds);
  
  names=quantiles;
  names1=paste("<",quantiles[1],sep="");
  names=c(names1,quantiles[1:length(quantiles)-1])
  names[length(names)]=paste(">=",quantiles[length(quantiles)-1],sep="");
  
  
  plot(quantiles,logodds,xlab=title1,main="Log  odds relationship",sub=subtitle1)
  x=cbind(count,defaultCount,defaultRate)
  
  row.names(x)<-names;
  x
}