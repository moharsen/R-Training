#set working directory
setwd("C://Users//Mohar//Desktop//LC/")

#Import datasets to R
build = read.csv(file = "case1.csv",header = TRUE, sep =",")
test = read.csv(file = "case2.csv",header = TRUE, sep =",")

#Vuiew the structure of the dataset
str(build) #Advantage vs. proc contents: you know the number of levels
summary(build) 

c#Attach dataset for convenience
attach(build)
#For categorical variables, see table()
table(build$home_ownership)

#Univariate analysis:continuous variables
#Look at FICO distribution
summary(fico)
#NA present. replace by median
#Missing value treatment. GG
fico[(is.na(fico))]=median(fico,na.rm=TRUE)
hist(fico)


#It seems there are some outliers
boxplot(fico)
quantile(fico,probs = seq(0,1,0.05))

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

floor <-function(x,floor){
  
  y=x;
  y[x<floor]=floor;
  
  y
}

fico=cap(fico,600)
summary(fico)

#data cleaning and treatment
# 
# for(i in 1:ncol(build)){
#   if(is.numeric(build[,i])=="TRUE")  
#   {high = quantile(build[i],probs = 0.95,na.rm=T)
#    low = quantile(build[i],probs = 0.05,na.rm=T)
#    build[i]=cap(build[i],high)
#    #     build[i]=floor(build[i],low)    
#    build[(is.na(build[i])), i]=median(build[,i],na.rm=TRUE)
#    
#   }
  

h <- hist(fico,  # Save histogram as object
          breaks = ,  # "Suggests" 11 bins
          #           breaks = seq(400, 700, by = 100),
          #           breaks = c(400, 600, 700)
          freq = FALSE,
          col = "thistle1", # Or use: col = colors() [626]
          main = "Histogram of fico scores of customers",
          xlab = "fico-scores")


# IF freq = FALSE, this will draw normal distribution
curve(dnorm(x, mean = mean(fico), sd = sd(fico)), 
      col = "thistle4", 
      lwd = 2,
      add = TRUE)
rug(fico,col="red")


