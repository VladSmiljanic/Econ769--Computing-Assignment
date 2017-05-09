library(haven)
#source <- read_dta("~/Desktop/Vlad's Stuff/Vlad's School/Econ769- Computing Assignment/017769exercise.dta")
source <- read_dta("C:/Users/vlads_000/Documents/GitHub/Econ769--Computing-Assignment/017769exercise.dta")
require(plm)
require(boot)
tab<-data.frame(Model=c("Pooled","Between","Within","First Difference","Random Effect"))
resamp<-function(data,replace,size){ #Resampling function
  unique_ind<-unique(source$id) #Identifies the datas unique individuals
  samp_ind<-sample(unique_ind,size=size,replace=replace) #Randomly samples over those individuals
  do.call(rbind,lapply(samp_ind,function(x) source[source$id==x,])) #Returns data frame with those individuals observations over time.
}

mydata<-resamp(source,replace=FALSE,size=266)
alpha<-numeric()
beta<-numeric()
se<-numeric()
se.b<-numeric()

pooled<- plm(lnhr ~ lnwg, data = mydata, model = "pooling",index=c("id"))
between <- plm(lnhr ~ lnwg, data = mydata, model = "between",index=c("id"))
within <- plm(lnhr ~ lnwg, data = mydata, model = "within",index=c("id"),effect="individual")
a.within<-mean(fixef(within)) 
fd<-plm(lnhr ~ lnwg, data = mydata, model = "fd",index=c("id"))
random<-plm(lnhr ~ lnwg, data = mydata, model = "random",index=c("id"),effect="individual")

alpha[1]<-coef(pooled)[1]
alpha[2]<-coef(between)[1]
alpha[3]<-a.within
alpha[4]<-coef(fd)[1]
alpha[5]<-coef(random)[1]
tab$alpha<-alpha

beta[1]<-coef(pooled)[2]
beta[2]<-coef(between)[2]
beta[3]<-coef(within)[1]
beta[4]<-coef(fd)[2]
beta[5]<-coef(random)[2]
tab$beta<-beta


se[1]<-sqrt(vcov(pooled)[2][2])
se[2]<-coef(between)[2]
se[3]<-coef(within)[1]
se[4]<-coef(fd)[2]
se[5]<-coef(random)[2]
tab$se<-se




boot.pool<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "pooling",index=c("id")))[2]

boot.between<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "between",index=c("id")))[2]

boot.within<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "within",index=c("id"),effect="individual"))[1]

boot.fd<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "fd",index=c("id")))[2]

boot.re<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "random",index=c("id")))[2]

boot.my<-function(myfun, R){
  vec.coef<-numeric()
  if (myfun=="pooling"){
    for (i in 1:R){
      vec.coef[i]<-boot.pool(resamp(source,replace=TRUE,size=532))
    }  
  } else if (myfun=="within"){
    for (i in 1:R){
      vec.coef[i]<-boot.within(resamp(source,replace=TRUE,size=532))
    }
  } else if (myfun=="between"){
    for (i in 1:R){
      vec.coef[i]<-boot.between(resamp(source,replace=TRUE,size=532))
    }
  } else if (myfun=="random"){
    for (i in 1:R){
      vec.coef[i]<-boot.re(resamp(source,replace=TRUE,size=532))
    }
  } else if (myfun=="fd"){
    for (i in 1:R){
      vec.coef[i]<-boot.fd(resamp(source,replace=TRUE,size=532))
    }
  }
  print(paste("Bootstrapped Standard Error: ",round(sd(vec.coef),digits=3)))
}

boot.my("pooling",200)


