library(haven)
source <- read_dta("~/Desktop/Vlad's Stuff/Vlad's School/Econ769- Computing Assignment/017769exercise.dta")
#source <- read_dta("C:/Users/vlads_000/Documents/GitHub/Econ769--Computing-Assignment/017769exercise.dta")
require(plm)
require(pglm)

resamp<-function(data,replace,size){ #Resampling function
  unique_ind<-unique(data$id) #Identifies the datas unique individuals
  samp_ind<-sample(unique_ind,size=size,replace=replace) #Randomly samples over those individuals
  do.call(rbind,lapply(samp_ind,function(x) data[data$id==x,])) #Returns data frame with those individuals observations over time.
}


form<-as.formula(paste("lnhr~lnwg+kids+disab+ageh+agesq"))
#form<-as.formula(paste("lnhr~lnwg"))

mydata<-resamp(source,replace=FALSE,size=266)

pooled<- plm(form, data = mydata, model = "pooling",index=c("id"))
between <- plm(form, data = mydata, model = "between",index=c("id"))
within <- plm(form, data = source, model = "within",index=c("id"),effect="individual")
a.within<-mean(fixef(within)) 
#Note this is the average of all the individual constant terms.

fd<-plm(form, data = mydata, model = "fd",index=c("id"))
random<-plm(form, data = source, model = "random",index=c("id"),effect="individual")

within <- plm(lnhr~lnwg, data = mydata, model = "within",index=c("id"),effect="individual")
random<-plm(lnhr~lnwg, data = mydata, model = "random",index=c("id"),effect="individual")

values<-list(pooled,between,fd,random)
alpha<-sapply(values, function(x) coef(x)[1])#Extracts constants
alpha[5]<-a.within #Need to do it seperately

beta<-sapply(values,function(x) coef(x)[2])
beta[5]<-coef(within)[1]

se<-sapply(values, function(x) sqrt(vcov(x)[2,2]))
se[5]<-sqrt(vcov(within)[1,1])

phtest(within,random) #Just for the labour supply elasticity component

boot_se<-function(data,R,model,effect,parameter){
  unique_ind<-unique(data$id)#Finds all unique individuals
  samp_ind<-list()#Creates list to store all of resamples on unique individuals
  for (j in 1:R){
    samp_ind[[j]]<-sample(unique_ind,size=length(unique_ind),replace=TRUE) #R resamples of individuals
  }
  #Creates list with all the bootstrap data sets we need
  func_data<-lapply(samp_ind, function(x) do.call(rbind,lapply(x,function(x) data[data$id==x,])))
  #Creates list of all the models from bootstrapping
  lapply(func_data, function(x) coef(plm(form, data=x, model=model,index="id",effect=effect)))
}

tab<-data.frame(Model=c("lnwg","kids","disab","ageh","agesq"))
for (j in values){
  se.b<-numeric()
  if (j =="within"){
    models<-boot_se(mydata,50,j,effect=NULL)
    test<-sapply(models, function(x) x)
    se.b[1]<-sd(test[1,])
    se.b[2]<-sd(test[2,])
    se.b[3]<-sd(test[3,])
    se.b[4]<-sd(test[4,])
    se.b[5]<-sd(test[5,])
    tab$j<-se.b
  } else{
    models<-boot_se(mydata,50,j,effect=NULL)
    test<-sapply(models, function(x) x)
    se.b[1]<-sd(test[2,])
    se.b[2]<-sd(test[3,])
    se.b[3]<-sd(test[4,])
    se.b[4]<-sd(test[5,])
    se.b[5]<-sd(test[6,])
    tab$j<-se.b
  }
}

models<-boot_se(mydata,200,"between",effect=NULL)
se.b[2]<-sd(sapply(models, function(x) x[2]))
#
models<-boot_se(mydata,200,"fd",effect=NULL)
se.b[3]<-sd(sapply(models, function(x) x[2]))
#
models<-boot_se(mydata,200,"random",effect="individual")
se.b[4]<-sd(sapply(models, function(x) x[2]))
#
models<-boot_se(mydata,200,"within",effect="individual")
se.b[5]<-sd(sapply(models, function(x) x[1]))
#
tab$alpha<-alpha
tab$beta<-beta
tab$se.b<-se.b
tab$se<-se
View(tab)

mydata$hr<-round(exp(mydata$lnhr)/365)
form.count<-as.formula(paste("hr~lnwg+kids+disab+ageh+agesq"))
summary(glm(hr~lnwg,family="poisson",data=mydata))
summary(pglm(hr~lnwg,model="within",family=poisson,data=mydata,index="id"))
summary(pglm(hr~lnwg,model="random",family=poisson,data=mydata,index="id"))
