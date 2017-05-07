library(haven)
source <- read_dta("~/Desktop/Vlad's Stuff/Vlad's School/Econ769- Computing Assignment/017769exercise.dta")
require(plm)
require(boot)

pooledOLS <- plm(lnhr ~ lnwg, data = source, model = "pooling",index=c("id","year"))
summary(pooledOLS)
between <- plm(lnhr ~ lnwg, data = source, model = "between",index=c("id"))

within <- plm(lnhr ~ lnwg, data = source, model = "within",effect="twoways")
mean(fixef(within)) #The average of all the fixed effects in the model. 

first_diff<-plm(lnhr ~ lnwg, data = source, model = "fd",index=c("id"))

rand_eff<-plm(lnhr ~ lnwg, data = source, model = "random",index=c("id","year"),effect="twoways")
summary(rand_eff)

boot.rx <- function(data,i) {coef(lm(lnhr~lnwg,subset=i,data=data))}
output.rx <- boot(source, boot.rx, 999)

x <- rchisq(100,df=5)
boot.fun <- function(mydat,i) mean(mydat[i])
boot.out <- boot(x,boot.fun,1000)
boot.out
