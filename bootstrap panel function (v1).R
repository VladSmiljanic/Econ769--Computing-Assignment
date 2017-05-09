set.seed(42)

ptm<-proc.time()
res<-function(data){
  unique_ind<-unique(source$id)
  samp_ind<-sample(unique_ind,size=length(unique_ind),replace=TRUE)
  do.call(rbind,lapply(samp_ind,function(x) source[source$id==x,]))
}

boot.pool<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "pooling",index=c("id")))[2]

boot.between<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "between",index=c("id")))[2]

boot.within<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "within",index=c("id"),effect="individual"))[1]

boot.fd<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "fd",index=c("id")))[2]

boot.re<-function(mydat) coef(plm(lnhr ~ lnwg, data = mydat, model = "random",index=c("id")))[2]

boot.my<-function(myfun, R){
  vec.coef<-numeric()
  if (myfun=="pooling"){
    for (i in 1:R){
      vec.coef[i]<-boot.pool(res(source))
    }  
  } else if (myfun=="within"){
    for (i in 1:R){
      vec.coef[i]<-boot.within(res(source))
    }
  } else if (myfun=="between"){
    for (i in 1:R){
      vec.coef[i]<-boot.between(res(source))
    }
  } else if (myfun=="random"){
    for (i in 1:R){
      vec.coef[i]<-boot.re(res(source))
    }
  } else if (myfun=="fd"){
    for (i in 1:R){
      vec.coef[i]<-boot.fd(res(source))
    }
  }
  mean(vec.coef)
}

boot.my("between",100)
proc.time()-ptm
