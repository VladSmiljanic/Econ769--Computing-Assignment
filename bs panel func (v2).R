set.seed(42)

ptm<-proc.time()
resample<-function(data,R,model,effect,parameter){
  unique_ind<-unique(source$id)#Finds all unique individuals
  samp_ind<-list()#Creates list to store all of resamples on unique individuals
  for (j in 1:R){
    samp_ind[[j]]<-sample(unique_ind,size=length(unique_ind),replace=TRUE) #R resamples of individuals
  }
  #Creates list with all the bootstrap data sets we need
  func_data<-lapply(samp_ind, function(x) do.call(rbind,lapply(x,function(x) source[source$id==x,])))
  #Creates list of all the models from bootstrapping
  lapply(func_data, function(x) plm(lnhr~lnwg, data=x, model=model,index="id",effect=effect))
}

models<-resample(source,100,"between",effect=NULL)
mean(sapply(models, function(x) coef(x)[2]))
proc.time()-ptm