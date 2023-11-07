source("generate_data.R")

Ts<-c(80,100)
nUs<-2
sim.param<-expand.grid(T=Ts,nU=nUs)
N<-200

`%between%`<-function(x,y){
    y[1]<=x & x<=y[2]
}

job.id<-as.numeric(Sys.getenv("SGE_TASK_ID"))
T<-sim.param$T[job.id]
nU<-sim.param$nU[job.id]

if(!dir.exists("cache")){
    dir.create("cache")
}

set.seed(1737209457+job.id)

results<-lapply(1:N,function(dummy){
    data<-generate.data(T,nU)
    correct.DR.result<-tryCatch({
        correct.DR(data)
    },error=function(e){
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    correct.h.result<-tryCatch({
        correct.h(data)
    },error=function(e){
        print(e)
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    correct.q.result<-tryCatch({
        correct.q(data)
    },error=function(e){
        print(e)
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    mis.h.DR.result<-tryCatch({
        mis.h.DR(data)
    },error=function(e){
        print(e)
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    mis.q.DR.result<-tryCatch({
        mis.q.DR(data)
    },error=function(e){
        print(e)
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    mis.h.result<-tryCatch({
        mis.h(data)
    },error=function(e){
        print(e)
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    mis.q.result<-tryCatch({
        mis.q(data)
    },error=function(e){
        print(e)
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    OLS.result<-tryCatch({
        OLS(data)
    },error=function(e){
        print(e)
        list(phi.hat=NA,SE=NA,CI=rep(NA,2),convergence=1)
    })
    
    data.frame(T=T,nU=nU,method=c("correct.DR","correct.h","correct.q","mis.h.DR","mis.q.DR","mis.h","mis.q","OLS"),
               phi.hat=c(correct.DR.result$phi.hat,correct.h.result$phi.hat,correct.q.result$phi.hat,mis.h.DR.result$phi.hat,mis.q.DR.result$phi.hat,mis.h.result$phi.hat,mis.q.result$phi.hat,OLS.result$phi.hat),
               SE=c(correct.DR.result$SE,correct.h.result$SE,correct.q.result$SE,mis.h.DR.result$SE,mis.q.DR.result$SE,mis.h.result$SE,mis.q.result$SE,OLS.result$SE),
               CI.cover=c(true.ATE %between% correct.DR.result$CI,true.ATE %between% correct.h.result$CI,true.ATE %between% correct.q.result$CI,true.ATE %between% mis.h.DR.result$CI,true.ATE %between% mis.q.DR.result$CI,true.ATE %between% mis.h.result$CI,true.ATE %between% mis.q.result$CI,true.ATE %between% OLS.result$CI),
               convergence=c(correct.DR.result$convergence,correct.h.result$convergence,correct.q.result$convergence,mis.h.DR.result$convergence,mis.q.DR.result$convergence,mis.h.result$convergence,mis.q.result$convergence,OLS.result$convergence))
})%>%bind_rows

saveRDS(results,paste0("cache/",job.id,".rds"))
