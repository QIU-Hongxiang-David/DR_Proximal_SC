library(magrittr)
library(dplyr)
library(tidyr)
library(mvtnorm)
library(gmm)
library(quadprog)

true.ATE<-2

generate.data<-function(T,nU=2){
    T0<-T/2
    trend<-1.5*sin((1:T)/T*20*pi)
    
    generate.v<-function(n){
        matrix(rnorm(n*nU),nrow=n,ncol=nU)
    }
    
    U<-matrix(nrow=T,ncol=nU)
    U[1,]<-generate.v(1)
    if(T>1){
        for(t in 2:T){
            U[t,]<-0.1*U[t-1,]+0.9*generate.v(1)
        }
    }
    
    Y.mean<-((1:T)>T0)*(1:T)/T*true.ATE*4/3+2*rowSums(U)+trend
    Y<-rnorm(T,Y.mean)
    W<-lapply(1:nU,function(j){
        W.mean<-2*U[,j]+trend
        rnorm(T,W.mean)
    })%>%do.call(what=cbind)
    Z<-lapply(1:nU,function(j){
        Z.mean<-2*U[,j]+trend
        rnorm(T,Z.mean)
    })%>%do.call(what=cbind)
    
    list(t=1:T,W=W,Z=Z,Y=Y,T=T,T0=T0,nU=nU)
}


correct.DR<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W)%*%theta[1:(nU+1)])
    }
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z)%*%theta[(nU+2):(2*nU+2)]))
    }
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*cbind(1,data$Z)
        g2<-(t>T0)*(matrix(theta[(2*nU+3):(3*nU+3)],nrow=T,ncol=nU+1,byrow=TRUE)-cbind(1,data$W))
        g3<-(t<=T0)*(q*cbind(1,data$W)-matrix(theta[(2*nU+3):(3*nU+3)],nrow=T,ncol=nU+1,byrow=TRUE))
        g4<-(t>T0)*(theta[3*nU+4]-(data$Y-h)+theta[3*nU+5])
        g5<-(t<=T0)*(theta[3*nU+5]-q*(data$Y-h))
        
        cbind(g1,g2,g3,g4,g5)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        out<-matrix(0,3*nU+5,3*nU+5)
        
        #g1
        out[1:(nU+1),1:(nU+1)]<--rbind(1,t(data$Z))%*%(cbind(1,data$W)*(t<=T0))/T
        
        #g2
        out[(nU+2):(2*nU+2),(2*nU+3):(3*nU+3)]<-diag((T-T0)/T,nU+1)
        
        #g3
        out[(2*nU+3):(3*nU+3),(2*nU+3):(3*nU+3)]<-diag(-T0/T,nU+1)
        M1<-rbind(1,t(data$W))
        M2<-q*(t<=T0)*cbind(1,data$Z)/T
        out[(2*nU+3):(3*nU+3),(nU+2):(2*nU+2)]<-M1%*%M2
        
        #g4
        out[3*nU+4,3*nU+4]<-out[3*nU+4,3*nU+5]<-(T-T0)/T
        out[3*nU+4,1:(nU+1)]<-colMeans(cbind(1,data$W)*(t>T0))
        
        #g5
        out[3*nU+5,3*nU+5]<-T0/T
        out[3*nU+5,1:(nU+1)]<-colMeans(q*cbind(1,data$W)*(t<=T0))
        out[3*nU+5,(nU+2):(2*nU+2)]<-colMeans(-q*(data$Y-h)*cbind(1,data$Z)*(t<=T0))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    
    init.psi<-colMeans(cbind(1,data$W[t>T0,]))
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,,drop=FALSE])%*%init.beta)*(data$Y[t<=T0]-predict(init.model.h,newdata=as.data.frame(data$W[t<=T0,,drop=FALSE]))))
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,,drop=FALSE])))-init.psi.minus
    theta0<-c(init.alpha,init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("alpha",0:nU),paste0("beta",0:nU),paste0("psi",0:nU),"phi","psi-")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat<-coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}


correct.h<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W)%*%theta[1:(nU+1)])
    }
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        
        g1<-T/T0*(t<=T0)*(data$Y-h)*cbind(1,data$Z)
        g2<-(t>T0)*(theta[nU+2]-(data$Y-h))
        
        cbind(g1,g2)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        
        out<-matrix(0,nU+2,nU+2)
        
        #g1
        out[1:(nU+1),1:(nU+1)]<--rbind(1,t(data$Z))%*%(cbind(1,data$W)*(t<=T0))/T0
        
        #g2
        out[nU+2,nU+2]<-(T-T0)/T
        out[nU+2,1:(nU+1)]<-colMeans((t>T0)*cbind(1,data$W))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,,drop=FALSE])))
    theta0<-c(init.alpha,init.phi)
    
    names(theta0)<-c(paste0("alpha",0:nU),"phi")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat=coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}


correct.q<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z)%*%theta[1:(nU+1)]))
    }
    
    g<-function(theta,data){
        q<-evalq(theta,data)
        
        g1<-(t>T0)*(matrix(theta[(nU+2):(2*nU+2)],nrow=T,ncol=nU+1,byrow=TRUE)-cbind(1,data$W))
        g2<-(t<=T0)*(q*cbind(1,data$W)-matrix(theta[(nU+2):(2*nU+2)],nrow=T,ncol=nU+1,byrow=TRUE))
        g3<-(t>T0)*(theta[2*nU+3]-data$Y+theta[2*nU+4])
        g4<-(t<=T0)*(theta[2*nU+4]-q*data$Y)
        
        cbind(g1,g2,g3,g4)
    }
    
    gradv<-function(theta,data){
        q<-evalq(theta,data)
        
        out<-matrix(0,2*nU+4,2*nU+4)
        
        #g1
        out[1:(nU+1),(nU+2):(2*nU+2)]<-diag((T-T0)/T,nU+1)
        
        #g2
        out[(nU+2):(2*nU+2),(nU+2):(2*nU+2)]<-diag(-T0/T,nU+1)
        M1<-rbind(1,t(data$W))
        M2<-q*(t<=T0)*cbind(1,data$Z)/T
        out[(nU+2):(2*nU+2),1:(nU+1)]<-M1%*%M2
        
        #g3
        out[2*nU+3,2*nU+3]<-out[2*nU+3,2*nU+4]<-(T-T0)/T
        
        #g4
        out[2*nU+4,2*nU+4]<-T0/T
        out[2*nU+4,1:(nU+1)]<-colMeans(-q*data$Y*cbind(1,data$Z)*(t<=T0))
        
        out
    }
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    init.psi<-colMeans(cbind(1,data$W[t>T0,]))
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,,drop=FALSE])%*%init.beta)*data$Y[t<=T0])
    init.phi<-mean(data$Y[t>T0])-init.psi.minus
    theta0<-c(init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("beta",0:nU),paste0("psi",0:nU),"phi","psi-")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat<-coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}


mis.h.DR<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W[,1])%*%theta[1:2])
    }
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z)%*%theta[3:(nU+3)]))
    }
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*cbind(1,data$Z[,1])
        g2<-(t>T0)*(matrix(theta[(nU+4):(2*nU+4)],nrow=T,ncol=nU+1,byrow=TRUE)-cbind(1,data$W))
        g3<-(t<=T0)*(q*cbind(1,data$W)-matrix(theta[(nU+4):(2*nU+4)],nrow=T,ncol=nU+1,byrow=TRUE))
        g4<-(t>T0)*(theta[2*nU+5]-(data$Y-h)+theta[2*nU+6])
        g5<-(t<=T0)*(theta[2*nU+6]-q*(data$Y-h))
        
        cbind(g1,g2,g3,g4,g5)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        out<-matrix(0,2*nU+6,2*nU+6)
        
        #g1
        out[1:2,1:2]<--rbind(1,t(data$Z[,1]))%*%(cbind(1,data$W[,1])*(t<=T0))/T
        
        #g2
        out[3:(nU+3),(nU+4):(2*nU+4)]<-diag((T-T0)/T,nU+1)
        
        #g3
        out[(nU+4):(2*nU+4),(nU+4):(2*nU+4)]<-diag(-T0/T,nU+1)
        M1<-rbind(1,t(data$W))
        M2<-q*(t<=T0)*cbind(1,data$Z)/T
        out[(nU+4):(2*nU+4),3:(nU+3)]<-M1%*%M2
        
        #g4
        out[2*nU+5,2*nU+5]<-out[2*nU+5,2*nU+6]<-(T-T0)/T
        out[2*nU+5,1:2]<-colMeans(cbind(1,data$W[,1])*(t>T0))
        
        #g5
        out[2*nU+6,2*nU+6]<-T0/T
        out[2*nU+6,1:2]<-colMeans(q*cbind(1,data$W[,1])*(t<=T0))
        out[2*nU+6,3:(nU+3)]<-colMeans(-q*(data$Y-h)*cbind(1,data$Z)*(t<=T0))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W[,1,drop=FALSE]),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    
    init.psi<-colMeans(cbind(1,data$W[t>T0,,drop=FALSE]))
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,,drop=FALSE])%*%init.beta)*(data$Y[t<=T0]-cbind(1,data$W[t<=T0,1,drop=FALSE])%*%init.alpha))
    init.phi<-mean(data$Y[t>T0]-cbind(1,data$W[t>T0,1,drop=FALSE])%*%init.alpha)-init.psi.minus
    theta0<-c(init.alpha,init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("alpha",0:1),paste0("beta",0:nU),paste0("psi",0:nU),"phi","psi-")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat<-coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}


mis.q.DR<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W)%*%theta[1:(nU+1)])
    }
    evalq<-function(theta,data){
        as.numeric(exp(-2*data$Z[,1]+cbind(1,data$Z[,2])%*%theta[(nU+2):(nU+3)]))
    }
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*cbind(1,data$Z)
        g2<-(t>T0)*(matrix(theta[(nU+4):(nU+5)],nrow=T,ncol=2,byrow=TRUE)-cbind(1,data$W[,1]))
        g3<-(t<=T0)*(q*cbind(1,data$W[,1])-matrix(theta[(nU+4):(nU+5)],nrow=T,ncol=2,byrow=TRUE))
        g4<-(t>T0)*(theta[nU+6]-(data$Y-h)+theta[nU+7])
        g5<-(t<=T0)*(theta[nU+7]-q*(data$Y-h))
        
        cbind(g1,g2,g3,g4,g5)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        out<-matrix(0,nU+7,nU+7)
        
        #g1
        out[1:(nU+1),1:(nU+1)]<--rbind(1,t(data$Z))%*%(cbind(1,data$W)*(t<=T0))/T
        
        #g2
        out[(nU+2):(nU+3),(nU+4):(nU+5)]<-diag((T-T0)/T,2)
        
        #g3
        out[(nU+4):(nU+5),(nU+4):(nU+5)]<-diag(-T0/T,2)
        M1<-rbind(1,t(data$W[,1]))
        M2<-q*(t<=T0)*cbind(1,data$Z[,2])/T
        out[(nU+4):(nU+5),(nU+2):(nU+3)]<-M1%*%M2
        
        #g4
        out[nU+6,nU+6]<-out[nU+6,nU+7]<-(T-T0)/T
        out[nU+6,1:(nU+1)]<-colMeans(cbind(1,data$W)*(t>T0))
        
        #g5
        out[nU+7,nU+7]<-T0/T
        out[nU+7,1:(nU+1)]<-colMeans(q*cbind(1,data$W)*(t<=T0))
        out[nU+7,(nU+2):(nU+3)]<-colMeans(-q*(data$Y-h)*cbind(1,data$Z[,2])*(t<=T0))
        
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z[,2,drop=FALSE]),family=binomial(),offset=-2*data$Z[,1])
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    
    init.psi<-colMeans(cbind(1,data$W[t>T0,1,drop=FALSE]))
    init.psi.minus<-mean(exp(-2*data$Z[t<=T0,1,drop=FALSE]+cbind(1,data$Z[t<=T0,2,drop=FALSE])%*%init.beta)*(data$Y[t<=T0]-predict(init.model.h,newdata=as.data.frame(data$W[t<=T0,,drop=FALSE]))))
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,,drop=FALSE])))-init.psi.minus
    theta0<-c(init.alpha,init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("alpha",0:nU),paste0("beta",0:1),paste0("psi",0:1),"phi","psi-")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat<-coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}


mis.h<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W[,1])%*%theta[1:2])
    }
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*cbind(1,data$Z[,1])
        g2<-(t>T0)*(theta[3]-(data$Y-h))
        
        cbind(g1,g2)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        
        out<-matrix(0,3,3)
        
        #g1
        out[1:2,1:2]<--rbind(1,t(data$Z[,1]))%*%(cbind(1,data$W[,1])*(t<=T0))/T
        
        #g2
        out[3,3]<-(T-T0)/T
        out[3,1:2]<-colMeans((t>T0)*cbind(1,data$W[,1]))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W[,1,drop=FALSE]),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    init.phi<-mean(data$Y[t>T0]-(cbind(1,data$W[t>T0,1,drop=FALSE])%*%init.alpha))
    theta0<-c(init.alpha,init.phi)
    
    names(theta0)<-c(paste0("alpha",0:1),"phi")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat=coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}


mis.q<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    
    evalq<-function(theta,data){
        as.numeric(exp(-2*data$Z[,1]+cbind(1,data$Z[,2])%*%theta[1:2]))
    }
    
    g<-function(theta,data){
        q<-evalq(theta,data)
        
        g1<-(t>T0)*(matrix(theta[3:4],nrow=T,ncol=2,byrow=TRUE)-cbind(1,data$W[,1]))
        g2<-(t<=T0)*(q*cbind(1,data$W[,1])-matrix(theta[3:4],nrow=T,ncol=2,byrow=TRUE))
        g3<-(t>T0)*(theta[5]-data$Y+theta[6])
        g4<-(t<=T0)*(theta[6]-q*data$Y)
        
        cbind(g1,g2,g3,g4)
    }
    
    gradv<-function(theta,data){
        q<-evalq(theta,data)
        
        out<-matrix(0,6,6)
        
        #g1
        out[1:2,3:4]<-diag((T-T0)/T,2)
        
        #g2
        out[3:4,3:4]<-diag(-T0/T,2)
        M1<-rbind(1,t(data$W[,1]))
        M2<-q*(t<=T0)*cbind(1,data$Z[,2])/T
        out[3:4,1:2]<-M1%*%M2
        
        #g3
        out[5,5]<-out[5,6]<-(T-T0)/T
        
        #g4
        out[6,6]<-T0/T
        out[6,1:2]<-colMeans(-q*data$Y*cbind(1,data$Z[,2])*(t<=T0))
        
        out
    }
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z[,2,drop=FALSE]),family=binomial(),offset=-2*data$Z[,1])
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    init.psi<-colMeans(cbind(1,data$W[t>T0,1,drop=FALSE]))
    init.psi.minus<-mean(exp(-2*data$Z[t<=T0,1]+cbind(1,data$Z[t<=T0,2,drop=FALSE])%*%init.beta)*data$Y[t<=T0])
    init.phi<-mean(data$Y[t>T0])-init.psi.minus
    theta0<-c(init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("beta",0:1),paste0("psi",0:1),"phi","psi-")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat=coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}

OLS<-function(data){
    T<-data$T
    T0<-data$T0
    nU<-data$nU
    t<-data$t
    treatment<<-as.numeric(t>T0)
    Y<<-data$Y
    W<<-data$W
    
    model<-gmm(g=Y~-1+W+treatment,x=~-1+W+treatment,wmatrix="ident",vcov="HAC")
    list(phi.hat=coef(model)["treatment"],SE=coef(summary(model))["treatment","Std. Error"],CI=confint(model)$test["treatment",],convergence=0)
}
