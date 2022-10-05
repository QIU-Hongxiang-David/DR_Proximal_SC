library(magrittr)
library(dplyr)
library(tidyr)
library(mvtnorm)
library(gmm)
library(quadprog)

true.ATE<-2
nU<-3
nW<-5
nZ<-10

#each column is the coefficient used to generated W and Z
a<-matrix(c(1,0,0,
            0,1,0,
            0,0,1,
            1,1,0,
            1,0,1),nrow=3)
b<-matrix(c(2,0,0,
            0,2,0,
            0,0,2,
            -3,0,0,
            0,-3,0,
            0,0,-3,
            1,-1,0,
            1,0,-1,
            0,1,-1,
            2,-.5,-.5),nrow=3)

generate.data<-function(T){
    T0<-T/2
    
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
    U<-pnorm(U)
    U[1:T0,]<-qexp(U[1:T0,],rate=1)
    U[(T0+1):T,]<-qexp(U[(T0+1):T,],rate=2)
    
    Y.mean<-true.ATE*((1:T)>T0)+2*rowSums(U)
    Y<-runif(T,Y.mean-1,Y.mean+1)
    W<-lapply(1:nW,function(j){
        W.mean<-U%*%a[,j]
        runif(T,W.mean-1,W.mean+1)
    })%>%do.call(what=cbind)
    Z<-lapply(1:nZ,function(j){
        Z.mean<-U%*%b[,j]
        runif(T,Z.mean-1,Z.mean+1)
    })%>%do.call(what=cbind)
    
    list(t=1:T,W=W,Z=Z,Y=Y,T=T,T0=T0)
}


correct.DR<-function(data){
    T<-data$T
    T0<-data$T0
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W)%*%theta[1:(nW+1)])
    }
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z)%*%theta[(nW+2):(nW+nZ+2)]))
    }
    gh<-cbind(1,data$Z)
    ngh<-ncol(gh)
    gq<-cbind(1,poly(data$W,degree=2,raw=TRUE))
    ngq<-ncol(gq)
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*gh
        g2<-(t>T0)*(matrix(theta[(nW+nZ+3):(nW+nZ+ngq+2)],nrow=T,ncol=ngq,byrow=TRUE)-gq)
        g3<-(t<=T0)*(q*gq-matrix(theta[(nW+nZ+3):(nW+nZ+ngq+2)],nrow=T,ncol=ngq,byrow=TRUE))
        g4<-(t>T0)*(theta[nW+nZ+ngq+3]-(data$Y-h)+theta[nW+nZ+ngq+4])
        g5<-(t<=T0)*(theta[nW+nZ+ngq+4]-q*(data$Y-h))
        
        cbind(g1,g2,g3,g4,g5)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        out<-matrix(0,ngh+2*ngq+2,nW+nZ+ngq+4)
        
        #g1
        out[1:ngh,1:(nW+1)]<--t(gh)%*%(cbind(1,data$W)*(t<=T0))/T
        
        #g2
        out[(ngh+1):(ngh+ngq),(nW+nZ+3):(nW+nZ+ngq+2)]<-diag((T-T0)/T,ngq)
        
        #g3
        out[(ngh+ngq+1):(ngh+2*ngq),(nW+nZ+3):(nW+nZ+ngq+2)]<-diag(-T0/T,ngq)
        M1<-t(gq)
        M2<-q*(t<=T0)*cbind(1,data$Z)/T
        out[(ngh+ngq+1):(ngh+2*ngq),(nW+2):(nW+nZ+2)]<-M1%*%M2
        
        #g4
        out[ngh+2*ngq+1,nW+nZ+ngq+3]<-out[ngh+2*ngq+1,nW+nZ+ngq+4]<-(T-T0)/T
        out[ngh+2*ngq+1,1:(nW+1)]<-colMeans(cbind(1,data$W)*(t>T0))
        
        #g5
        out[ngh+2*ngq+2,nW+nZ+ngq+4]<-T0/T
        out[ngh+2*ngq+2,1:(nW+1)]<-colMeans(q*cbind(1,data$W)*(t<=T0))
        out[ngh+2*ngq+2,(nW+2):(nW+nZ+2)]<-colMeans(-q*(data$Y-h)*cbind(1,data$Z)*(t<=T0))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    
    init.psi<-colMeans(gq[t>T0,,drop=FALSE])
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,,drop=FALSE])%*%init.beta)*(data$Y[t<=T0]-predict(init.model.h,newdata=as.data.frame(data$W[t<=T0,,drop=FALSE]))))
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,,drop=FALSE])))-init.psi.minus
    theta0<-c(init.alpha,init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("alpha",0:nW),paste0("beta",0:nZ),paste0("psi",1:ngq),"phi","psi-")
    
    model<-gmm(g=g,x=data,t0=theta0,gradv=gradv,wmatrix="ident",control=list(maxit=1e4),method="BFGS",vcov="iid")
    
    phi.hat=coef(model)["phi"]
    var<-sandwich::vcovHAC(model)
    rownames(var)<-colnames(var)<-names(theta0)
    SE<-sqrt(var["phi","phi"])
    CI<-phi.hat+qnorm(c(.025,.975))*SE
    list(phi.hat=phi.hat,SE=SE,CI=CI,convergence=model$algoInfo$convergence)
}


correct.h<-function(data){
    T<-data$T
    T0<-data$T0
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W)%*%theta[1:(nW+1)])
    }
    gh<-cbind(1,data$Z)
    ngh<-ncol(gh)
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        
        g1<-T/T0*(t<=T0)*(data$Y-h)*gh
        g2<-(t>T0)*(theta[nW+2]-(data$Y-h))
        
        cbind(g1,g2)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        
        out<-matrix(0,ngh+1,nW+2)
        
        #g1
        out[1:ngh,1:(nW+1)]<--t(gh)%*%(cbind(1,data$W)*(t<=T0))/T0
        
        #g2
        out[ngh+1,nW+2]<-(T-T0)/T
        out[ngh+1,1:(nW+1)]<-colMeans((t>T0)*cbind(1,data$W))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,,drop=FALSE])))
    theta0<-c(init.alpha,init.phi)
    
    names(theta0)<-c(paste0("alpha",0:nW),"phi")
    
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
    t<-data$t
    
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z)%*%theta[1:(nZ+1)]))
    }
    gq<-cbind(1,poly(data$W,degree=2,raw=TRUE))
    ngq<-ncol(gq)
    
    g<-function(theta,data){
        q<-evalq(theta,data)
        
        g1<-(t>T0)*(matrix(theta[(nZ+2):(nZ+ngq+1)],nrow=T,ncol=ngq,byrow=TRUE)-gq)
        g2<-(t<=T0)*(q*gq-matrix(theta[(nZ+2):(nZ+ngq+1)],nrow=T,ncol=ngq,byrow=TRUE))
        g3<-(t>T0)*(theta[nZ+ngq+2]-data$Y+theta[nZ+ngq+3])
        g4<-(t<=T0)*(theta[nZ+ngq+3]-q*data$Y)
        
        cbind(g1,g2,g3,g4)
    }
    
    gradv<-function(theta,data){
        q<-evalq(theta,data)
        
        out<-matrix(0,2*ngq+2,nZ+ngq+3)
        
        #g1
        out[1:ngq,(nZ+2):(nZ+ngq+1)]<-diag((T-T0)/T,ngq)
        
        #g2
        out[(ngq+1):(2*ngq),(nZ+2):(nZ+ngq+1)]<-diag(-T0/T,ngq)
        M1<-t(gq)
        M2<-q*(t<=T0)*cbind(1,data$Z)/T
        out[(ngq+1):(2*ngq),1:(nZ+1)]<-M1%*%M2
        
        #g3
        out[2*ngq+1,nZ+ngq+2]<-out[2*ngq+1,nZ+ngq+3]<-(T-T0)/T
        
        #g4
        out[2*ngq+2,nZ+ngq+3]<-T0/T
        out[2*ngq+2,1:(nZ+1)]<-colMeans(-q*data$Y*cbind(1,data$Z)*(t<=T0))
        
        out
    }
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    init.psi<-colMeans(gq[t>T0,,drop=FALSE])
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,,drop=FALSE])%*%init.beta)*data$Y[t<=T0])
    init.phi<-mean(data$Y[t>T0])-init.psi.minus
    theta0<-c(init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("beta",0:nZ),paste0("psi",1:ngq),"phi","psi-")
    
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
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W[,1])%*%theta[1:2])
    }
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z)%*%theta[3:(nZ+3)]))
    }
    gh<-cbind(1,data$Z[,1])
    ngh<-ncol(gh)
    gq<-cbind(1,poly(data$W,degree=2,raw=TRUE))
    ngq<-ncol(gq)
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*gh
        g2<-(t>T0)*(matrix(theta[(nZ+4):(nZ+ngq+3)],nrow=T,ncol=ngq,byrow=TRUE)-gq)
        g3<-(t<=T0)*(q*gq-matrix(theta[(nZ+4):(nZ+ngq+3)],nrow=T,ncol=ngq,byrow=TRUE))
        g4<-(t>T0)*(theta[nZ+ngq+4]-(data$Y-h)+theta[nZ+ngq+5])
        g5<-(t<=T0)*(theta[nZ+ngq+5]-q*(data$Y-h))
        
        cbind(g1,g2,g3,g4,g5)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        out<-matrix(0,ngh+2*ngq+2,nZ+ngq+5)
        
        #g1
        out[1:2,1:2]<--t(gh)%*%(cbind(1,data$W[,1])*(t<=T0))/T
        
        #g2
        out[3:(ngq+2),(nZ+4):(nZ+ngq+3)]<-diag((T-T0)/T,ngq)
        
        #g3
        out[(ngh+ngq+1):(ngh+2*ngq),(nZ+4):(nZ+ngq+3)]<-diag(-T0/T,ngq)
        M1<-t(gq)
        M2<-q*(t<=T0)*cbind(1,data$Z)/T
        out[(ngh+ngq+1):(ngh+2*ngq),3:(nZ+3)]<-M1%*%M2
        
        #g4
        out[ngh+2*ngq+1,nZ+ngq+4]<-out[ngh+2*ngq+1,nZ+ngq+5]<-(T-T0)/T
        out[ngh+2*ngq+1,1:2]<-colMeans(cbind(1,data$W[,1])*(t>T0))
        
        #g5
        out[ngh+2*ngq+2,nZ+ngq+5]<-T0/T
        out[ngh+2*ngq+2,1:2]<-colMeans(q*cbind(1,data$W[,1])*(t<=T0))
        out[ngh+2*ngq+2,3:(nZ+3)]<-colMeans(-q*(data$Y-h)*cbind(1,data$Z)*(t<=T0))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W[,1,drop=FALSE]),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    
    init.psi<-colMeans(gq[t>T0,,drop=FALSE])
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,,drop=FALSE])%*%init.beta)*(data$Y[t<=T0]-predict(init.model.h,newdata=as.data.frame(data$W[t<=T0,1,drop=FALSE]))))
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,1,drop=FALSE])))-init.psi.minus
    theta0<-c(init.alpha,init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("alpha",0:1),paste0("beta",0:nZ),paste0("psi",1:ngq),"phi","psi-")
    
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
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W)%*%theta[1:(nW+1)])
    }
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z[,1])%*%theta[(nW+2):(nW+3)]))
    }
    gh<-cbind(1,data$Z)
    ngh<-ncol(gh)
    gq<-cbind(1,poly(data$W[,1],degree=2,raw=TRUE))
    ngq<-ncol(gq)
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*gh
        g2<-(t>T0)*(matrix(theta[(nW+4):(nW+ngq+3)],nrow=T,ncol=ngq,byrow=TRUE)-gq)
        g3<-(t<=T0)*(q*gq-matrix(theta[(nW+4):(nW+ngq+3)],nrow=T,ncol=ngq,byrow=TRUE))
        g4<-(t>T0)*(theta[nW+ngq+4]-(data$Y-h)+theta[nW+ngq+5])
        g5<-(t<=T0)*(theta[nW+ngq+5]-q*(data$Y-h))
        
        cbind(g1,g2,g3,g4,g5)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        q<-evalq(theta,data)
        
        out<-matrix(0,ngh+2*ngq+2,nW+ngq+5)
        
        #g1
        out[1:ngh,1:(nW+1)]<--t(gh)%*%(cbind(1,data$W)*(t<=T0))/T
        
        #g2
        out[(ngh+1):(ngh+ngq),(nW+4):(nW+ngq+3)]<-diag((T-T0)/T,ngq)
        
        #g3
        out[(ngh+ngq+1):(ngh+2*ngq),(nW+4):(nW+ngq+3)]<-diag(-T0/T,ngq)
        M1<-t(gq)
        M2<-q*(t<=T0)*cbind(1,data$Z[,1])/T
        out[(ngh+ngq+1):(ngh+2*ngq),(nW+2):(nW+3)]<-M1%*%M2
        
        #g4
        out[ngh+2*ngq+1,nW+ngq+4]<-out[ngh+2*ngq+1,nW+ngq+5]<-(T-T0)/T
        out[ngh+2*ngq+1,1:(nW+1)]<-colMeans(cbind(1,data$W)*(t>T0))
        
        #g5
        out[ngh+2*ngq+2,nW+ngq+5]<-T0/T
        out[ngh+2*ngq+2,1:(nW+1)]<-colMeans(q*cbind(1,data$W)*(t<=T0))
        out[ngh+2*ngq+2,(nW+2):(nW+3)]<-colMeans(-q*(data$Y-h)*cbind(1,data$Z[,1])*(t<=T0))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z[,1,drop=FALSE]),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    
    init.psi<-colMeans(gq[t>T0,,drop=FALSE])
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,1,drop=FALSE])%*%init.beta)*(data$Y[t<=T0]-predict(init.model.h,newdata=as.data.frame(data$W[t<=T0,,drop=FALSE]))))
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,,drop=FALSE])))-init.psi.minus
    theta0<-c(init.alpha,init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("alpha",0:nW),paste0("beta",0:1),paste0("psi",1:ngq),"phi","psi-")
    
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
    t<-data$t
    
    evalh<-function(theta,data){
        as.numeric(cbind(1,data$W[,1])%*%theta[1:2])
    }
    gh<-cbind(1,data$Z[,1])
    ngh<-ncol(gh)
    
    g<-function(theta,data){
        h<-evalh(theta,data)
        
        g1<-(t<=T0)*(data$Y-h)*gh
        g2<-(t>T0)*(theta[3]-(data$Y-h))
        
        cbind(g1,g2)
    }
    
    gradv<-function(theta,data){
        h<-evalh(theta,data)
        
        out<-matrix(0,ngh+1,3)
        
        #g1
        out[1:ngh,1:2]<--t(gh)%*%(cbind(1,data$W[,1])*(t<=T0))/T
        
        #g2
        out[ngh+1,3]<-(T-T0)/T
        out[ngh+1,1:2]<-colMeans((t>T0)*cbind(1,data$W[,1]))
        
        out
    }
    
    init.model.h<-lm(data$Y~.,data=as.data.frame(data$W[,1,drop=FALSE]),subset=t<=T0)
    init.alpha<-coef(init.model.h)
    init.phi<-mean(data$Y[t>T0]-predict(init.model.h,newdata=as.data.frame(data$W[t>T0,1,drop=FALSE])))
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
    t<-data$t
    
    evalq<-function(theta,data){
        as.numeric(exp(cbind(1,data$Z[,1])%*%theta[1:2]))
    }
    gq<-cbind(1,poly(data$W[,1],degree=2,raw=TRUE))
    ngq<-ncol(gq)
    
    g<-function(theta,data){
        q<-evalq(theta,data)
        
        g1<-(t>T0)*(matrix(theta[3:(ngq+2)],nrow=T,ncol=ngq,byrow=TRUE)-gq)
        g2<-(t<=T0)*(q*gq-matrix(theta[3:(ngq+2)],nrow=T,ncol=ngq,byrow=TRUE))
        g3<-(t>T0)*(theta[ngq+3]-data$Y+theta[ngq+4])
        g4<-(t<=T0)*(theta[ngq+4]-q*data$Y)
        
        cbind(g1,g2,g3,g4)
    }
    
    gradv<-function(theta,data){
        q<-evalq(theta,data)
        
        out<-matrix(0,2*ngq+2,ngq+4)
        
        #g1
        out[1:ngq,3:(ngq+2)]<-diag((T-T0)/T,ngq)
        
        #g2
        out[(ngq+1):(2*ngq),3:(ngq+2)]<-diag(-T0/T,ngq)
        M1<-t(gq)
        M2<-q*(t<=T0)*cbind(1,data$Z[,1])/T
        out[(ngq+1):(2*ngq),1:2]<-M1%*%M2
        
        #g3
        out[2*ngq+1,ngq+3]<-out[2*ngq+1,ngq+4]<-(T-T0)/T
        
        #g4
        out[2*ngq+2,ngq+4]<-T0/T
        out[2*ngq+2,1:2]<-colMeans(-q*data$Y*cbind(1,data$Z[,1])*(t<=T0))
        
        out
    }
    
    init.model.q<-glm(I(t>T0)~.,data=as.data.frame(data$Z[,1,drop=FALSE]),family=binomial())
    init.beta<-coef(init.model.q)
    init.beta[1]<-init.beta[1]+log((T-T0)/T0)
    init.psi<-colMeans(gq[t>T0,,drop=FALSE])
    init.psi.minus<-mean(exp(cbind(1,data$Z[t<=T0,1,drop=FALSE])%*%init.beta)*data$Y[t<=T0])
    init.phi<-mean(data$Y[t>T0])-init.psi.minus
    theta0<-c(init.beta,init.psi,init.phi,init.psi.minus)
    
    names(theta0)<-c(paste0("beta",0:1),paste0("psi",1:ngq),"phi","psi-")
    
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
