### 《多元统计分析及R语言建模》第四版函数 
### 暨南大学 王斌会 2016.1.1  
### 适用于所有R版本，
### 请在R在用source("mvstats.R")调用


plot.andrews<-function(x){
   # x is a matrix or data frame of data
   if (is.data.frame(x)==TRUE)
      x<-as.matrix(x)
   t<-seq(-pi, pi, pi/30)
   m<-nrow(x); n<-ncol(x)
   f<-array(0, c(m,length(t)))
   for(i in 1:m){
      f[i,]<-x[i,1]/sqrt(2)
      for( j in 2:n){
          if (j%%2==0) 
             f[i,]<-f[i,]+x[i,j]*sin(j/2*t)
          else
             f[i,]<-f[i,]+x[i,j]*cos(j%/%2*t)
      } 
  }
  #plot(c(-pi,pi), c(min(f),max(f)), type="n", xlab="t", ylab="f(t)")
  plot(c(-pi,pi), c(min(f),max(f)), type="n", xlab="", ylab="")
  for(i in 1:m) lines(t, f[i,] , col=i)
  legend(2,max(f),rownames(x),col=1:nrow(x),lty=1:nrow(x),bty='n',cex=0.8)
}

stat<-function(x)
{
  if (all(is.na(x))) {
        stop("All elements of ", substitute(x), " have a missing value")
  }
  if(is.vector(x))
  {
     S=cbind(n=length(x),mean=mean(x),sd=sd(x),min=min(x),max=max(x))
  }
  else  
  {
     S=cbind(n=nrow(x),mean=apply(x,2,mean),sd=apply(x,2,sd),min=apply(x,2,min),max=apply(x,2,max))
  }   
   print(round(S,4))
}

freq<-function(X)
{   
    vars=names(X)
    X=as.matrix(X)
    p=ncol(X)
    tab<-function(Y)
    { 
       T1=table(Y)
       Percent=T1/sum(T1)*100
       T2=cbind(Freqency=T1,Percent,Cum.percent=cumsum(Percent))
       Total=c(sum(T1),100,100)
       print(rbind(T2,Total))
    }
    if (p == 1) { cat('\n'); tab(X); }
    else {
       for(i in 1:p) 
         { cat('\n',vars[i],"\n"); tab(X[,i]) } 
   }
}

corr.test<-function(X,diag=TRUE){
  options(digits=4)
  p=ncol(X);
  if(diag)
  {
    tp=matrix(1,p,p);
    for(i in 1:p)
    {
      for(j in 1:i) tp[i,j]=cor.test(X[,i],X[,j])$stat;
      for(j in i:p) tp[i,j]=cor.test(X[,i],X[,j])$p.value;
    }
    cat("corr test: \n"); 
    tp=round(matrix(tp,p,dimnames=list(names(X),names(X))),4)
    print(tp)
    #return(tp)
    cat("lower is t value，upper is p value \n")
  }
  else
  {
    cat("\n corr test: t value, p value \n"); 
    if(is.matrix(X)) var=1:p
    else var=names(X);
    for(i in 1:(p-1))
    {
       for(j in (i+1):p) #cat(i,j,round(cor.test(X[,i],X[,j])$stat,4),round(cor.test(X[,i],X[,j])$p.value,4),"\n");
       cat(' ',var[i],'-',var[j],cor.test(X[,i],X[,j])$stat,cor.test(X[,i],X[,j])$p.value,"\n")
    }
  }
} #corr.test(y,F)
 
reg.plot<-function(fm)
{
  p=ncol(fm$model);
  if(p==2){ plot(fm$model[,2],fm$model[,1],
           xlab=names(fm$model[2]),ylab=names(fm$model[1])); abline(fm,col='red');} 
  else{ plot(rownames(fm$model),fm$model[,1],type='p',xlab='i',ylab='.y,-y^');
       lines(rownames(fm$model),fm$fit); }
}

coef.sd<-function(fm) #计算标准回归系数函数
{
    b=fm$coeff; 
    p=length(b);
    si=apply(fm$model[,2:p],2,sd); sy=sd(fm$model[,1]);
    b1=b[2:p]*(si/sy); 
    #cat("标准回归系数: ", round(b1,4));
    list(coef.sd=b1)
}
   

H.clust<-function(X,d="euc",m="comp",proc=FALSE,plot=TRUE)
{
  D=dist(X,d)
  hc <- hclust(D,m)            
  #if(proc){ cat("\n cluster procdure: \n"); print(cbind(hc$merge,hc$height)) }
  PROC=cbind(merge=hc$merge,height=hc$height)
  if(proc) print(PROC)
  if(plot) plot(hc,ylab=d,main=m)    
  #plot(hc,hang=hang,xlab="",ylab="",main="")    
  #hc1=as.dendrogram(hc)
  #plot(hc1,xlab="G",ylab="D",horiz=TRUE) 
  #list(D=D,hc=hc,proc=proc)
  return(hc)
} #C=H.clust(X)

plot.text<-function(X,h=0,v=0)
{
   plot(X);abline(h=h,v=v,lty=3)
   text(X,label=rownames(X),pos=1.1,adj=0.5,cex=0.85) 
}

princomp.rank<-function(PCA,m,plot=FALSE)
{
  W=as.matrix(PCA[[1]]^2/sum(PCA[[1]]^2))
  PCs=as.matrix(PCA$scores[,1:m])          
  PC=PCs%*%W[1:m]/sum(W[1:m])  
  #print(PC)
  ans=cbind(PCs,'PC'=PC[,1],'rank'=rank(-PC[,1]))
  #cat("\n"); print(ans)
  if(plot) {
   plot(PCs);abline(h=0,v=0,lty=3)
   text(PCs,label=rownames(PCs),pos=1.1,adj=0.5,cex=0.85) 
  }
  return(ans)
} #princomp.rank(PCA,2,T)

###princomp.analy<-function(X,m=ncol(X),cor=TRUE,Var=TRUE,rank=FALSE,plot=FALSE,biplot=FALSE)
factanal.rank<-function(Fac,plot=FALSE)
{
  Fs=Fac$scores          
  W=apply(Fac$loadings^2,2,sum)
  Wi=W/sum(W);
  F=Fs%*%Wi  
  #cat("\n"); print(cbind('F'=F[,1],'rank'=rank(-F[,1])))
  Ri=data.frame('F'=F,'rank'=rank(-F))
  if(plot)
  {
     plot(Fs);abline(h=0,v=0,lty=3)
     text(Fs,label=rownames(Fs),pos=1.1,adj=0.5,cex=0.85) 
  }
  #common=apply(Fac$loadings^2,1,sum);
  list(Fs=Fs,Ri=Ri)
}#Fac=factanal(X,2,rot="varimax",scores="regression")

#Factor Analysis for Princomp
factpc<-function(X, m=2,rotation="none",scores="regression")
{  
   options(digits=4)
   S=cor(X); 
   p<-nrow(S); diag_S<-diag(S); sum_rank<-sum(diag_S)
   #rowname<-paste("X", 1:p, sep="")
   rowname = names(X)
   colname<-paste("Factor", 1:p, sep="")
   A<-matrix(0, nrow=p, ncol=p, dimnames=list(rowname, colname))
   eig<-eigen(S)
   for (i in 1:p)
      A[,i]<-sqrt(eig$values[i])*eig$vectors[,i]
   for (i in 1:p) { if(sum(A[,i])<0) A[,i] = -A[,i] }
   h<-diag(A%*%t(A))
   rowname<-c("SS loadings", "Proportion Var", "Cumulative Var")
   B<-matrix(0, nrow=3, ncol=p, dimnames=list(rowname, colname))
   for (i in 1:p){
     B[1,i]<-sum(A[,i]^2)
     B[2,i]<-B[1,i]/sum_rank
     B[3,i]<-sum(B[1,1:i])/sum_rank
   }
   #cat("\nFactor Analysis for Princomp: \n\n");
   #cat("\n"); print(B[,1:m]);
   W=B[2,1:m]*100; 
   Vars=cbind('Vars'=B[1,],'Vars.Prop'=B[2,],'Vars.Cum'=B[3,]*100)
   #cat("\n"); print(Vars[1:m,])
   #cat("\n"); print(A[,1:m]);
   A=A[,1:m] 
   #{ cat("\n common and specific \n"); print(cbind(common=h, spcific=diag_S-h)); }
   if(rotation=="varimax")
   {   
       #stop(" factor number >= 2 !")
       cat("\n Factor Analysis for Princomp in Varimax: \n\n");
       VA=varimax(A); A=VA$loadings; 
       s2=apply(A^2,2,sum); 
       k=rank(-s2); s2=s2[k]; 
       W=s2/sum(B[1,])*100; 
       Vars=cbind('Vars'=s2,'Vars.Prop'=W,'Vars.Cum'=cumsum(W))
       rownames(Vars) <- paste("Factor", 1:m, sep="")
       #print(Vars[1:m,])
       A=A[,k]
       for (i in 1:m) { if(sum(A[,i])<0) A[,i] = -A[,i] }
       A=A[,1:m]; 
       colnames(A) <- paste("Factor", 1:m, sep="")
       #cat("\n"); print(A) 
   }
   fit<-NULL
   fit$Vars<-Vars[1:m,]
   fit$loadings <- A
   X=as.matrix(scale(X));
   PCs=X%*%solve(S)%*%A
   #if(scores) cat("\n"); print(PCs)
   fit$scores <- PCs
   #if(rank)
   { 
      W=W/sum(W);
      PC=PCs%*%W;
      #cat("\n"); print(cbind(PCs,'PC'=PC[,1],'rank'=rank(-PC[,1])))
      Ri=data.frame('F'=PC,'Ri'=rank(-PC))
      fit$Rank <- Ri
   }
   #if(plot)
   #{ plot(PCs);abline(h=0,v=0,lty=3); text(PCs,label=rownames(PCs),pos=1.1,adj=0.5,cex=0.85) }
   #if(biplot)
   #{ biplot(PCs,A) } #text(PCs,label=rownames(PCs),pos=1.1,adj=0.5,cex=0.85) 
   common=apply(A^2,1,sum);
   fit$common <- common
   fit
   #list(Vars=B[,1:m],loadings=A,scores=PCs,Ri=Ri,common=common)
} #fa=factpc(X,2)

cancor.test<-function(x,y,plot=FALSE)
{
   x=scale(x); y=scale(y);
   n=nrow(x);p=ncol(x);q=ncol(y);
   ca=cancor(x,y)
   cat("\n"); print(ca);
   #cancor.test(ca$cor,n,p,q)
   r=ca$cor 
   m<-length(r); Q<-rep(0, m); P=rep(0,m); lambda <- 1
   for (k in m:1){
     lambda<-lambda*(1-r[k]^2); 
     Q[k]<- -log(lambda)  
   }
   s<-0; i<-m 
   for (k in 1:m){
     Q[k]<- (n-k+1-1/2*(p+q+3)+s)*Q[k]
     P[k]<-1-pchisq(Q[k], (p-k+1)*(q-k+1))
   }
   cat("cancor test: \n"); print(cbind(r,Q,P))
   if(plot){
      u=as.matrix(x)%*%ca$xcoef
      v=as.matrix(y)%*%ca$ycoef
      plot(u[,1],v[,1],xlab='u1',ylab='v1')
      abline(lm(u[,1]~v[,1]))
  }
}
#cancor.test(XY[,1:3],XY[,4:6],plot=TRUE)

weight=function(B) #B为构造的判断矩阵，按行以向量的形式提供参数
{
	A=matrix(B,nrow=sqrt(length(B)),ncol=sqrt(length(B)),byrow=TRUE)
      #A=t(B);  
	n=ncol(A);
	mul_collect=c(1:n);
	for(i in 1:n)
	   mul_collect[i]=prod(A[i,])
	weight=mul_collect^(1/n);
	weight_one=weight/sum(weight);
	round(weight_one,4)
}#返回权重向量

CI_CR=function(B)
{
      RI=c(0,0,0.58,0.90,1.12,1.24,1.32,1.41,1.45,1.49,1.51)#RI值，为常数
	Wi=weight(B);
	n=length(Wi);
	if(n>2)
	{
           W=matrix(Wi,ncol=1);
           A=matrix(B,nrow=sqrt(length(B)),ncol=sqrt(length(B)),byrow=TRUE)
	AW=A%*%W;
	aw=as.vector(AW);
	la_max=sum(aw/Wi)/n;
	CI=(la_max-n)/(n-1);
	CR=CI/RI[n];
	cat("\n CI=",round(CI,4),"\n")
	cat("\n CR=",round(CR,4),"\n")
	cat("\n la_max=",round(la_max,4),"\n\n")
	if(CR<=0.1)
	{
           cat(" Consistency test is OK！\n");
	   cat("\n Wi: ",round(Wi,4),"\n");
	}
	else 
	{	
	   cat(" Please adjust the judgment matrix! \n")
	   Wi=null;
	   break;
	}
        }
        else if(n<=2){ 
          return(Wi);
        }
}#判断一致性，通过判断则返回权重向量

z_score=function(B,converse=FALSE)  #converse为T或TRUE时，采用逆向指标，默认为F
{
   B=as.vector(B);
   if(converse==FALSE||converse==F||converse=="")
   {
	min_value=min(B);
	max_value=max(B);
	z_score=(B-min_value)/(max_value-min_value)*60+40;
	z_score;
   }
   else if(converse==TRUE||converse==T)
   {
	min_value=min(B);
	max_value=max(B);
	
	z_score=(max_value-B)/(max_value-min_value)*60+40;
	z_score;
   }
} #无量纲化，返回某指标的分值

z_data=function(data,converse=FALSE)
{
	n=ncol(data);
	m=nrow(data);
	score_array=array(1:(m*n),c(m,n));
	for(i in 1:n)
	{
	   score_array[,i]=z_score(data[,i],converse);
	}
	SCORE=as.matrix(score_array);
	dimnames(SCORE)[1]=dimnames(data)[1];
	dimnames(SCORE)[2]=dimnames(data)[2];
	round(SCORE,4)
}#输出标准化分数，data为读取的数据

S_rank=function(data,Wi) #计算最终的加权评分
{
   wight_matrix=matrix(Wi,ncol=1,byrow=FALSE);
   score_matrix=as.matrix(data);
   Si=score_matrix%*%wight_matrix;
   print(data.frame(Si=Si,ri=rank(-Si)))
   #list(Si=Si,ri=rank(-Si))
   list(Si=Si)
}#计算加权分值

