#***********************************************
#******  ��Ԫͳ�Ʒ�����R���Խ�ģ�����İ棩******
#******  �����ļ�: mvstats4.R"            ******
#******  �����ļ�: mvstats4.xls"          ******
#******  ����ʱ�䣺����� 2016.3.1        ******
#***********************************************
######��������á�------------------------------
rm(list=ls())                          #�����ڴ�
options(digits=4)                      #���λ��
par(mar=c(4,4,2,1),cex=0.75)           #ͼ������
source("mvstats.R") #library(mvstats)  #�����Զ��庯�����

#####��1�� ��Ԫͳ�Ʒ�������#####
X=matrix(1:20,10,2)
X
x=rnorm(100)          #����100����׼��̬�ֲ������
x
hist(x,prob=T)        #��x�������ݵ�ֱ��ͼ�����������ʾƵ��
curve(dnorm(x),add=T) #����ֱ��ͼ��ϵ��ܶȺ������� 

#####��2�� ��Ԫ���ݵ���ѧ���ＰRʹ��#####
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)#����һ������
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)
length(x1)#�����ĳ���
mode(x1)#���ݵ�����
rbind(x1,x2)#���кϲ�
cbind(x1,x2)#���кϲ�
matrix(x1,nrow=3,ncol=4)#����x1���ݴ�������
matrix(x1,nrow=4,ncol=3)#�����������������仯�ľ���
matrix(x1,nrow=4,ncol=3,byrow=T)#�������������еľ���
A=matrix(1:12,nrow=3,ncol=4)#��������
t(A)#�����ת��
A=B=matrix(1:12,nrow=3,ncol=4)#����������ͬ�ľ���
A+B#����ӷ�
A-B#�������
A=matrix(1:12,nrow=3,ncol=4)#��������
B=matrix(1:12,nrow=4,ncol=3)#��������
A%*%B#�����ĳ˻�
A=matrix(1:16,nrow=4,ncol=4)#������������Ⱦ���
diag(A)#��þ���Խ���Ԫ��
diag(diag(A))#���öԽ���Ԫ�ش����ԽǾ���
diag(3)#����3�׵�λ����
A=matrix(rnorm(16),4,4)#��������
A
solve(A)#��������
(A=diag(4)+1)#��������
(A.e=eigen(A,symmetric=T))#����������ֵ����������
A.e$vectors%*%diag(A.e$values)%*%t(A.e$vectors)
#������������U������ֵ����D��ԭ����A�Ĺ�ϵA=UDU'
(A.c=chol(A))#�����Choleskey�ֽ�
t(A.c)%*%A.c#Choleskey�ֽ����V��ԭ����A.c�Ĺ�ϵA.c=V'V
(A=matrix(1:18,3,6))#��������
(A.s=svd(A))#���������ֵ�ֽ�
A.s$u%*%diag(A.s$d)%*%t(A.s$v)#���������ֵ�ֽ�����ԭ����A�Ĺ�ϵA=UDV'
(A=matrix(1:16,4,4))#��������
qr(A)#�����QR�ֽ�
(A=matrix(1:4,2,2))#��������
(B=matrix(rep(1,4),2,2))#��������
kronecker(A,B)#�����kronecker��
A=matrix(1:12,3,4)#��������
dim(A)#�����ά��
nrow(A)#���������
ncol(A)#���������
rowSums(A)#���������
rowMeans(A)#���������ֵ
colSums(A)#���������
colMeans(A)#���������ֵ
apply(A,1,sum)#���������
apply(A,1,mean)#���������ֵ
apply(A,2,sum)#���������
apply(A,2,mean)#���������ֵ
A=matrix(rnorm(100),20,5)#��������
apply(A,2,var)#�������󷽲�
B=matrix(1:12,3,4);B
apply(B,2,function(x,a) x*a,a=2)  #�������������

(X=data.frame(x1,x2))#������X1��X2���������ݿ�
(X=data.frame('����'=x1,'����'=x2))#�������ݿ��µ��б�ǩ
(X=read.table("textdata.txt"))#��ȡ��Ϊtextdata��txt��ʽ�ĵ�
(X=read.table("textdata.txt",header=T))
#��ȡ�����б�ǩ����Ϊtextdata��txt��ʽ�ĵ�
X=read.csv("textdata.csv")#��ȡ��Ϊtextdata��csv��ʽ�ĵ�

hist(x1) #�������ߵ�ֱ��ͼ
plot(x1,x2) #�������ߺ����ص�ɢ��ͼ
data=read.table("clipboard",header=T) #�����а����ݶ������ݿ�data��
head(data)#��ʾǰ6������
attach(data)#������
table(����)#һά������
barplot(table(����),col=1:7)#����ͼ
pie(table(���))#��ͼ
table(����,�Ա�) #��ά������
barplot(table(����,�Ա�),beside=T,col=1:7)#���Ա�����������ͼ
barplot(table(�Ա�,����),beside=T,col=1:2)#�����������Ա���ͼ
ftable(����,�Ա�,���) #�����䡢�Ա����еĽ��Ƶ����ά������
ftable(�Ա�,����,���)#���Ա��������еĽ��Ƶ����ά������
(ft=ftable(�Ա�,���,����))#��ʾ���Ա𡢽�����е�����Ƶ����ά������
rowSums(ft)#���к�
colSums(ft)#���к�
sum(ft)#���ܺ�

#####��3�� ���Ԫ���ݵ�ֱ�۱�ʾ��Rʹ��#####
d3.1=read.table("clipboard",header=T);d3.1 
#�����а����ݶ������ݿ�d3.1��   
barplot(apply(d3.1,1,mean),las=3)#��������ֵ����ͼ
barplot(apply(d3.1,2,mean))#��������ֵͼ����ͼ
barplot(apply(d3.1[,2:8],2,mean))#ȥ����һ�к�����ݰ�������ֵ����ͼ
barplot(apply(d3.1,2,median))#��������λ������ͼ
pie(apply(d3.1,2,mean))#��������ֵ��ͼ
boxplot(d3.1)#����������ͼ
boxplot(d3.1,horizontal=T)#����ͼ��ͼ�ΰ�ˮƽ����
stars(d3.1,full=T,key.loc=c(13,1.5))                #����ͼ����360������ͼ
stars(d3.1,full=F,key.loc=c(13,1.5))                #����ͼ����180������ͼ
stars(d3.1,full=T,draw.segments=T,key.loc=c(13,1.5))#����ͼ����360�Ȳ�ɫԲ������ͼ
stars(d3.1,full=F,draw.segments=T,key.loc=c(13,1.5))#����ͼ����180�Ȳ�ɫԲ������ͼ
library(aplpack)#����aplpack��
faces(d3.1,ncol.plot=7)#��ÿ��7��������ͼ
faces(d3.1[,2:8],ncol.plot=7)#ȥ����һ��������ÿ��7��������ͼ
faces(d3.1[c(1,9,19,28,29,30),])#ѡ���1,9,19,28,29,30���۲�Ķ�Ԫ����������ͼ
library(mvstats)#����mvstats��
plot.andrews(d3.1)#���Ƶ�������ͼ
plot.andrews(d3.1[c(1,9,19,28,29,30),])
#ѡ���1,9,19,28,29,30���۲�Ķ�Ԫ��������������ͼ

#####��4�� ��Ԫ�����ع������Rʹ��#####
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)#����
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)#����
plot(x1,x2)#��ɢ��ͼ
lxy<-function(x,y) sum(x*y)-sum(x)*sum(y)/length(x)  #���������˻��ͺ���
lxy(x1,x1)#x1�������ƽ����
lxy(x2,x2)#x1�������ƽ����
lxy(x1,x2)#x1�������˻���
(r=lxy(x1,x2)/sqrt(lxy(x1,x1)*lxy(x2,x2)))#��ʾ�������˻��ͼ�������ϵ��
cor(x1,x2)#�������ϵ��
n=length(x1)#�����ĳ���
tr=r/sqrt((1-r^2)/(n-2))#���ϵ���������tͳ����
tr
cor.test(x1,x2)#���ϵ���������
x=x1#�Ա���,����������2.2
y=x2#�����,����������2.2
b=lxy(x,y)/lxy(x,x)#���Իع鷽��б��
a=mean(y)-b*mean(x)#���Իع鷽�̽ؾ�
c(a=a,b=b)#��ʾ���Իع鷽�̹���ֵ
plot(x,y)#��ɢ��ͼ
lines(x,a+b*x)#���ӹ��Ʒ�����
SST=lxy(y,y)#������������ƽ����
SSR=b*lxy(x,y)#�ع�ƽ����
SSE=SST-SSR#���ƽ����
MSR=SSR/1#�ع����
MSE=SSE/(n-2)#������
F= MSR/MSE#Fͳ����
c(SST=SST,SSR=SSR,SSE=SSE,MSR=MSR,MSE=MSE,F=F)#��ʾ���
sy.x=sqrt(MSE)#���Ʊ�׼��
sb=sy.x/sqrt(lxy(x,x))#�����ƽ����
t=b/sb#tͳ����
ta=qt(1-0.05/2,n-2)#t��λ��
c(sy.x=sy.x,sb=sb,t=t,ta=ta)#��ʾ���
yx=read.table("clipboard",header=T)#������4.3���� 
fm=lm(y~x,data=yx)#һԪ���Իع�ģ��
fm
summary(lm(x2~x1))
plot(y~x,data=yx)#��ɢ��ͼ
abline(fm)#���ӻع���
anova(fm)#ģ�ͷ������
summary(fm)#�ع�ϵ��t����

yX=read.table("clipboard",header=T)#������4.4���� 
plot(yX,gap=0)
(fm=lm(y~x1+x2+x3+x4,data=yX))#��ʾ��Ԫ���Իع�ģ��
coef.sd(fm)#��׼��ƫ�ع�ϵ�����
anova(fm)#��Ԫ���Իع�ģ�ͷ������
summary(fm)#��Ԫ���Իع�ϵ��t����
summary(fm)$fstat
cor(yX)#��Ԫ�������ϵ������
pairs(yX)#��Ԫ����ɢ��ͼ
corr.test(yX)#��Ԫ�������ϵ������
(R2=summary(fm)$r.sq)#��ʾ��Ԫ���Իع�ģ�;���ϵ��
(R=sqrt(R2))#��ʾ��Ԫ���ݸ����ϵ��
library(leaps)#����leaps��
varsel=regsubsets(y~x1+x2+x3+x4,data=yX)#��Ԫ�������Իع����ѡ��ģ��
result=summary(varsel)#����ѡ�񷽷����           
data.frame(result$outmat,RSS=result$rss,R2=result$rsq)#RSS�;���ϵ��׼����չʾ 
data.frame(result$outmat,adjR2=result$adjr2,Cp=result$cp,BIC=result$bic)
#��������ϵ��,Cp��BIC׼����չʾ
fm=lm(y~x1+x2+x3+x4)#��Ԫ�������Իع�ģ��
fm.step=step(fm,direction="forward")#��ǰ���뷨����ѡ����
fm.step=step(fm,direction="backward")#����޳�������ѡ����
fm.step=step(fm,direction="both")#��ɸѡ������ѡ����
summary(lm(y~x1+x2+x3+I(x3^2)+x4+I(x4^2),data=yX))

#####��5�� ���弰һ����ģ�ͼ�Rʹ��#####
d5.1=read.table("clipboard",header=T)#��ȡ��5.1���� 
logit.glm<-glm(y~x1+x2+x3,family=binomial,data=d5.1)#Logistic�ع�ģ��
summary(logit.glm)#Logistic�ع�ģ�ͽ��
logit.step<-step(logit.glm,direction="both")#��ɸѡ������ѡ��
summary(logit.step)#��ɸѡ������ѡ����
pre1<-predict(logit.step,data.frame(x1=1))#Ԥ����������˾��Logistic�ع���
p1<-exp(pre1)/(1+exp(pre1))#Ԥ����������˾�������¹ʸ���
pre2<-predict(logit.step,data.frame(x1=0))#Ԥ�������������˾��Logistic�ع���
p2<-exp(pre2)/(1+exp(pre2))#Ԥ�������������˾�������¹ʸ���
c(p1,p2)#�����ʾ
d5.2=read.table("clipboard",header=T)#��ȡ��5.2���� 
log.glm<-glm(y~x1+x2,family=poisson(link=log),data=d5.2)#��Ԫ��������ģ��
summary(log.glm)#��Ԫ��������ģ�ͽ��
d5.3=read.table("clipboard",header=T)#��ȡ��5.3���� 
anova(lm(Y~factor(A),data=d5.3))#��ȫ������ģ�ͷ������
d5.4=read.table("clipboard",header=T)#��ȡ��5.4����
anova(lm(Y~factor(A)+factor(B),data=d5.4))#�����λ�����ģ�ͷ������

#####��6�� �б������Rʹ��#####
d6.1=read.table("clipboard",header=T)#��ȡ��6.1���� 
attach(d6.1)#������
plot(x1,x2)
text(x1,x2,G,adj=-0.5)#��ʶ���������G
library(MASS)
(ld=lda(G~x1+x2))#�����б�ģ��
Z=predict(ld)#���������б�ģ��Ԥ���������
newG=Z$class#Ԥ������������
cbind(G,Z$x,newG)#��ʾ���
(tab=table(G,newG))#��������        
sum(diag(prop.table(tab)))#�ж���   
predict(ld,data.frame(x1=8.1,x2=2.0)) #�ж�
detach(d6.1)
     
d6.2=read.table("clipboard",header=T)#��ȡ��6.2���� 
d6.2
attach(d6.2)#������
par(mar=c(4,4,2,1),cex=0.75) 
plot(Q,C);text(Q,C,G1,adj=-0.5)
plot(Q,P);text(Q,P,G1,adj=-0.5)
plot(C,P);text(C,P,G1,adj=-0.5)
library(MASS)
qd=qda(G1~Q+C+P);qd
cbind(G1,newG=predict(qd)$class)
predict(qd,data.frame(Q=8,C=7.5,P=65))

ld=lda(G1~Q+C+P);ld
W=predict(ld)
cbind(G1,Wx=W$x,newG=W$class)
predict(ld,data.frame(Q=8,C=7.5,P=65)) #�ж�
options(digits=3)

d6.3=read.table("clipboard",header=T)#��ȡ��6.3���� 
attach(d6.3)#������
plot(Q,C);text(Q,C,G2,adj=-0.5,cex=0.75)
plot(Q,P);text(Q,P,G2,adj=-0.5,cex=0.75)        
plot(C,P);text(C,P,G2,adj=-0.5,cex=0.75)        

ld=lda(G2~Q+C+P); ld
Z=predict(ld)
newG=Z$class
cbind(G2,round(Z$x,3),newG)
(tab=table(G2,newG))
diag(prop.table(tab,1))
sum(diag(prop.table(tab)))
plot(Z$x)
text(Z$x[,1],Z$x[,2],G2,adj=-0.8,cex=0.75)
predict(ld,data.frame(Q=8,C=7.5,P=65)) #�ж�

qd=qda(G2~Q+C+P); qd
Z=predict(qd)
newG=Z$class
cbind(G2,newG)
(tab=table(G2,newG))
sum(diag(prop.table(tab)))
predict(qd,data.frame(Q=8,C=7.5,P=65)) #�ж�

(ld1=lda(G2~Q+C+P,prior=c(1,1,1)/3))#���������ȵ�Bayes�б�ģ��
(ld2=lda(G2~Q+C+P,prior=c(5,8,7)/20))#������ʲ���ȵ�Bayes�б�ģ��        
Z1=predict(ld1)#Ԥ���������        
cbind(G2,round(Z1$x,3),newG=Z1$class)#��ʾ��� 
Z2=predict(ld2)#Ԥ���������
cbind(G2,round(Z2$x,3),newG=Z2$class)#��ʾ���
table(G2,Z1$class)#��������
table(G2,Z2$class)#��������
round(Z1$post,3) #ld1ģ�ͺ������
round(Z2$post,3) #ld2ģ�ͺ������
predict(ld1,data.frame(Q=8,C=7.5,P=65))  # ld1ģ�͵��ж�
predict(ld2,data.frame(Q=8,C=7.5,P=65))  # ld2ģ�͵��ж�

#####��7�� ���������Rʹ��#####
x1=c(5,7,3,6,6)
x2=c(7,1,2,5,6)
plot(x1,x2)
text(x1,x2,names=c(1:5),adj=-0.5) 
X=cbind(x1,x2)
dist(X)#Ĭ��Ϊeuclidean����
dist(X,diag=TRUE)#�������Խ��߾���
dist(X,method="manhattan")#manhattan����
dist(X,method="minkowski",p=1)#manhattan����
dist(X,upper=TRUE)#���������Ǿ���        
dist(X,method="minkowski",p=2)#euclidean����
hc<-hclust(dist(X),"single")#��̾��뷨
cbind(hc$merge,hc$height)#�������
plot(hc)#����ͼ
hc<-hclust(dist(X),"ward")#ward���뷨 
cbind(hc$merge,hc$height)#�������
plot(hc)#����ͼ        
d7.2=read.table("clipboard",header=T) 
plot(d7.2)
library(mvstats)
H.clust(d7.2,"euclidean","single",plot=T)#��̾��뷨
H.clust(d7.2,"euclidean","complete",plot=T)#����뷨
H.clust(d7.2,"euclidean","median",plot=T)#�м���뷨 
H.clust(d7.2,"euclidean","average",plot=T)#��ƽ����        
H.clust(d7.2,"euclidean","centroid",plot=T)#���ķ�        
H.clust(d7.2,"euclidean","ward",plot=T)#ward��
x1=matrix(rnorm(1000,mean=0,sd=0.3),ncol=10)#��ֵ1,��׼��Ϊ0.3��100x10����̬���������
x2=matrix(rnorm(1000,mean=1,sd=0.3),ncol=10) 
x=rbind(x1,x2)
H.clust(x,"euclidean","complete")
cl=kmeans(x,2)#kmeans����
pch1=rep("1",100)
pch2=rep("2",100)
plot(x,col=cl$cluster,pch=c(pch1,pch2),cex=0.7)
points(cl$centers,col=3,pch="*",cex=3)
x1=matrix(rnorm(10000,mean=0,sd=0.3),ncol=10)#��ֵ1,��׼��Ϊ0.3��1000x10����̬���������
x2=matrix(rnorm(10000,mean=1,sd=0.3),ncol=10) 
x=rbind(x1,x2)
cl=kmeans(x,2)#kmeans����
pch1=rep("1",1000)
pch2=rep("2",1000)
plot(x,col=cl$cluster,pch=c(pch1,pch2),cex=0.7)
points(cl$centers,col=3,pch ="*",cex=3)
#####��8�� ���ɷַ�����Rʹ��#####
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)
plot(x1,x2,xlim=c(145,180),ylim=c(25,75))
lines(c(150,178),c(33,66));text(180,68,"y1")
lines(c(161,168),c(60,38));text(161,63,"y2")      
X=read.table("clipboard",header=T) # ��7.2����
cor(X)
PCA=princomp(X,cor=T)#���ɷַ���
PCA#����ֵ�����Ž�� 
options(digits=3)
summary(PCA)
PCA$loadings#���ɷ��غ�
par(mar=c(4,4,2,1),cex=0.75)
screeplot(PCA,type="lines")        
PCA$scores[,1:2]  #���ɷֵ÷�  
      
source("mvstats.R") #library(mvstats)
princomp.rank(PCA,m=2)#���ɷ�����
princomp.rank(PCA,m=2,plot=T)#���ɷ���������ͼ

#####��9�� ���ӷ�����Rʹ��#####
X=read.table("clipboard",header=T)#��ȡ��9.1����
cor(X)
(FA0=factanal(X,3,rot="none"))#������Ȼ�����ӷ���
library(mvstats)
(Fac=factpc(X,3))#���ɷݷ����ӷ���
(Fa1=factanal(X,3,rot="varimax")) #varimax����ת���ӷ���
Fa1=factanal(X,3,scores="regression")#ʹ�ûع���Ʒ��ļ�����Ȼ�����ӷ���
Fa1$scores
Fac1=factpc(X,3,scores="regression")#ʹ�ûع���Ʒ������ɷݷ����ӷ���
Fac1$scores
factanal.rank(Fa1,plot=T)#���ӵ÷���ͼ������
biplot(Fa1$scores,Fa1$loadings)#ǰ2��������Ϣ�ص�ͼ        

X=read.table("clipboard",header=T)#��ȡ��7.2����       
library(mvstats)
Fac0=factpc(X,3)#���ӷ���
Fac0$Vars#���������
Fac1=factpc(X,3,rot="varimax")#������ת���ӷ���
Fac1$Vars#���������
Fac0$loadings#�����غ�        
Fac1$loadings#�����غ�        
Fac1$scores#���ӵ÷�        
Fac1$Rank#����        
par(mar=c(4,4,2,1)+0.1,cex=0.75)
plot(Fac1$scores,ylim=c(-3,3)); abline(h = 0, v = 0, lty = 3)
text(Fac1$scores,label=rownames(X),pos=1,adj=0.5,cex=0.8)

plot.text(Fac1$scores)#���ӵ÷�ͼ
biplot(Fac1$scores,Fac1$loading)#��Ϣ�ص�ͼ        

#####��10�� ��Ӧ������Rʹ��#####
X=read.table("clipboard",header=T)#��ȡ��10.1����
chisq.test(X)#��������
library(MASS)#����MASS��        
ca1=corresp(X,nf=2)#��Ӧ����        
ca1#��Ӧ�������
par(mar=c(4,4,3,1),cex=0.8)
biplot(ca1)#˫������ͼ
#abline(v=0,h=0,lty=3)#��������
X=read.table("clipboard",header=T)#ѡȡ��10.2����        
ca2=corresp(X,nf=2)#��Ӧ����        
ca2#��Ӧ�������
biplot(ca2)#˫������ͼ
#abline(v=0,h=0,lty=3)#��������
plot(cars)
#####��11�� ������ط�����Rʹ��#####
X=read.table("clipboard",header=T)#��ȡ��11.1����
(R=cor(X))
R11=R[1:3,1:3]
R12=R[1:3,4:6]
R21=R[4:6,1:3]
R22=R[4:6,4:6]
A=solve(R11)%*%R12%*%solve(R22)%*%R21#��һ�������Ӧ�ľ���        
ev=eigen(A)$values# ����ֵ
ev        
sqrt(ev)#�������ϵ��
xy=scale(X)#���ݱ�׼�� 
ca=cancor(xy[,1:3],xy[,4:6])#������ط��� 
ca$cor#�������ϵ�� 
ca$xcoef#��һ������ĵ����غ�
ca$ycoef#�ڶ�������ĵ����غ�
library(mvstats)        
cancor.test(xy[,1:3],xy[,4:6],plot=T)#������ط�����������ͼ

d11.2=read.table("clipboard",header=T)#ѡȡ��11.2����
cancor.test(d11.2[,1:4],d11.2[5:10],plot=T)#������ط�����������ͼ    
   
#####��12�� ��ά��ȷ�MDS��Rʹ��#####
D=read.table("clipboard",header=T)
library(MASS)
D=as.matrix(D)     
fit=isoMDS(D,k=2) 
x=fit$points[,1]
y=fit$points[,2]
plot(x,y,type="n")
text(x,y,labels=row.names(D))
X=read.table("clipboard",header=T)
d=dist(X) 
fit=isoMDS(d,k=2)
x=fit$points[,1]
y=fit$points[,2]
plot(x,y);abline(v=0,h=0,lty=3)
text(x,y,labels=row.names(X)) 

#####��13�� �ۺ����۷�����Rʹ��#####
library(mvstats)#����mvstats��
A=c(1,3,7,1/3,1,3,1/7,1/3,1)#������жϾ���
(A_W=weight(A))#A��Ȩ��
CI_CR(A)#һ���Լ���
B1data=read.table("clipboard",header=T)#ѡȡ��13.1��A-G������
B1_z=z_data(B1data)#���������ٻ�z=(x-max)/(max-min)*60+40
B1_z
Si=apply(B1_z,1,mean)#�������ֵ
cbind(B1_z,Si) 
cbind(Si=Si,ri,rank(-Si))#��Siֵ�ߵ�����
B1=c(1,4,5,3,6,7,1/4,1,2,1/2,3,4,1/5,1/2,1,1/3,2,3,1/3,  2,  3,  1,  4,5,
1/6,1/3,1/2,1/4,1,2,1/7,1/4,1/3,1/5,1/2,1)#����B1���жϾ���
B1_W=weight(B1)#B1��Ȩ��
B1_W
CI_CR(B1)#һ���Լ���
S_rank(B1_Z,B1_W)#��B1�õ��ۺϵ÷ּ�����
B2=c(1,4,5,7,8,9,1/4,1,2,4,5,6,1/5,1/2,1,3,4,5,1/7,1/4,1/3,1,2,3,1/8,1/5,
1/4,1/2,1,2,1/9,1/6,1/5,1/3,1/2,1)#����B2���жϾ���
B2_W=weight(B2)#B2��Ȩ��
B2_W
CI_CR(B2)#һ���Լ���
B3=c(1,5,2,6,2,6,1,1/5,1,1/4,2,1/4,2,0.2,1/2,5,1,5,1,5,1/2,1/6,1/2,1/5,
1,1/5,1,1/6,1/2,4,1,5,1,5,1/2,1/6,1/2,1/5,1,1/5,1,1/6,1,5,2,2,2,6,1)#����B3���жϾ���
B3_W=weight(B3)#B3��Ȩ��
B3_W
CI_CR(B3)#һ���Լ���
data=read.table("clipboard",header=T)#ѡȡ��13.1����
x1=data[,1:6]#B1������
x2=data[,7:12]#B2������
x3=data[,13:19]#B3������
S1=S_rank(z_data(x1),B1_W)#��B1�õ��ۺϵ÷ּ�����
S2=S_rank(z_data(x2),B2_W)#��B2�õ��ۺϵ÷ּ�����
S3=S_rank(z_data(x3),B3_W)#��B3�õ��ۺϵ÷ּ�����
S=cbind(S1$Si,S2$Si,S3$Si)#�γɵ÷�����
S_rank(S,A_W)#��A�õ��ۺϵ÷ּ�����