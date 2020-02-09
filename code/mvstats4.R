#***********************************************
#******  多元统计分析及R语言建模（第四版）******
#******  代码文件: mvstats4.R"            ******
#******  数据文件: mvstats4.xls"          ******
#******  作者时间：王斌会 2016.3.1        ******
#***********************************************
######【输出设置】------------------------------
rm(list=ls())                          #清理内存
options(digits=4)                      #结果位数
par(mar=c(4,4,2,1),cex=0.75)           #图形修饰
source("mvstats.R") #library(mvstats)  #加载自定义函数或包

#####第1章 多元统计分析概述#####
X=matrix(1:20,10,2)
X
x=rnorm(100)          #产生100个标准正态分布随机数
x
hist(x,prob=T)        #做x这组数据的直方图，其中纵轴表示频率
curve(dnorm(x),add=T) #添加直方图拟合的密度函数曲线 

#####第2章 多元数据的数学表达及R使用#####
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)#创建一个向量
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)
length(x1)#向量的长度
mode(x1)#数据的类型
rbind(x1,x2)#按行合并
cbind(x1,x2)#按列合并
matrix(x1,nrow=3,ncol=4)#利用x1数据创建矩阵
matrix(x1,nrow=4,ncol=3)#创建行数列数发生变化的矩阵
matrix(x1,nrow=4,ncol=3,byrow=T)#创建按照行排列的矩阵
A=matrix(1:12,nrow=3,ncol=4)#创建矩阵
t(A)#求矩阵转置
A=B=matrix(1:12,nrow=3,ncol=4)#创建两个相同的矩阵
A+B#矩阵加法
A-B#矩阵减法
A=matrix(1:12,nrow=3,ncol=4)#创建矩阵
B=matrix(1:12,nrow=4,ncol=3)#创建矩阵
A%*%B#求矩阵的乘积
A=matrix(1:16,nrow=4,ncol=4)#创建行列数相等矩阵
diag(A)#获得矩阵对角线元素
diag(diag(A))#利用对角线元素创建对角矩阵
diag(3)#创建3阶单位矩阵
A=matrix(rnorm(16),4,4)#创建矩阵
A
solve(A)#求矩阵的逆
(A=diag(4)+1)#创建矩阵
(A.e=eigen(A,symmetric=T))#求矩阵的特征值与特征向量
A.e$vectors%*%diag(A.e$values)%*%t(A.e$vectors)
#特征向量矩阵U和特征值矩阵D与原矩阵A的关系A=UDU'
(A.c=chol(A))#矩阵的Choleskey分解
t(A.c)%*%A.c#Choleskey分解矩阵V与原矩阵A.c的关系A.c=V'V
(A=matrix(1:18,3,6))#创建矩阵
(A.s=svd(A))#矩阵的奇异值分解
A.s$u%*%diag(A.s$d)%*%t(A.s$v)#矩阵的奇异值分解结果与原矩阵A的关系A=UDV'
(A=matrix(1:16,4,4))#创建矩阵
qr(A)#矩阵的QR分解
(A=matrix(1:4,2,2))#创建矩阵
(B=matrix(rep(1,4),2,2))#创建矩阵
kronecker(A,B)#矩阵的kronecker积
A=matrix(1:12,3,4)#创建矩阵
dim(A)#矩阵的维数
nrow(A)#矩阵的行数
ncol(A)#矩阵的列数
rowSums(A)#矩阵按行求和
rowMeans(A)#矩阵按行求均值
colSums(A)#矩阵按列求和
colMeans(A)#矩阵按列求均值
apply(A,1,sum)#矩阵按行求和
apply(A,1,mean)#矩阵按行求均值
apply(A,2,sum)#矩阵按列求和
apply(A,2,mean)#矩阵按列求均值
A=matrix(rnorm(100),20,5)#创建矩阵
apply(A,2,var)#矩阵按列求方差
B=matrix(1:12,3,4);B
apply(B,2,function(x,a) x*a,a=2)  #矩阵按列求函数结果

(X=data.frame(x1,x2))#产生由X1和X2构建的数据框
(X=data.frame('身高'=x1,'体重'=x2))#赋予数据框新的列标签
(X=read.table("textdata.txt"))#读取名为textdata的txt格式文档
(X=read.table("textdata.txt",header=T))
#读取具有列标签的名为textdata的txt格式文档
X=read.csv("textdata.csv")#读取名为textdata的csv格式文档

hist(x1) #做出身高的直方图
plot(x1,x2) #做出身高和体重的散点图
data=read.table("clipboard",header=T) #将剪切板数据读入数据框data中
head(data)#显示前6组数据
attach(data)#绑定数据
table(年龄)#一维列联表
barplot(table(年龄),col=1:7)#条形图
pie(table(结果))#饼图
table(年龄,性别) #二维列联表
barplot(table(年龄,性别),beside=T,col=1:7)#以性别分组的年龄条图
barplot(table(性别,年龄),beside=T,col=1:2)#以年龄分组的性别条图
ftable(年龄,性别,结果) #以年龄、性别排列的结果频数三维列联表
ftable(性别,年龄,结果)#以性别、年龄排列的结果频数三维列联表
(ft=ftable(性别,结果,年龄))#显示以性别、结果排列的年龄频数三维列联表
rowSums(ft)#求行和
colSums(ft)#求列和
sum(ft)#求总和

#####第3章 多多元数据的直观表示及R使用#####
d3.1=read.table("clipboard",header=T);d3.1 
#将剪切板数据读入数据框d3.1中   
barplot(apply(d3.1,1,mean),las=3)#按行做均值条形图
barplot(apply(d3.1,2,mean))#按列做均值图条形图
barplot(apply(d3.1[,2:8],2,mean))#去掉第一列后的数据按列做均值条形图
barplot(apply(d3.1,2,median))#按列做中位数条形图
pie(apply(d3.1,2,mean))#按列做均值饼图
boxplot(d3.1)#按列做箱线图
boxplot(d3.1,horizontal=T)#箱线图中图形按水平放置
stars(d3.1,full=T,key.loc=c(13,1.5))                #具有图例的360度星相图
stars(d3.1,full=F,key.loc=c(13,1.5))                #具有图例的180度星相图
stars(d3.1,full=T,draw.segments=T,key.loc=c(13,1.5))#具有图例的360度彩色圆形星相图
stars(d3.1,full=F,draw.segments=T,key.loc=c(13,1.5))#具有图例的180度彩色圆形星相图
library(aplpack)#加载aplpack包
faces(d3.1,ncol.plot=7)#按每行7个做脸谱图
faces(d3.1[,2:8],ncol.plot=7)#去掉第一个变量按每行7个做脸谱图
faces(d3.1[c(1,9,19,28,29,30),])#选择第1,9,19,28,29,30个观测的多元数据做脸谱图
library(mvstats)#加载mvstats包
plot.andrews(d3.1)#绘制调和曲线图
plot.andrews(d3.1[c(1,9,19,28,29,30),])
#选择第1,9,19,28,29,30个观测的多元数据做调和曲线图

#####第4章 多元相关与回归分析及R使用#####
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)#身高
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)#体重
plot(x1,x2)#做散点图
lxy<-function(x,y) sum(x*y)-sum(x)*sum(y)/length(x)  #建立离均差乘积和函数
lxy(x1,x1)#x1的离均差平方和
lxy(x2,x2)#x1的离均差平方和
lxy(x1,x2)#x1的离均差乘积和
(r=lxy(x1,x2)/sqrt(lxy(x1,x1)*lxy(x2,x2)))#显示用离均差乘积和计算的相关系数
cor(x1,x2)#计算相关系数
n=length(x1)#向量的长度
tr=r/sqrt((1-r^2)/(n-2))#相关系数假设检验t统计量
tr
cor.test(x1,x2)#相关系数假设检验
x=x1#自变量,数据来自例2.2
y=x2#因变量,数据来自例2.2
b=lxy(x,y)/lxy(x,x)#线性回归方程斜率
a=mean(y)-b*mean(x)#线性回归方程截距
c(a=a,b=b)#显示线性回归方程估计值
plot(x,y)#做散点图
lines(x,a+b*x)#添加估计方程线
SST=lxy(y,y)#因变量的离均差平方和
SSR=b*lxy(x,y)#回归平方和
SSE=SST-SSR#误差平方和
MSR=SSR/1#回归均方
MSE=SSE/(n-2)#误差均方
F= MSR/MSE#F统计量
c(SST=SST,SSR=SSR,SSE=SSE,MSR=MSR,MSE=MSE,F=F)#显示结果
sy.x=sqrt(MSE)#估计标准差
sb=sy.x/sqrt(lxy(x,x))#离均差平方和
t=b/sb#t统计量
ta=qt(1-0.05/2,n-2)#t分位数
c(sy.x=sy.x,sb=sb,t=t,ta=ta)#显示结果
yx=read.table("clipboard",header=T)#加载例4.3数据 
fm=lm(y~x,data=yx)#一元线性回归模型
fm
summary(lm(x2~x1))
plot(y~x,data=yx)#做散点图
abline(fm)#添加回归线
anova(fm)#模型方差分析
summary(fm)#回归系数t检验

yX=read.table("clipboard",header=T)#加载例4.4数据 
plot(yX,gap=0)
(fm=lm(y~x1+x2+x3+x4,data=yX))#显示多元线性回归模型
coef.sd(fm)#标准化偏回归系数结果
anova(fm)#多元线性回归模型方差分析
summary(fm)#多元线性回归系数t检验
summary(fm)$fstat
cor(yX)#多元数据相关系数矩阵
pairs(yX)#多元数据散点图
corr.test(yX)#多元数据相关系数检验
(R2=summary(fm)$r.sq)#显示多元线性回归模型决定系数
(R=sqrt(R2))#显示多元数据复相关系数
library(leaps)#加载leaps包
varsel=regsubsets(y~x1+x2+x3+x4,data=yX)#多元数据线性回归变量选择模型
result=summary(varsel)#变量选择方法结果           
data.frame(result$outmat,RSS=result$rss,R2=result$rsq)#RSS和决定系数准则结果展示 
data.frame(result$outmat,adjR2=result$adjr2,Cp=result$cp,BIC=result$bic)
#调整决定系数,Cp和BIC准则结果展示
fm=lm(y~x1+x2+x3+x4)#多元数据线性回归模型
fm.step=step(fm,direction="forward")#向前引入法变量选择结果
fm.step=step(fm,direction="backward")#向后剔除法变量选择结果
fm.step=step(fm,direction="both")#逐步筛选法变量选择结果
summary(lm(y~x1+x2+x3+I(x3^2)+x4+I(x4^2),data=yX))

#####第5章 广义及一般性模型及R使用#####
d5.1=read.table("clipboard",header=T)#读取例5.1数据 
logit.glm<-glm(y~x1+x2+x3,family=binomial,data=d5.1)#Logistic回归模型
summary(logit.glm)#Logistic回归模型结果
logit.step<-step(logit.glm,direction="both")#逐步筛选法变量选择
summary(logit.step)#逐步筛选法变量选择结果
pre1<-predict(logit.step,data.frame(x1=1))#预测视力正常司机Logistic回归结果
p1<-exp(pre1)/(1+exp(pre1))#预测视力正常司机发生事故概率
pre2<-predict(logit.step,data.frame(x1=0))#预测视力有问题的司机Logistic回归结果
p2<-exp(pre2)/(1+exp(pre2))#预测视力有问题的司机发生事故概率
c(p1,p2)#结果显示
d5.2=read.table("clipboard",header=T)#读取例5.2数据 
log.glm<-glm(y~x1+x2,family=poisson(link=log),data=d5.2)#多元对数线性模型
summary(log.glm)#多元对数线性模型结果
d5.3=read.table("clipboard",header=T)#读取例5.3数据 
anova(lm(Y~factor(A),data=d5.3))#完全随机设计模型方差分析
d5.4=read.table("clipboard",header=T)#读取例5.4数据
anova(lm(Y~factor(A)+factor(B),data=d5.4))#随机单位组设计模型方差分析

#####第6章 判别分析及R使用#####
d6.1=read.table("clipboard",header=T)#读取例6.1数据 
attach(d6.1)#绑定数据
plot(x1,x2)
text(x1,x2,G,adj=-0.5)#标识点所属类别G
library(MASS)
(ld=lda(G~x1+x2))#线性判别模型
Z=predict(ld)#根据线性判别模型预测所属类别
newG=Z$class#预测的所属类别结果
cbind(G,Z$x,newG)#显示结果
(tab=table(G,newG))#混淆矩阵        
sum(diag(prop.table(tab)))#判对率   
predict(ld,data.frame(x1=8.1,x2=2.0)) #判定
detach(d6.1)
     
d6.2=read.table("clipboard",header=T)#读取例6.2数据 
d6.2
attach(d6.2)#绑定数据
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
predict(ld,data.frame(Q=8,C=7.5,P=65)) #判定
options(digits=3)

d6.3=read.table("clipboard",header=T)#读取例6.3数据 
attach(d6.3)#绑定数据
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
predict(ld,data.frame(Q=8,C=7.5,P=65)) #判定

qd=qda(G2~Q+C+P); qd
Z=predict(qd)
newG=Z$class
cbind(G2,newG)
(tab=table(G2,newG))
sum(diag(prop.table(tab)))
predict(qd,data.frame(Q=8,C=7.5,P=65)) #判定

(ld1=lda(G2~Q+C+P,prior=c(1,1,1)/3))#先验概率相等的Bayes判别模型
(ld2=lda(G2~Q+C+P,prior=c(5,8,7)/20))#先验概率不相等的Bayes判别模型        
Z1=predict(ld1)#预测所属类别        
cbind(G2,round(Z1$x,3),newG=Z1$class)#显示结果 
Z2=predict(ld2)#预测所属类别
cbind(G2,round(Z2$x,3),newG=Z2$class)#显示结果
table(G2,Z1$class)#混淆矩阵
table(G2,Z2$class)#混淆矩阵
round(Z1$post,3) #ld1模型后验概率
round(Z2$post,3) #ld2模型后验概率
predict(ld1,data.frame(Q=8,C=7.5,P=65))  # ld1模型的判定
predict(ld2,data.frame(Q=8,C=7.5,P=65))  # ld2模型的判定

#####第7章 聚类分析及R使用#####
x1=c(5,7,3,6,6)
x2=c(7,1,2,5,6)
plot(x1,x2)
text(x1,x2,names=c(1:5),adj=-0.5) 
X=cbind(x1,x2)
dist(X)#默认为euclidean距离
dist(X,diag=TRUE)#添加主对角线距离
dist(X,method="manhattan")#manhattan距离
dist(X,method="minkowski",p=1)#manhattan距离
dist(X,upper=TRUE)#添加上三角距离        
dist(X,method="minkowski",p=2)#euclidean距离
hc<-hclust(dist(X),"single")#最短距离法
cbind(hc$merge,hc$height)#分类过程
plot(hc)#聚类图
hc<-hclust(dist(X),"ward")#ward距离法 
cbind(hc$merge,hc$height)#分类过程
plot(hc)#聚类图        
d7.2=read.table("clipboard",header=T) 
plot(d7.2)
library(mvstats)
H.clust(d7.2,"euclidean","single",plot=T)#最短距离法
H.clust(d7.2,"euclidean","complete",plot=T)#最长距离法
H.clust(d7.2,"euclidean","median",plot=T)#中间距离法 
H.clust(d7.2,"euclidean","average",plot=T)#类平均法        
H.clust(d7.2,"euclidean","centroid",plot=T)#重心法        
H.clust(d7.2,"euclidean","ward",plot=T)#ward法
x1=matrix(rnorm(1000,mean=0,sd=0.3),ncol=10)#均值1,标准差为0.3的100x10的正态随机数矩阵
x2=matrix(rnorm(1000,mean=1,sd=0.3),ncol=10) 
x=rbind(x1,x2)
H.clust(x,"euclidean","complete")
cl=kmeans(x,2)#kmeans聚类
pch1=rep("1",100)
pch2=rep("2",100)
plot(x,col=cl$cluster,pch=c(pch1,pch2),cex=0.7)
points(cl$centers,col=3,pch="*",cex=3)
x1=matrix(rnorm(10000,mean=0,sd=0.3),ncol=10)#均值1,标准差为0.3的1000x10的正态随机数矩阵
x2=matrix(rnorm(10000,mean=1,sd=0.3),ncol=10) 
x=rbind(x1,x2)
cl=kmeans(x,2)#kmeans聚类
pch1=rep("1",1000)
pch2=rep("2",1000)
plot(x,col=cl$cluster,pch=c(pch1,pch2),cex=0.7)
points(cl$centers,col=3,pch ="*",cex=3)
#####第8章 主成分分析及R使用#####
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)
plot(x1,x2,xlim=c(145,180),ylim=c(25,75))
lines(c(150,178),c(33,66));text(180,68,"y1")
lines(c(161,168),c(60,38));text(161,63,"y2")      
X=read.table("clipboard",header=T) # 例7.2数据
cor(X)
PCA=princomp(X,cor=T)#主成分分析
PCA#特征值开根号结果 
options(digits=3)
summary(PCA)
PCA$loadings#主成分载荷
par(mar=c(4,4,2,1),cex=0.75)
screeplot(PCA,type="lines")        
PCA$scores[,1:2]  #主成分得分  
      
source("mvstats.R") #library(mvstats)
princomp.rank(PCA,m=2)#主成分排名
princomp.rank(PCA,m=2,plot=T)#主成分排名与作图

#####第9章 因子分析及R使用#####
X=read.table("clipboard",header=T)#读取例9.1数据
cor(X)
(FA0=factanal(X,3,rot="none"))#极大似然法因子分析
library(mvstats)
(Fac=factpc(X,3))#主成份法因子分析
(Fa1=factanal(X,3,rot="varimax")) #varimax法旋转因子分析
Fa1=factanal(X,3,scores="regression")#使用回归估计法的极大似然法因子分析
Fa1$scores
Fac1=factpc(X,3,scores="regression")#使用回归估计法的主成份法因子分析
Fac1$scores
factanal.rank(Fa1,plot=T)#因子得分作图与排名
biplot(Fa1$scores,Fa1$loadings)#前2个因子信息重叠图        

X=read.table("clipboard",header=T)#读取例7.2数据       
library(mvstats)
Fac0=factpc(X,3)#因子分析
Fac0$Vars#方差及贡献率
Fac1=factpc(X,3,rot="varimax")#运用旋转因子分析
Fac1$Vars#方差及贡献率
Fac0$loadings#因子载荷        
Fac1$loadings#因子载荷        
Fac1$scores#因子得分        
Fac1$Rank#排名        
par(mar=c(4,4,2,1)+0.1,cex=0.75)
plot(Fac1$scores,ylim=c(-3,3)); abline(h = 0, v = 0, lty = 3)
text(Fac1$scores,label=rownames(X),pos=1,adj=0.5,cex=0.8)

plot.text(Fac1$scores)#因子得分图
biplot(Fac1$scores,Fac1$loading)#信息重叠图        

#####第10章 对应分析及R使用#####
X=read.table("clipboard",header=T)#读取例10.1数据
chisq.test(X)#卡方检验
library(MASS)#加载MASS包        
ca1=corresp(X,nf=2)#对应分析        
ca1#对应分析结果
par(mar=c(4,4,3,1),cex=0.8)
biplot(ca1)#双坐标轴图
#abline(v=0,h=0,lty=3)#添加轴线
X=read.table("clipboard",header=T)#选取例10.2数据        
ca2=corresp(X,nf=2)#对应分析        
ca2#对应分析结果
biplot(ca2)#双坐标轴图
#abline(v=0,h=0,lty=3)#添加轴线
plot(cars)
#####第11章 典型相关分析及R使用#####
X=read.table("clipboard",header=T)#读取例11.1数据
(R=cor(X))
R11=R[1:3,1:3]
R12=R[1:3,4:6]
R21=R[4:6,1:3]
R22=R[4:6,4:6]
A=solve(R11)%*%R12%*%solve(R22)%*%R21#第一组变量对应的矩阵        
ev=eigen(A)$values# 特征值
ev        
sqrt(ev)#典型相关系数
xy=scale(X)#数据标准化 
ca=cancor(xy[,1:3],xy[,4:6])#典型相关分析 
ca$cor#典型相关系数 
ca$xcoef#第一组变量的典型载荷
ca$ycoef#第二组变量的典型载荷
library(mvstats)        
cancor.test(xy[,1:3],xy[,4:6],plot=T)#典型相关分析及检验作图

d11.2=read.table("clipboard",header=T)#选取例11.2数据
cancor.test(d11.2[,1:4],d11.2[5:10],plot=T)#典型相关分析及检验作图    
   
#####第12章 多维标度法MDS及R使用#####
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

#####第13章 综合评价方法及R使用#####
library(mvstats)#加载mvstats包
A=c(1,3,7,1/3,1,3,1/7,1/3,1)#构造的判断矩阵
(A_W=weight(A))#A的权重
CI_CR(A)#一致性检验
B1data=read.table("clipboard",header=T)#选取例13.1中A-G列数据
B1_z=z_data(B1data)#数据无量纲化z=(x-max)/(max-min)*60+40
B1_z
Si=apply(B1_z,1,mean)#按行求均值
cbind(B1_z,Si) 
cbind(Si=Si,ri,rank(-Si))#按Si值高低排名
B1=c(1,4,5,3,6,7,1/4,1,2,1/2,3,4,1/5,1/2,1,1/3,2,3,1/3,  2,  3,  1,  4,5,
1/6,1/3,1/2,1/4,1,2,1/7,1/4,1/3,1/5,1/2,1)#构造B1的判断矩阵
B1_W=weight(B1)#B1的权重
B1_W
CI_CR(B1)#一致性检验
S_rank(B1_Z,B1_W)#按B1得到综合得分及排名
B2=c(1,4,5,7,8,9,1/4,1,2,4,5,6,1/5,1/2,1,3,4,5,1/7,1/4,1/3,1,2,3,1/8,1/5,
1/4,1/2,1,2,1/9,1/6,1/5,1/3,1/2,1)#构造B2的判断矩阵
B2_W=weight(B2)#B2的权重
B2_W
CI_CR(B2)#一致性检验
B3=c(1,5,2,6,2,6,1,1/5,1,1/4,2,1/4,2,0.2,1/2,5,1,5,1,5,1/2,1/6,1/2,1/5,
1,1/5,1,1/6,1/2,4,1,5,1,5,1/2,1/6,1/2,1/5,1,1/5,1,1/6,1,5,2,2,2,6,1)#构造B3的判断矩阵
B3_W=weight(B3)#B3的权重
B3_W
CI_CR(B3)#一致性检验
data=read.table("clipboard",header=T)#选取例13.1数据
x1=data[,1:6]#B1组数据
x2=data[,7:12]#B2组数据
x3=data[,13:19]#B3组数据
S1=S_rank(z_data(x1),B1_W)#按B1得到综合得分及排名
S2=S_rank(z_data(x2),B2_W)#按B2得到综合得分及排名
S3=S_rank(z_data(x3),B3_W)#按B3得到综合得分及排名
S=cbind(S1$Si,S2$Si,S3$Si)#形成得分数据
S_rank(S,A_W)#按A得到综合得分及排名
