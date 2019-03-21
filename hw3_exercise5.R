#Exercise 5

##Questions 5.1
###Import data from two excel forms
M<-data.frame(read.csv("~/Desktop/613/hw3/product.csv",stringsAsFactors=FALSE))
N<-data.frame(read.csv("~/Desktop/613/hw3/demos.csv",stringsAsFactors=FALSE))
###Construct X variables (income and prices)
Xprice<-M[,4:13]
Xnew<-t(matrix(rep(t(Xprice[,1]),each=10),nrow=10,ncol=4470))
Xpricenew<-Xprice-Xnew
X<-M[,2:13]
Y<-N[,2:3]
Data3<-merge(X,Y,by.X="hhid",all.X=TRUE)
Xincome<-t(matrix(rep(t(Data3[,13]),each=10),nrow=10,ncol=4470))
####Construct a decision matrix Ydumy
Ydumy<-matrix(0,nrow=4470,ncol=10)
for (i in 1:10){
  Ydumy[,i]<-as.numeric(M$choice==i)
}
###Construct a likelihood function
Target_mixed<-function(par.,Xincome.=Xincome,Xprice.=Xpricenew,Ydumy.=Ydumy){
  #Seperate parameters into three parts: beta, beta_j and alfa
  alfa<-matrix(rep(c(0,par.[1:9]),each=4470),nrow=4470,ncol=10)
  beta_j<-matrix(rep(c(0,par.[10:18]),each=4470),nrow=4470,ncol=10)
  beta<-par.[19]
  #Construct Vij matrix
  V<-alfa+beta*Xprice.+Xincome.*beta_j
  Pn<-exp(V)
  Pd<-rowSums(Pn)
  P<-log(Pn/Pd)
  Prob<-sum(P*Ydumy.)
  likelihoodmixed<--Prob
  return(likelihoodmixed)
}
###Do optimization
parmixed<-c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5)
beta_mixed<-optim(par=parmixed,Target_mixed)
trans<-beta_mixed$par
beta_f<-as.matrix(trans)
####Print the outcome; 
####Print beta_f: ("alfa1","alfa2","alfa3","alfa4","alfa5","alfa6","alfa7","alfa8","alfa9","beta1","beta2","beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta")
print(beta_f)

##Question 5.2
###Construct decision matrxi:Ydumy
Ydumy2<-matrix(0,nrow=4470,ncol=9)
for (i in 2:10){
  Ydumy2[,i-1]<-as.numeric(M$choice==i)
}
###Construct X variables: Xpricenew2 and Xincome 
Xincome2<-Xincome[,1:9]
Xpricenew2<-as.matrix(data.frame(Xpricenew[,1],Xpricenew[,3:10]))
colnames(Xpricenew2)<-c("PPk_Stk","PFl_Stk", "PHse_Stk", "PGen_Stk", "PImp_Stk", "PSS_Tub", "PPk_Tub", "PFl_Tub","PHse_Tub")
Target_mixed2<-function(par.,Xincome.=Xincome2,Xprice.=Xpricenew2,Ydumy.=Ydumy2){
  #Seperate parameters into three parts: beta, beta_j and alfa
  alfa<-matrix(rep(c(0,par.[1:8]),each=4470),nrow=4470,ncol=9)
  beta_j<-matrix(rep(c(0,par.[9:16]),each=4470),nrow=4470,ncol=9)
  beta<-par.[17]
  #Construct Vij by using mixed logit model
  V<-alfa+beta*Xprice.+Xincome.*beta_j
  #Calculate the likelihood
  Pn<-exp(V)
  Pd<-rowSums(Pn)
  P<-log(Pn/Pd)
  Prob<-sum(P*Ydumy.)
  likelihoodmixed<--Prob
  #Return likelihood
  return(likelihoodmixed)
}
###Do optimization
parmixed2<-c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5)
beta_mixed2<-optim(par=parmixed2,Target_mixed2)
trans2<-beta_mixed2$par
beta_r<-as.matrix(trans2)
###Print out the outcome; 
###print beta_r: ("alfa1","alfa2","alfa3","alfa4","alfa5","alfa6","alfa7","alfa8","beta1","beta2","beta3","beta4","beta5","beta6","beta7","beta8","beta")
print(beta_r)

##Question 5.3
### Calculate MTT test statistics
beta_fnew<-as.matrix(rbind(beta_f[2:9],beta_f[11:18],beta_f[19]))
MTT<-2*(Target_mixed2(beta_fnew)-Target_mixed2(beta_r))
###Print the outcome of the test statistics

##Question 5.4
###Calculate the p-value of MTT test
TEST<-pchisq(MTT,df=17,lower.tail=FALSE)
###Print the test result of MTT test
print(TEST)
###Scince p value of MTT test statistics is 0, and 0<0.05, we reject IIA.
