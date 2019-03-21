#Exercise 3
#install.packages("mlogit")
#install.packages("stargazer")
#install.packages("texreg")
library(zoo)
library(Formula)
library(dplyr)
library(lmtest)
library(mlogit)
## Question 3.1
### Since we are interested in the effect of family income on demand, and family income does change across alternatives
### Download data from two excel forms
M<-data.frame(read.csv("~/Desktop/613/hw3/product.csv",stringsAsFactors=FALSE))
N<-data.frame(read.csv("~/Desktop/613/hw3/demos.csv",stringsAsFactors=FALSE))

### Merge two data frame to a new data frame and take out income variable as X variable
X<-M[,2:13]
Y<-N[,2:3]
Data3<-merge(X,Y,by.X="hhid",all.X=TRUE)
Xincome<-t(matrix(rep(t(Data3[,13]),each=10),nrow=10,ncol=4470))
###Construct a decision matrix named Ydumy
Ydumy<-matrix(0,nrow=4470,ncol=10)
for (i in 1:10){
  Ydumy[,i]<-as.numeric(Data3$choice==i)
}
### Construct the likelihood function of multinomial logit model 
Target_multinomial<-function(par1.,X.=Xincome,Ydumy.=Ydumy){
  ####Construct Vij matrix
  alfa<-matrix(rep(c(0,par1.[1:9]),each=4470),nrow=4470,ncol=10)
  beta<-matrix(rep(c(0,par1.[10:18]),each=4470),nrow=4470,ncol=10)
  V<-alfa+beta*X.
  ####Construct a probability matrix
  Pn<-exp(V)
  Pd<-rowSums(Pn)
  P<-log(Pn/Pd)
  Prob<-sum(P*Ydumy)
  ####Return -likelihood
  likelihoodmul<--Prob
  return(likelihoodmul)
}
###Do optimization
parmultilogit<-c(-0.6,-0.6,-0.6,-0.6,-0.6,-0.6,-0.6,-0.6,-0.6,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1)
beta_multilogit<-optim(par=parmultilogit,Target_multinomial)$par
### Calculate the likelihood by using parmultilogit as initial value
like<-Target_multinomial(beta_multilogit)
print(like)
###Store beta into an excel form
beta_mulogit<-as.matrix(beta_multilogit)
###Print the outcome: (alfa_1,alfa_2,alfa_3,alfa_4,alfa_5,alfa_6,alfa_7,alfa_8,alfa_9,beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9)
print(beta_mulogit)
### Write parameters into excel
write.csv(beta_mulogit,"~/Desktop/613/hw3/beta_multilogit.csv")

##Question 3.3
###Since beta_1,beta_2,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9 are all negative，it is indicated that compared with the product 1, the higher income a household has, the less likely it is for him to buy product 2,3,5,6,7,8,9,10. 
###Since all alfa_j (j=1,2,3,4,5,6,7,8,9) are negative，it is indicated that given an individual (given fixed income), compared with the product 1, all other products (product 2,product 3,product 4,product 5,product 6,product 7,product 8,product 9,product 10) are less preferred 
