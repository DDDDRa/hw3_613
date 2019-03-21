
#Question 2
##Q2.1
### Since we want to know the effect of price on demands and prices are variables(regressors) which will not change across different choices, we shall use conditional logit model

##Q2.2
###Download the data from two excel forms
M<-data.frame(read.csv("~/Desktop/613/hw3/product.csv",stringsAsFactors=FALSE))
N<-data.frame(read.csv("~/Desktop/613/hw3/demos.csv",stringsAsFactors=FALSE))
###Calculate X matrix and do normalization
Xprice<-M[,4:13]
Xnew<-t(matrix(rep(t(Xprice[,1]),each=10),nrow=10,ncol=4470))
Xpricenew<-Xprice-Xnew
### Construct a decision matrix named Ydumy to demonstrate all individuals' choices
Ydumy<-matrix(0,nrow=4470,ncol=10)
for (i in 1:10){
  Ydumy[,i]<-as.numeric(M$choice==i)
}
###Construct a likelihood funciton of the conditional logit model
Targetcondlogit<-function(par.,X.=Xpricenew,Ydumy.=Ydumy){
  #### Construct a alfa matrix by using par.
  A0<-matrix(0,nrow=4470,ncol=1)
  A1<-matrix(par.[2],nrow=4470,ncol=1)
  A2<-matrix(par.[3],nrow=4470,ncol=1)
  A3<-matrix(par.[4],nrow=4470,ncol=1)
  A4<-matrix(par.[5],nrow=4470,ncol=1)
  A5<-matrix(par.[6],nrow=4470,ncol=1)
  A6<-matrix(par.[7],nrow=4470,ncol=1)
  A7<-matrix(par.[8],nrow=4470,ncol=1)
  A8<-matrix(par.[9],nrow=4470,ncol=1)
  A9<-matrix(par.[10],nrow=4470,ncol=1)
  A<-as.matrix(data.frame(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9))
  #### Calculate V
  V<-X.*par.[1]+A
  #### Calculate probability matrix
  Pd<-exp(V)
  Pn<-rowSums(Pd)
  P<-Pd/Pn
  P<-log(P)
  #### Take out all the probabilities that are applied (be chosen) and calculate -likelihood of the conditional logit model
  test1<-P*Ydumy.
  likelihood_log<--sum(test1)
  return(likelihood_log)
}
###Do optimization
par<-c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5)
beta_conditionlogit<-optim(par=par,Targetcondlogit)
beta_condlogit<-as.matrix(beta_conditionlogit$par)
#Print the outcome: beta,alfa1,alfa2,alfa3,alfa4,alfa5,alfa6,alfa7,alfa8,alfa9
print(beta_condlogit)
### Write the data into excel form
write.csv(beta_condlogit,"~/Desktop/613/hw3/beta_condilogit.csv")

## Question 2.3
### Because beta is negative, which indicates that the higher the price it is, the less utility that an individual will have by choosing the product, and the less likely it is that an individual is going to choose the product.
### alfa1, alfa3, alfa4, alfa5,alfa9 are all negative, which indicates that compared with the product 1 (PPk_Stk), product 2,4,5,6,10 ( PBB_Stk, PHse_Stk,PGen_Stk,PImp_Stk,PHse_Tub) are less preferred and thus are less likely to be chosen given the same price. 
### alfa2, alfa6,alfa7, alfa8, are all positive, which indicates that compare with the product 1 (PPk_Stk), product 3,7,8,9 (PFl_Stk,PSS_Tub,PPk_Tub,PFl_Tub) are more preferred and thus they are more likely to be chosen given the same price.
