#Exercise 4
##Question 4.1: Compute the marginal effect of the conditional logit model (Marginal Effect at Average Value) 
###Import data from excel forms and merge data to a new form named Data3
M<-data.frame(read.csv("~/Desktop/613/hw3/product.csv",stringsAsFactors=FALSE))
N<-data.frame(read.csv("~/Desktop/613/hw3/demos.csv",stringsAsFactors=FALSE))
X<-M[,2:13]
Y<-N[,2:3]
Data3<-merge(X,Y,by.X="hhid",all.X=TRUE)
Dataq4<-as.matrix(apply(Data3,2,FUN = mean))
###Construct a matrix to place variable:price and do normalization
Xq4condlogit<-t(Dataq4[3:12])
Xq4condlogit1<-matrix(rep(Xq4condlogit[1,1],each=10),nrow=1,ncol=10)
Xq4condlogit<-Xq4condlogit-Xq4condlogit1
###Calcualte marginal effect of conditional logit modle by using a function
condmeq4<-function(par.,X.=Xq4condlogit){
  alfa<-matrix(c(0,par.[2:10]),nrow=1,ncol=10)
  ####Construct the Vij matrix
  V<-X.*par.[1]+alfa
  ####Calculate probability and get Probability matrix
  Pn<-exp(V)
  Pd<-rowSums(Pn)
  Ppre<-Pn/Pd
  ### Calculate marginal effect
  P<-matrix(rep(Ppre,each=10),nrow=10,ncol=10)
  Pt<-t(P)
  I<-diag(10)
  me<-P*(I-Pt)*par.[1]
  return(me)
}

###Import beta and alfa_j of exercise 2 from an excel form
beta_conlogitq4<-read.csv("~/Desktop/613/hw3/beta_condilogit.csv")
beta_conlogq4<-c(beta_conlogitq4[,2])
###Get marginal effect at average value of conditional logit modle
me_conlogit<-condmeq4(beta_conlogq4)
###Print the outcome
print(me_conlogit)

###Interpret the marginal effect at average value: 
###It is easily found that in the marginal effect at average value matrix, the values in the diagonal are all negative, but other values are positive.
###This indicates that the increase in the corresponding component of the price variable for kth alternative increases the probability of the kth alternative and decrease the probability of the other alternatives  

##Question 4.2: Compute the marginal effect of multinomial logit model(Average Marginal Effect)
###Construct a matrix to place income variable
Xincomeq4<-t(matrix(rep(t(Data3[,13]),each=10),nrow=10,ncol=4470))
###Calculate marginal effect by using a function
multmeq4<-function(beta.,X.=Xincomeq4){
  ####Construct a probability matrix (4470*10)
  alfa<-matrix(rep(c(0,beta.[1:9]),each=4470),nrow=4470,ncol=10)
  beta<-matrix(rep(c(0,beta.[10:18]),each=4470),nrow=4470,ncol=10)
  ####Calculate Vij matrix
  V<-alfa+beta*X.
  ####Calculate the probability matrix
  Pn<-exp(V)
  Pd<-rowSums(Pn)
  P<-Pn/Pd
  ####Construct a beta_bar matrix
  betatest<-as.matrix(rowSums(P*beta))
  beta_bar<-t(matrix(rep(t(betatest),each=10),nrow=10,ncol=4470))
  ####Calculate marginal effect of multinomial logit model
  betadiff<-beta-beta_bar
  me<-P*betadiff
  return(me)
}
###Import beta_j and alfa_j of exercise 3 from an excel form 
beta_mullogita4<-read.csv("~/Desktop/613/hw3/beta_multilogit.csv")
beta_mullogq4<-c(beta_mullogita4[,2])
memultq4<-multmeq4(beta_mullogq4)
amemultq4<-t(as.matrix(apply(memultq4,2,FUN = mean)))
###Print the outcome: average marginal effect 
print(amemultq4)

###Interpret the average marginal effect of multinomial logit modle
### It is find that the average marginal effect of choice one and choice three on income is positive, and average marginal effects of all other choices on income are negative. It is indicated that the higher the income of an individual it is, the more likely he is to choose product 1 and product 4, the less likely he is to choose product 2,3,5,6,7,8,9,10.