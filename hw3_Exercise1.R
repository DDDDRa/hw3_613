library(dplyr)
#Exercise 1
##Question 1.1
###Download the data from excel forms
M<-data.frame(read.csv("~/Desktop/613/hw3/product.csv",stringsAsFactors=FALSE))
N<-data.frame(read.csv("~/Desktop/613/hw3/demos.csv",stringsAsFactors=FALSE))
###Set two empty vector to place mean and standard deviation of 10 products
mean<-c()
disp<-c()
###Calculate mean and standard deviation of 10 products
for(i in 4:13){
  mean1<-mean(M[,i])
  disp1<-sqrt(var(M[,i]))
  mean<-c(mean,mean1)
  disp<-c(disp,disp1)
}
#Print out the outcomes
mean<-as.matrix(mean)
disp<-as.matrix(disp)
rownames(mean)<-c("PPk_Stk_mean","PBB_Stk_mean","PFl_Stk_mean","PHse_Stk_mean","PGen_Stk_mean","PImp_Stk_mean","PSS_Tub_mean","PPk_Tub_mean","PFl_Tub_mean","PHse_Tub_mean")
rownames(disp)<-c("PPk_Stk_disp","PBB_Stk_disp","PFl_Stk_disp","PHse_Stk_disp","PGen_Stk_disp","PImp_Stk_disp","PSS_Tub_disp","PPk_Tub_disp","PFl_Tub_disp","PHse_Tub_disp")
print(mean)
print(disp)

##Question 1.2
###Construct a matrix called revenue to get each individual's choice in buying product at a certain price.
###The sume of the matrix "Revenue" is the total revenue of the market.
####Construct three matrix to place market shares regarding different classification
by_category<-matrix(0,nrow=1,ncol=2)
colnames(by_category)<-c("marketshare_stk","marketshrae_tub")
by_brand<-matrix(0,nrow=1,ncol=7)
colnames(by_brand)<-c("marketshare_PPk","marketshare_PBB","marketshare_PFl","marketshare_PHse","marketshare_PGen","marketshare_PImp","marketshare_PSS")
by_product<-matrix(0,nrow=1,ncol=10)
colnames(by_product)<-c("PPk_Stk_mean","PBB_Stk_mean","PFl_Stk_mean","PHse_Stk_mean","PGen_Stk_mean","PImp_Stk_mean","PSS_Tub_mean","PPk_Tub_mean","PFl_Tub_mean","PHse_Tub_mean")

####Construct a Price Matrix named Xpriceq1
Xpriceq1<-M[,4:13]
####Construct a decision matrix named Ydumy to present all individual's choices
Ydumy<-matrix(0,nrow=4470,ncol=10)
for (i in 1:10){
  Ydumy[,i]<-as.numeric(M$choice==i)
}
####Get the Revenue matrix
Revenue<-Xpriceq1*Ydumy

###Calculate the market share by product
by_product<-t(as.matrix(apply(Revenue,2,FUN = sum)))
by_product<-by_product/sum(by_product)
####Print out the outcome of the market shrae by product
print(by_product)

###Calcualte the market share by brands
by_brand[1,1]<-by_product[1,1]+by_product[1,8]
by_brand[1,2]<-by_product[1,2]
by_brand[1,3]<-by_product[1,3]+by_product[1,9]
by_brand[1,4]<-by_product[1,4]+by_product[1,10]
by_brand[1,5]<-by_product[1,5]
by_brand[1,6]<-by_product[1,6]
by_brand[1,7]<-by_product[1,7]
by_brand<-by_brand/sum(by_brand)
####Print out the outcome of the market share by brands
print(by_brand)

###Calculate the market share by category
by_category[1,1]<-sum(by_product[1,1:6])
by_category[1,2]<-sum(by_product[1,7:10])
by_category<-by_category/sum(by_category)
####Print out the outcome of the market share by category
print(by_category)  


##Question 1.3
###Construct a decision matrix named Ydumy, which demonstrates all individuals' choices
Ydumy<-matrix(0,nrow=4470,ncol=10)
for (i in 1:10){
  Ydumy[,i]<-as.numeric(M$choice==i)
}
X<-M[,2:13]
Y<-N[,2:9]
Dataq1<-merge(X,Y,by.X ="hhid",all.X=TRUE)
###Find income categories (14 types of income)
income_category<-as.matrix(unique(Dataq1[,13]))

###Mapping between income and choices
income_choice<-matrix(0,nrow=14,ncol=10)
for (i in 1:4470){
  for (j in 1:14){
    if(Dataq1[i,13]==income_category[j,1]){
      k<-Dataq1[i,2]
      income_choice[j,k]<-income_choice[j,k]+1
      }
  }
}
###Report a matrix named income_choice
print(income_choice)