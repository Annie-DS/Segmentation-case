#IMPORTING DATASET IN R
ccgeneral1<-read.csv(file='D:\\BA/CC GENERAL.csv',header = T)

str(ccgeneral1)
names(ccgeneral1)

# CREATING NEW VARIABLES
ccgeneral1$m_avg_purchase<-ccgeneral1$PURCHASES/ccgeneral1$TENURE

ccgeneral1$m_cash_advance<-ccgeneral1$CASH_ADVANCE/ccgeneral1$TENURE

ccgeneral1$purchase_type<-as.numeric(ifelse(ccgeneral1$INSTALLMENTS_PURCHASES==0 & ccgeneral1$ONEOFF_PURCHASES!=0,
                                            1,ifelse(ccgeneral1$ONEOFF_PURCHASES==0 & ccgeneral1$INSTALLMENTS_PURCHASES!=0,2 ,ifelse(
                                              ccgeneral1$ONEOFF_PURCHASES==0 & ccgeneral1$INSTALLMENTS_PURCHASES==0,3,ifelse(ccgeneral1$INSTALLMENTS_PURCHASES!=0 & 
                                                                                                                               ccgeneral1$ONEOFF_PURCHASES!=0,4,'')))))

ccgeneral1$average_cashadv_trx<-ifelse(ccgeneral1$CASH_ADVANCE_TRX==0,0,ccgeneral1$CASH_ADVANCE/ccgeneral1$CASH_ADVANCE_TRX)

ccgeneral1$average_purchase_trx<-ifelse(ccgeneral1$PURCHASES_TRX==0,0,ccgeneral1$PURCHASES/ccgeneral1$PURCHASES_TRX)

ccgeneral1$limit_usage<-ifelse(ccgeneral1$CREDIT_LIMIT==0,0,ccgeneral1$BALANCE/ccgeneral1$CREDIT_LIMIT)

ccgeneral1$pay_ratio<-ifelse(ccgeneral1$MINIMUM_PAYMENTS==0,0,ccgeneral1$PAYMENTS/ccgeneral1$MINIMUM_PAYMENTS)

#REMOVING REDUNDANT VARIABLES
cc<-which(colnames(ccgeneral1) %in% c("CUST_ID","purchase_type"))
ccgeneral<-ccgeneral1[-cc]

#USER DEFINE FUNCTION
mystats<- function(x){
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  mean<-mean(a)
  s<-sd(a)
  n<-length(a)
  min<-min(a)
  pctls<-quantile(a,probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max<-max(a)
  return(c(n=n,nmiss=nmiss,mean=mean,stdev=s,min=min,pctls=pctls,max=max))
}
vars<-c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES" ,         
        "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY"
        ,"CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS"
        ,"MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE","m_avg_purchase","m_cash_advance",                  
        "average_cashadv_trx" ,"limit_usage","pay_ratio","average_purchase_trx")

options(scipen = 999)

diag_stats<-t(data.frame(apply(ccgeneral[vars],2,mystats)))

#OUTLIERS TREATMENT
ccgeneral$BALANCE[ccgeneral$BALANCE>9338.804814]<-9338.804814
ccgeneral$BALANCE[ccgeneral$BALANCE<0.06510059000]<-0.06510059000
ccgeneral$BALANCE_FREQUENCY[ccgeneral$BALANCE_FREQUENCY<0.09090900000]<-0.09090900000
ccgeneral$PURCHASES[ccgeneral$PURCHASES>8977.290000]<-8977.290000
ccgeneral$ONEOFF_PURCHASES[ccgeneral$ONEOFF_PURCHASES>6689.898200]<-6689.898200
ccgeneral$INSTALLMENTS_PURCHASES[ccgeneral$INSTALLMENTS_PURCHASES>3886.240500]<-3886.240500
ccgeneral$CASH_ADVANCE[ccgeneral$CASH_ADVANCE>9588.163357]<-9588.163357
ccgeneral$CASH_ADVANCE_TRX[ccgeneral$CASH_ADVANCE_TRX>29.000000]<-29.000000
ccgeneral$PURCHASES_TRX[ccgeneral$PURCHASES_TRX>116.510000]<-116.510000
ccgeneral$CREDIT_LIMIT[ccgeneral$CREDIT_LIMIT>17000]<-17000
ccgeneral$CREDIT_LIMIT[ccgeneral$CREDIT_LIMIT<500.00000000000]<-500.00000000000
ccgeneral$PAYMENTS[ccgeneral$PAYMENTS>13608.715541]<-13608.715541
ccgeneral$MINIMUM_PAYMENTS[ccgeneral$MINIMUM_PAYMENTS>9034.098737]<-9034.098737
ccgeneral$MINIMUM_PAYMENTS[ccgeneral$MINIMUM_PAYMENTS<19.52447752000]<-19.52447752000
ccgeneral$m_avg_purchase[ccgeneral$m_avg_purchase>748.107500]<-748.107500
ccgeneral$m_cash_advance[ccgeneral$m_cash_advance>799.013613]<-799.013613
ccgeneral$average_cashadv_trx[ccgeneral$average_cashadv_trx>1909.183420]<-1909.183420
ccgeneral$average_purchase_trx[ccgeneral$average_purchase_trx>756.992100]<-756.992100
ccgeneral$pay_ratio[ccgeneral$pay_ratio>50.405830]<-50.405830
ccgeneral$pay_ratio[ccgeneral$pay_ratio<0.05582549959]<-0.05582549959
ccgeneral$limit_usage[ccgeneral$limit_usage>1.057061]<-1.057061
ccgeneral$limit_usage[ccgeneral$limit_usage<0.00002840431]<-0.00002840431


#MIISING VALUE IMPUTATION
ccgeneral$MINIMUM_PAYMENTS[is.na(ccgeneral$MINIMUM_PAYMENTS)]<-mean(ccgeneral$MINIMUM_PAYMENTS,na.rm = T)
ccgeneral$CREDIT_LIMIT[is.na(ccgeneral$CREDIT_LIMIT)]<-mean(ccgeneral$CREDIT_LIMIT,na.rm = T)
ccgeneral$limit_usage[is.na(ccgeneral$limit_usage)]<-mean(ccgeneral$limit_usage,na.rm = T)
ccgeneral$pay_ratio[is.na(ccgeneral$pay_ratio)]<-mean(ccgeneral$pay_ratio,na.rm = T)

#SCALING
final_data<-ccgeneral[vars]
final_data<-data.frame(scale(final_data))


#FACTOR ANALYSIS

corrm<-cor(final_data)
require(psych)
require(GPArotation)
scree(corrm,factors = T,pc=F,main='Scree plot',hline = NULL,add=F)
eigen(corrm)$values
require(dplyr)
eigen_values<-mutate(data.frame(eigen(corrm)$values),cum_sum_eigen=cumsum(eigen.corrm..values),
                     pct_var=eigen.corrm..values/sum(eigen.corrm..values),
                     cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

FA <- fa(r=corrm,8,rotate = "varimax",fm="ml")
FA_sort<-fa.sort(FA)
ls(FA_sort)
FA_sort$loadings
Loadings<-data.frame(FA_sort$loadings[1:ncol(final_data),])
write.csv(Loadings,"\\Users\\Abhishek\\Desktop\\results/loadings1.csv")

vars1<- c("ONEOFF_PURCHASES",
          "PURCHASES",
          "PAYMENTS",
          "average_purchase_trx",
          "CREDIT_LIMIT",
          "PURCHASES_INSTALLMENTS_FREQUENCY",
          "PURCHASES_FREQUENCY",
          "average_cashadv_trx",
          "limit_usage",
          "PRC_FULL_PAYMENT",
          "CASH_ADVANCE_FREQUENCY")

#CLUSTER ANALYSIS
final_data_vars1<-final_data[vars1]
cluster_three<-kmeans(final_data_vars1,3)
cluster_four<-kmeans(final_data_vars1,4)
cluster_five<-kmeans(final_data_vars1,5)
cluster_six<-kmeans(final_data_vars1,6)

ccgeneral_new<-cbind(ccgeneral,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster
                     ,km_clust_5=cluster_five$cluster,km_clust_6=cluster_six$cluster)

#PROFILING

#CONVERTING INTO FACTORS
ccgeneral_new$km_clust_3<-factor(ccgeneral_new$km_clust_3)
ccgeneral_new$km_clust_4<-factor(ccgeneral_new$km_clust_4)
ccgeneral_new$km_clust_5<-factor(ccgeneral_new$km_clust_5)
ccgeneral_new$km_clust_6<-factor(ccgeneral_new$km_clust_6)

install.packages('tables')
require(tables)

profile<-tabular(1+BALANCE + BALANCE_FREQUENCY + PURCHASES + ONEOFF_PURCHASES + INSTALLMENTS_PURCHASES +        
                   CASH_ADVANCE + PURCHASES_FREQUENCY + ONEOFF_PURCHASES_FREQUENCY + PURCHASES_INSTALLMENTS_FREQUENCY +
                   CASH_ADVANCE_FREQUENCY + CASH_ADVANCE_TRX + PURCHASES_TRX + CREDIT_LIMIT + PAYMENTS +
                   MINIMUM_PAYMENTS + PRC_FULL_PAYMENT + TENURE + m_avg_purchase + m_cash_advance +                  
                   average_cashadv_trx + limit_usage + pay_ratio + average_purchase_trx ~ mean +
                   (mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),
                 data=ccgeneral_new)

profile1<-data.frame(as.matrix(profile))

profile<-tabular(1~length + (length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=ccgeneral_new)

profile2<-data.frame(as.matrix(profile))

write.csv(profile1,"C:\\Users\\Abhishek\\Desktop\\results/profile1.csv")
write.csv(profile2,"C:\\Users\\Abhishek\\Desktop\\results/profile2.csv")
#
