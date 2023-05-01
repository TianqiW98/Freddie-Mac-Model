library(plyr)
library(dplyr)

orig<-read.table('/Users/cathyzhang/desktop/sample_orig_2006.txt', sep = "|",
                 col.names=c("fico","dt_first_pi", "flag_fthb", "dt_matr", "cd_msa",
                             "mi_pct", "cnt_units", "occpy_sts", "cltv", "dti", "orig_upb",
                             "ltv","int_rt", "channel", "ppmt_pnlty", "prod_type",
                             "st", "prop_type", "zipcode", "id_loan", "loan_purpose",
                             "orig_loan_term" , "cnt_borr", "seller_name", "servicer_name", "flag_sc")) 




dim(orig)
head(orig)

head(orig)
orig<-orig[c(1,9,10,11,12,13,20)]

orig1<-orig[c("fico","cltv", "dti", "orig_upb","ltv","int_rt", "id_loan"  )]
orig

dim(orig)
head(orig)

set.seed(2016)
orig_train<-sample_n(orig, 35000)
dim(orig_train)



orig_test<-subset(orig, !id_loan %in% orig_train$id_loan)
dim(orig_test)



svcg<-read.table('/Users/cathyzhang/desktop/sample_svcg_2006.txt', sep = "|",
               col.names=c("id_loan","Period", "Act_endg_upb", "delq_sts", "loan_age",
                           "mths_remng", "repch_flag", "flag_mod", "CD_Zero_BAL", "Dt_zero_BAL", "New_Int_rt",
                           "Amt_Non_Int_Brng_Upb","Dt_Lst_Pi","MI_Recoveries",
                           "Net_Sale_Proceeds","Non_MI_Recoveries", "Expenses","legal_costs", "maint_pres_costs", "taxes_ins_costs", "misc_costs", 
                           "actual_loss", "modcost", "stepmod_ind", "dpm_ind", "eltv"))


dim(svcg)
head(svcg)

orig[1:10,]
svcg[1:10,]



#whole_train<-right_join( svcg, orig_train, "id_loan",suffix = c(".x", ".y")
whole_test<-join( svcg, orig_test, "id_loan", type = "right", match = "all")
whole_train<-join( svcg, orig_train, "id_loan", type = "right", match = "all")
#whole_test<-whole_test[is.na(whole_test$Period)==0,]
#whole_train<-whole_train[is.na(whole_train$Period)==0,]


dim(whole_train)
dim(whole_test)
#dim(whole)



table(whole_train$delq_sts)

whole_train<-whole_train[!whole_train$delq_sts=="R",]
whole_train$dfss<-ifelse(as.numeric(whole_train$delq_sts)<3, 
                         0, ifelse(as.numeric(whole_train$delq_sts)>=3, 1,NA))


table(whole_train$dfss)
hist(as.numeric(whole_train$delq_sts))
hist(whole_train$dfss)



table(whole_train$fico)
hist.default(whole_train$fico, xlim=c(50,1000))
whole_train1<-whole_train[!whole_train$fico==9999,]
hist.default(whole_train1$fico, xlim=c(50,1000))

table(whole_train$Period, whole_train$dfss)


table(whole_train1$int_rt)
hist.default(whole_train1$int_rt)

table(whole_train1$cltv)
whole_train1<-whole_train[!whole_train$cltv==999,]
hist.default(whole_train1$cltv)

table(whole_train1$eltv)
max(whole_train1$eltv)
hist(whole_train1$eltv)


table(whole_train1$ltv)
hist.default(whole_train1$ltv)


table(whole_train1$dti)
whole_train2<-whole_train1[!whole_train1$dti==999,]
table(whole_train2$dti)
hist.default(whole_train2$dti)

table(as.factor(whole_train2$Period))
table(whole_train2$loan_age)

##################################################################

whole_train2<-whole_train2[order( whole_train2$id_loan, whole_train2$Period ),
                           ]

whole_train2$month=substr(as.factor(whole_train2$Period),5,6)
whole_train2$year=substr(as.factor(whole_train2$Period),1,4)
whole_train2[1:20,]

whole_train2$qrt<-ifelse(whole_train2$month %in% c("01", "02", "12"), 
                         1, ifelse(whole_train2$month %in% c("03", "04", "05"), 2, 
                                   ifelse(whole_train2$month %in% c("06", "07", "08"), 3, 
                                          ifelse(whole_train2$month %in% c("09", "10", "11"), 4, NA)))
                         
)


head(whole_train2)
whole_train3<-whole_train2[,c(1,2,3,4,5,11,26:36)]
head(whole_train3)
whole_train3$eltv
sum(is.na(whole_train3$eltv))/length(whole_train3$eltv)
sum(is.na(whole_train3$dfss))/length(whole_train3$dfss)
sum(is.na(whole_train3$loan_age))/length(whole_train3$loan_age)
sum(is.na(whole_train3$New_Int_rt))/length(whole_train3$New_Int_rt)
sum(is.na(whole_train3$fico))/length(whole_train3$fico)
sum(is.na(whole_train3$cltv))/length(whole_train3$cltv)
sum(is.na(whole_train3$ltv))/length(whole_train3$ltv)
sum(is.na(whole_train3$orig_upb))/length(whole_train3$orig_upb)

whole_train4<-whole_train3[,-7]
head(whole_train4)


lag<-function(ts,n,j)
{
  lag_m<-rep(0,(n-1))
  for (i in 1:(n-j))
  {
    lag_m[(i+j)]<-ts[i]
  }
  lag_m
}

df<-as.vector(whole_train4$dfss)
n<-length(df)
whole_train4=mutate(whole_train4,lag_df=lag(df,n,1))
whole_train4$lag_df[1]=0

sz<-dim(whole_train4)



###############################################################
library(data.table)
dt=data.table(whole_train4)

###removes the loans multiple defaults##############################

rollsum<-function(x)
{sum1<-array(NA, length(x))

if(length(x)==1){sum1=0}
else 
{for (i in 1:(length(x)-1))
{sum1[1]<-0
sum1[i+1]<-sum1[i]+x[i+1] }
  sum1
}}



#whole_train4=cbind(whole_train4,roll_sum=rollsum(whole_train4$lag_df))

iden0<-dt[,rollsum(lag_df) ,by =id_loan]

iden2<-iden0[V1>1]
min(iden2$V1)
iden<-subset(whole_train4,  id_loan %in% iden2$id_loan)

whole_train5<- cbind(whole_train4, behind =iden0$V1 )
whole_train6<-subset(whole_train5, behind<1)
#####################################checking 
dt1=data.table(whole_train6)
iden01<-dt1[,rollsum(lag_df) ,by =list(id_loan)]
iden21<-iden01[V1>1]


#whole_train4=cbind(whole_train4,roll_sum=rollsum(whole_train4$lag_df))



x<-table(whole_train6$dfss,  whole_train6$year)
###########################################################
whole_train6$yearqrt=paste(trimws(whole_train6$year), 'q',trimws(whole_train6$qrt),collapse = NULL)

whole_train6<- whole_train6[order(whole_train6$id_loan, whole_train6$Period,decreasing=TRUE),]

whole_train7=whole_train6[!(duplicated(whole_train6[c("yearqrt","id_loan")], fromLast = TRUE)), ]

whole_train7<- whole_train7[order(whole_train7$id_loan, whole_train7$yearqrt),]
#######################################binning
head(whole_train7)
library("woeBinning")
binning <- woe.binning(df=whole_train7, 'dfss', c('loan_age','New_Int_rt','fico','dti','cltv','ltv','int_rt','orig_upb'), min.perc.total=0.05, min.perc.class=0.01, stop.limit=0.1, event.class=1)

woe.binning.table(binning)

woe.binning.plot(binning)


databin_varsadded <- woe.binning.deploy(whole_train7, binning, add.woe.or.dum.var='woe')
dim(whole_train7)
dim(databin_varsadded)
names(databin)
names(databin_varsadded)
###dataout=write.csv(databin_varsadded, '/Users/cathyzhang/desktop/loan binned_2009.csv')


######Transformation########################################################################################

#macro<-read.csv('/Users/cathyzhang/desktop/Macro Variables Qtr Updated_stress.csv')
macro_data<-read.csv('/Users/cathyzhang/desktop/Macro Variables Qtr Updated4.csv')



macro_trans<-function(macro)
{
  n<-dim(macro)[1];
  m<-dim(macro)[2];
  
  lag<-function(ts,n,j)
  {
    lag_m<-rep(0,(n-1))
    for (i in 1:(n-j))
    {
      lag_m[(i+j)]<-ts[i]
    }
    lag_m
    }
  
  names<-colnames(macro);
  name0<-names;
  
  for (i in 2:m)
  {
    for (j in 1:8)
    {
      assign("name1", paste("lag_", j, "_", name0[i], sep=""));
      names=c(names,name1);
    }
  }
  
  
  for (i in 2:m)
  {
    macro=cbind(macro, lag(macro[,i],n,1),lag(macro[,i],n,2),lag(macro[,i],n,3),lag(macro[,i],n,4),
                lag(macro[,i],n,5), lag(macro[,i],n,6),lag(macro[,i],n,7),lag(macro[,i],n,8))};
  
  names(macro)<-names;
  
  for (i in 2:m)
  {
    for (j in 1:8)
    {
      assign("name1", paste("lag_QoQ_",j,"_",names[i],sep="" ));
      assign("name2", paste("lag_QoQ_cr_",j,"_",names[i],sep="" ));
      assign("name3", paste("log_",j,"_",names[i],sep="" ));
      assign("name4", paste("lag_QoQ_df_",j,"_",names[i],sep="" ));
      
      names=c(names,name1, name2, name3, name4);
    }
  }
  
  
  
  l1<-matrix(0,n,(m-1))
  l2<-matrix(0,n,(m-1))
  l3<-matrix(0,n,(m-1))
  l4<-matrix(0,n,(m-1))
  
  for (i in 2:m)
  {
    for (j in 2:n)
    {
      
      l1[j,(i-1)]<-macro[j,i]-macro[(j-1),i]
      l2[j,(i-1)]<-(macro[j,i]/macro[(j-1),i])-1
      l3[j,(i-1)]<-log(macro[j,i])
      l4[j,(i-1)]<-log(macro[j,i]/macro[(j-1),i])
    }
  }
  
  
  
  for (i in 1:(m-1))
  {
    macro=cbind(macro, lag(l1[,i],n,1),  lag(l2[,i],n,1) ,lag(l3[,i],n,1), lag(l4[,i],n,1),
                lag(l1[,i],n,2),  lag(l2[,i],n,2) ,lag(l3[,i],n,2), lag(l4[,i],n,2),
                lag(l1[,i],n,3),  lag(l2[,i],n,3) ,lag(l3[,i],n,3), lag(l4[,i],n,3),
                lag(l1[,i],n,4),  lag(l2[,i],n,4) ,lag(l3[,i],n,4), lag(l4[,i],n,4),
                lag(l1[,i],n,5),  lag(l2[,i],n,5) ,lag(l3[,i],n,5), lag(l4[,i],n,5),
                lag(l1[,i],n,6),  lag(l2[,i],n,6) ,lag(l3[,i],n,6), lag(l4[,i],n,6),
                lag(l1[,i],n,7),  lag(l2[,i],n,7) ,lag(l3[,i],n,7), lag(l4[,i],n,7),
                lag(l1[,i],n,8),  lag(l2[,i],n,8) ,lag(l3[,i],n,8), lag(l4[,i],n,8))
    
  }
  
  
  names(macro)<-names;
  macro$qrt=substr(as.factor(macro$X),7,7);
  macro$year=substr(as.factor(macro$X),1,4)
  macro;
}


macro<-macro_trans(macro_data)
whole<-join(databin_varsadded, macro, by=c("year","qrt"), type = "left", match = "all")
head(whole)

##########################################################
#########################variable selection example


data(state)
statedata <-data.frame(state.x77,row.names=state.abb,check.names=T) 


g <- lm(Life.Exp ~.,data=statedata) 
summary(g)

g <- update(g, .~. -Area) 
summary(g)
g <- update(g, .~. -Illiteracy ) 
summary(g)
g <- update(g, .~.- Income) 
summary(g)
g <- update(g, .~.- Population) 
summary(g)



library("leaps")
regfit_full = regsubsets( Life.Exp ~.,data= statedata)
reg_summary = summary(regfit_full, matrix=T, matrix.logical = T)
summary(regfit_full,adjr2=T)



plot(regfit_full, scale = "r2") 
plot(regfit_full, scale = "adjr2") 
plot(regfit_full, scale = "Cp") 
plot(regfit_full, scale = "bic")



############################################variable selection#################
library(bestglm)

#regfit_full = regsubsets( dfss ~.,data= whole)

head(whole)
whole.best.logistic <- within(whole, {
  id_loan   <- NULL        # Delete
  Period <-NULL
  Act_endg_upb<-NULL
  delq_sts<-NULL
  lag_df<-NULL
  behind<-NULL
  yearqrt<-NULL
  woe.fico.binned<-NULL
  woe.dti.binned<-NULL
  woe.int_rt.binned<-NULL
  woe.New_Int_rt.binned<-NULL
  woe.cltv.binned<-NULL
  woe.ltv.binned<-NULL
  woe.loan_age.binned<-NULL
  woe.orig_upb.binned<-NULL
  orig_upb.binned<-NULL
  fico<-NULL
  dti<-NULL
  int_rt<-NULL
  New_Int_rt<-NULL
  cltv<-NULL
  ltv<-NULL
 # orig_upb<-NULL
  loan_age<-NULL
  ltv.binned<-NULL
  New_Int_rt.binned<-NULL
  X<-NULL
  month<-NULL
  year<-NULL
  qrt<-NULL
})

whole<- whole[is.na(whole$New_Int_rt)==0,]
cor(whole$New_Int_rt, whole$int_rt)
cor(whole$cltv, whole$ltv)
n1<-dim(whole.best.logistic)[2]
head(whole.best.logistic)

whole.best.logistic<-whole.best.logistic[,c(1,3:n1,2)]
head(whole.best.logistic)
dim(whole.best.logistic)

whole.best.logistic$fico.binned <- as.factor(whole.best.logistic$fico.binned)
whole.best.logistic$dti.binned<- as.factor(whole.best.logistic$dti.binned)
whole.best.logistic$int_rt.binned<- as.factor(whole.best.logistic$int_rt.binned)
whole.best.logistic$cltv.binned<- as.factor(whole.best.logistic$cltv.binned)
whole.best.logistic$loan_age.binned<- as.factor(whole.best.logistic$loan_age.binned)
#whole.best.logistic$orig_upb.binned<- as.factor(whole.best.logistic$orig_upb.binned)
whole.best.logistic<- whole.best.logistic[is.na(whole.best.logistic$orig_upb)==0,]

dim(whole.best.logistic)
head(whole.best.logistic)

bic<-seq(1,n1)
for (i in 1:416)
{   bic[i]<- BIC(glm(dfss~whole.best.logistic[,i], data = whole.best.logistic, family = "binomial")) }


BIC(glm(dfss~whole.best.logistic[,1], data = whole.best.logistic, family = "binomial")) 



bic[n1]=0
name<-names(whole.best.logistic)
bic_name<-cbind(bic,name)
bic_name[order(bic),2]
bic_name

write.csv(bic_name, '/Users/cathyzhang/desktop/bic.csv' )

#FulMod <- glm(dfss~     
 #            Unemployment        
  #            +log_1_Unemployment+loan_age.binned
  #            +lag_1_Unemployment +fico.binned       
   #           +lag_QoQ_6_HPI+lag_QoQ_cr_6_HPI
    #          +lag_QoQ_df_6_HPI+lag_QoQ_cr_7_HPI        
    #       +lag_QoQ_7_HPI+lag_QoQ_df_7_HPI       
    #         +log_2_Unemployment+HPI             
    #         +lag_1_HPI+log_1_HPI        
    #         +lag_QoQ_cr_5_HPI+lag_QoQ_5_HPI          
    #         +lag_QoQ_df_5_HPI+lag_2_Unemployment  
    #     +lag_2_HPI+log_2_HPI,  
    #          family=binomial(link="logit"),data=whole.best.logistic)
#summary(FulMod)
#car::vif(FulMod)



FulMod <- glm(dfss~loan_age.binned+Unemployment
              +log_1_Unemployment+fico.binned
              +lag_1_Unemployment+lag_QoQ_6_HPI
              +lag_QoQ_cr_6_HPI +lag_QoQ_df_6_HPI
              +lag_QoQ_cr_7_HPI +lag_QoQ_7_HPI
              +lag_QoQ_df_7_HPI+log_2_Unemployment
              +HPI+lag_1_HPI
              +log_1_HPI+lag_QoQ_cr_5_HPI
              +lag_QoQ_5_HPI+lag_QoQ_df_5_HPI
              +lag_2_Unemployment+lag_2_HPI  ,
              family=binomial(link="logit"),data=whole.best.logistic)
summary(FulMod)
car::vif(FulMod)
fullMod2<-step(FulMod,direction="backward")

####################################
FulMod <- glm(dfss ~ loan_age.binned + Unemployment + log_1_Unemployment + 
                fico.binned + lag_1_Unemployment + lag_QoQ_6_HPI + lag_QoQ_cr_6_HPI + 
                lag_QoQ_cr_7_HPI + lag_QoQ_df_7_HPI + lag_QoQ_cr_5_HPI + 
                lag_QoQ_5_HPI + lag_2_HPI ,
              family=binomial(link="logit"),data=whole.best.logistic)
summary(FulMod)
car::vif(FulMod)



FulMod <- glm(dfss ~ loan_age.binned   + 
                fico.binned + lag_1_Unemployment   + 
                   lag_QoQ_cr_5_HPI,
              family=binomial(link="logit"),data=whole.best.logistic)
summary(FulMod)
car::vif(FulMod)



#fullMod2<-step(FulMod,direction="backward")



FulMod <- glm(dfss ~ loan_age.binned + lag_1_Unemployment + fico.binned   
                      + lag_QoQ_df_5_HPI,  
              family=binomial(link="logit"),data=whole.best.logistic)
summary(FulMod)


car::vif(FulMod)





##############################################################

#########################Final model################################

FulMod <- glm(dfss ~ loan_age.binned + lag_1_Unemployment + fico.binned   
              + lag_QoQ_df_5_HPI  ,  
              family=binomial(link="logit"),data=whole.best.logistic)
summary(FulMod)





##FulMod <- glm(dfss~fico.binned
## +int_rt.binned
##+lag_QoQ_cr_1_Unemployment
##+log_3_PMI,
##family=binomial(link="logit"),data=whole.best.logistic)
##summary(FulMod)


library(cvAUC)

prob=predict(FulMod,type=c("response"))
whole.best.logistic$prob=prob
library(pROC)
g <- roc(dfss ~ prob, data = whole.best.logistic)
plot(g)  
g


cvAUC(whole.best.logistic$prob, whole.best.logistic$dfss, label.ordering = NULL, folds = NULL)


#FulMod <- glm(dfss~as.factor(fico..........binned)+as.factor(dti...binned)
#    +as.factor(int_rt..binned)
#  +lag_3_PMI
#  +lag_QoQ_cr_1_Unemployment
#  +lag_QoQ_3_Unemployment
#+lag_QoQ_cr_3_Unemployment
#  +CIV,
# family=binomial(link="logit"),data=whole.best.logistic)
#summary(FulMod)


#cor(whole.best.logistic$lag_QoQ_cr_1_Unemployment,whole.best.logistic$lag_QoQ_cr_3_Unemployment)




databin<-read.csv('/Users/cathyzhang/desktop/loan out_2009.csv')
library("woeBinning")
binning <- woe.binning(df=databin, 'def', c('loan_age','New_Int_rt','fico','dti','cltv','ltv','int_rt','orig_upb'), min.perc.total=0.05, min.perc.class=0.01, stop.limit=0.1, event.class='bad')
databin_varsadded <- woe.binning.deploy(databin, binning, add.woe.or.dum.var='woe')


dim(databin)
dim(databin_varsadded)
names(databin)
names(databin_varsadded)

dataout=write.csv(databin_varsadded, '/Users/cathyzhang/desktop/loan binned_2009.csv')


##########################################work on the testing dataset###############
whole_test<-whole_test[!whole_test$delq_sts=="R",]
whole_test$dfss<-ifelse(as.numeric(whole_test$delq_sts)<3, 
                        0, ifelse(as.numeric(whole_test$delq_sts)>=3, 1,NA))

table(whole_test$dfss)
whole_test1<-whole_test[!whole_test$fico==9999,]
whole_test2<-whole_test1[!whole_test1$dti==999,]


whole_test2<-whole_test2[order( whole_test2$id_loan, whole_test2$Period ),
                                               ]

whole_test2$month=substr(as.factor(whole_test2$Period),5,6)
whole_test2$year=substr(as.factor(whole_test2$Period),1,4)
whole_test2[1:20,]


whole_test2$qrt<-ifelse(whole_test2$month %in% c("01", "02", "12"), 
                        1, ifelse(whole_test2$month %in% c("03", "04", "05"), 2, 
                                  ifelse(whole_test2$month %in% c("06", "07", "08"), 3, 
                                         ifelse(whole_test2$month %in% c("09", "10", "11"), 4, NA)))
                        
)



#whole_test3<-whole_test2[,c(1,2,3,4,5,11,26:36)]
whole_test4<-whole_test2[,-19]
table(whole_test4$dfss)


df<-as.vector(whole_test4$dfss)


n<-length(df)
whole_test4=mutate(whole_test4,lag_df=lag(df,n,1))
whole_test4$lag_df[1]=0

sz<-dim(whole_test4)



###############################################################
library(data.table)
dt=data.table(whole_test4)
###removes the loans multiple defaults##############################

iden0<-dt[,rollsum(lag_df) ,by =id_loan]

iden2<-iden0[V1>1]
min(iden2$V1)
iden<-subset(whole_test4,  id_loan %in% iden2$id_loan)

whole_test5<- cbind(whole_test4, behind =iden0$V1 )
whole_test6<-subset(whole_test5, behind<1)
#####################################checking 
dt1=data.table(whole_test6)
iden01<-dt1[,rollsum(lag_df) ,by =list(id_loan)]
iden21<-iden01[V1>1]


#whole_train4=cbind(whole_train4,roll_sum=rollsum(whole_train4$lag_df))



x<-table(whole_test6$dfss,  whole_test6$year)
#whole_train4=cbind(whole_train4,roll_sum=rollsum(whole_train4$lag_df))



###########################################################
whole_test6$yearqrt=paste(trimws(whole_test6$year), 'q',trimws(whole_test6$qrt),collapse = NULL)

whole_test6<- whole_test6[order(whole_test6$id_loan, whole_test6$Period,decreasing=TRUE),]

whole_test7=whole_test6[!(duplicated(whole_test6[c("yearqrt","id_loan")], fromLast = TRUE)), ]

whole_test7<- whole_test7[order(whole_test7$id_loan, whole_test7$yearqrt),]
#######################################binning

head(whole_test7)
library("woeBinning")

databin_varsadded_test<- woe.binning.deploy(whole_test7, binning, add.woe.or.dum.var='woe')


whole_test<-join(databin_varsadded_test, macro, by=c("year","qrt"), type = "left", match = "all")



whole.best.logistic_test <- within(whole_test, {
  id_loan   <- NULL        # Delete
  Period <-NULL
  Act_endg_upb<-NULL
  delq_sts<-NULL
  lag_df<-NULL
  behind<-NULL
  yearqrt<-NULL
  fico<-NULL
  dti<-NULL
  int_rt<-NULL
  New_Int_rt<-NULL
  cltv<-NULL
  ltv<-NULL
  orig_upb<-NULL
  loan_age<-NULL
  ltv.binned<-NULL
  New_Int_rt.binned<-NULL
  woe.fico.binned<-NULL
  woe.dti.binned<-NULL
  woe.int_rt.binned<-NULL
  woe.New_Int_rt.binned<-NULL
  woe.cltv.binned<-NULL
  woe.ltv.binned<-NULL
  woe.loan_age.binned<-NULL
  woe.orig_upb.binned<-NULL
  X<-NULL
  month<-NULL
  year<-NULL
})




n1<-dim(whole.best.logistic_test)[2]
whole.best.logistic<-whole.best.logistic_test[,c(2:n1,1)]
ls.str(whole.best.logistic_test)


whole.best.logistic_test$fico.binned <- as.factor(whole.best.logistic_test$fico.binned)
whole.best.logistic_test$dti.binned<- as.factor(whole.best.logistic_test$dti.binned)
whole.best.logistic_test$int_rt.binned<- as.factor(whole.best.logistic_test$int_rt.binned)
whole.best.logistic_test$cltv.binned<- as.factor(whole.best.logistic_test$cltv.binned)
whole.best.logistic_test$loan_age.binned<- as.factor(whole.best.logistic_test$loan_age.binned)
whole.best.logistic_test$orig_upb.binned<- as.factor(whole.best.logistic_test$orig_upb.binned)



FulMod_test<- glm(dfss~lag_1_Unemployment + 
                    loan_age.binned + 
                    fico.binned   + 
                    lag_QoQ_df_5_HPI,
                  family=binomial(link="logit"),data=whole.best.logistic_test)
summary(FulMod_test)
 

##library(Deducer)
library(ggplot2)

library(pROC)
####test_prob = predict(FulMod, newdata = whole.best.logistic, type = "response")
#####test_ = roc(whole.best.logistic$dfss ~ test_prob, plot = TRUE, print.auc = TRUE)

######test_prob_test = predict(FulMod_test, newdata = whole.best.logistic_test, type = "response")
######test_test = roc(whole.best.logistic_test$dfss ~ test_prob_test, plot = TRUE, print.auc = TRUE)


whole.best.logistic_test$pred <- predict(FulMod_test, whole.best.logistic_test, type="response")
#roc <- roc(whole.best.logistic_test$dfss,whole.best.logistic_test$pred) #creates an object with all sorts of diagnostics including sensitivities and specificities
dfss<-whole.best.logistic_test$dfss
pred<-whole.best.logistic_test$pred


library(pROC)
g <- roc(dfss ~ pred, data = whole.best.logistic_test)
plot(g)


cvAUC(whole.best.logistic_test$pred, whole.best.logistic_test$dfss, label.ordering = NULL, folds = NULL)




#################################################Forecasting #####################################

macro_stress<-read.csv('/Users/cathyzhang/desktop/Macro Variables Qtr Updated4_stress.csv')
#macro_base<-read.csv('/Users/cathyzhang/desktop/Macro Variables Qtr Updated4.csv')


rm(whole_train)
rm(whole_train1)
rm(whole_train2)
rm(whole_train3)
rm(whole_train4)
rm(whole_train5)
rm(whole_train6)

macro<-macro_trans(macro_stress)
#macro<-macro_trans(macro_base)

databin_varsadded_test_last<-databin_varsadded[databin_varsadded$Period=="201806",]
databin_varsadded_test_last1<-replace(databin_varsadded_test_last, "Period","201809")
databin_varsadded_test_last2<-replace(databin_varsadded_test_last, "Period","201812")
databin_varsadded_test_last3<-replace(databin_varsadded_test_last, "Period","201903")
databin_varsadded_test_last4<-replace(databin_varsadded_test_last, "Period","201906")
databin_varsadded_test_last1<-replace(databin_varsadded_test_last1, "qrt","3")
databin_varsadded_test_last2<-replace(databin_varsadded_test_last2, "qrt","4")
databin_varsadded_test_last3<-replace(databin_varsadded_test_last3, "qrt","1")
databin_varsadded_test_last4<-replace(databin_varsadded_test_last4, "qrt","2")
databin_varsadded_test_last3<-replace(databin_varsadded_test_last3, "year","2019")
databin_varsadded_test_last4<-replace(databin_varsadded_test_last4, "year","2019")

databin_varsadded_stress=rbind(databin_varsadded, databin_varsadded_test_last1,databin_varsadded_test_last2,
                               databin_varsadded_test_last3, databin_varsadded_test_last4)

table(databin_varsadded_stress$Period)

databin_varsadded_stress<-databin_varsadded_stress[!databin_varsadded_stress$loan_age.binned=="NA",]

whole_test_stress<-join(databin_varsadded_stress, macro, by=c("year","qrt"), type = "left", match = "all")



whole.best.logistic_stress <- within(whole_test_stress, {
  delq_sts<-NULL
  lag_df<-NULL
  behind<-NULL
  yearqrt<-NULL
  woe.fico.binned<-NULL
  woe.dti.binned<-NULL
  woe.int_rt.binned<-NULL
  woe.New_Int_rt.binned<-NULL
  woe.cltv.binned<-NULL
  woe.ltv.binned<-NULL
  woe.loan_age.binned<-NULL
  woe.orig_upb.binned<-NULL
  fico<-NULL
  dti<-NULL
  int_rt<-NULL
  New_Int_rt<-NULL
  cltv<-NULL
  ltv<-NULL
  orig_upb<-NULL
  loan_age<-NULL
  ltv.binned<-NULL
  New_Int_rt.binned<-NULL
  X<-NULL
})



whole.best.logistic_stress$fico.binned <- as.factor(whole.best.logistic_stress$fico.binned)
whole.best.logistic_stress$dti.binned<- as.factor(whole.best.logistic_stress$dti.binned)
whole.best.logistic_stress$int_rt.binned<- as.factor(whole.best.logistic_stress$int_rt.binned)
whole.best.logistic_stress$cltv.binned<- as.factor(whole.best.logistic_stress$cltv.binned)
whole.best.logistic_stress$loan_age.binned<- as.factor(whole.best.logistic_stress$loan_age.binned)
whole.best.logistic_stress$orig_upb.binned<- as.factor(whole.best.logistic_stress$orig_upb.binned)
whole.best.logistic_stress<-whole.best.logistic_stress[!whole.best.logistic_stress$loan_age.binned=="Missing",]

whole.best.logistic_stress$pred <- predict(FulMod, whole.best.logistic_stress, type="response")
whole.best.logistic_stress<-whole.best.logistic_stress[order(whole.best.logistic_stress$id_loan, whole.best.logistic_stress$Period ),]


pred<-as.vector(whole.best.logistic_stress$pred)
n<-length(pred)

##########Test
rm(databin_varsadded_test_last)
rm(databin_varsadded)

umrate<-as.vector(whole.best.logistic_stress$lag_1_Unemployment)
dt1=data.table(whole.best.logistic_stress)
dttest<-dt1[,sum(umrate) ,by =list(Period)]

#tables(whole.best.logistic_stress$pred, whole.best.logistic_stress$Period)

whole.best.logistic_stress<-mutate(whole.best.logistic_stress,lag_pred=lag(pred,n,1), lag_2_pred =lag(pred,n,2), lag_3_pred=lag(pred,n,3))
lag_pred<-as.vector(whole.best.logistic_stress$lag_pred)
lag_2_pred<-as.vector(whole.best.logistic_stress$lag_2_pred)
lag_3_pred<-as.vector(whole.best.logistic_stress$lag_3_pred)




whole.best.logistic_stress<-whole.best.logistic_stress[order( whole.best.logistic_stress$id_loan, whole.best.logistic_stress$Period ),   ]


whole.best.logistic_stress$pdrate<-ifelse(whole.best.logistic_stress$year %in% c("2018") & whole.best.logistic_stress$qrt %in% c("3"), 
                                          pred, ifelse(whole.best.logistic_stress$year %in% c("2018") & whole.best.logistic_stress$qrt %in% c("4"), (1-lag_pred)*pred, 
                                                       ifelse(whole.best.logistic_stress$year %in% c("2019") & whole.best.logistic_stress$qrt %in% c("1"),  (1-lag_2_pred)*(1-lag_pred)*pred,
                                                              ifelse(whole.best.logistic_stress$year %in% c("2019") & whole.best.logistic_stress$qrt %in% c("2"),(1-lag_3_pred)*(1-lag_2_pred)*(1-lag_pred)*pred, NA))))

#max(whole.best.logistic_stress$pdrate)
whole.best.logistic_stress[whole.best.logistic_stress$Period=="201806","pdrate"]



dt3<-data.table(whole.best.logistic_stress)
forecast_base<-dt3[, sum(0.45*Act_endg_upb*pdrate),by=Period]
forecast_base<-na.omit(forecast_base)

forecast<-dt3[, sum(0.45*Act_endg_upb*pdrate),by=Period]
forecast<-na.omit(forecast)


##########Loss forecast plots in R for 4 quarters ##########################
par(mfcol=c(1,1))
plot(c(1,2,3,4), forecast_base$V1,col="green",main="Loss forecasting 4q",type="l",ylim=c(100000,110000), xlab="quarter", ylab="loss")
lines(c(1,2,3,4),forecast$V1,col="red", ylim=c(100000,110000), xlab="quarter")









