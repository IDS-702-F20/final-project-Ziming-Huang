rm(list=ls())
setwd('D:/IDS 702/final project')
library('readxl')
library("dplyr")
library('ggplot2')
library(rms) #for VIF
library(MASS)
library(GGally)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
require(caTools)
require(gtsummary)



data=read_excel('data/default of credit card clients.xlsx',skip=1,col_names=TRUE)
#remove some data
var_list=c(colnames(data)[3:5])
pay_list=c(colnames(data)[7:12])
data<-data[data$MARRIAGE %in% c(1,2,3) & data$EDUCATION %in% c(1,2,3,4), ]
for (var in pay_list){data=data[data[,var]!=-2,]}

num_list=colnames(data)[c(2,13:24)]
data[,num_list] <- lapply(data[,num_list], numeric)



#factor variables
data[,var_list] <- lapply(data[,var_list] , factor)
data<-rename(data, c("default"="default payment next month"))
data['default_f']<-factor(data$default)
str(data)

#split data
set.seed(123) 
sample = sample.split(data$ID, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)


data0<-data[data$default==0,]
data1<-data[data$default==1,]


#change unit
data$LIMIT_BAL<-data$LIMIT_BAL/1000
chg_list<-colnames(data)[13:24]
for (var in chg_list){data[,var]<-data[,var]/1000}

#replace -a as zero







#first to do some EDA
#EDA----
#age
ggplot(data,aes(y=default_f, x=(AGE), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="Age",y="Premature") + 
  theme_classic() + theme(legend.position="none")

png(filename = "figure/age.png",type = "cairo", res = 300, width = 3000,
    height = 1500,bg = "transparent")
plot(tapply(data$AGE, data$AGE, mean),
     tapply(data$default, data$AGE, mean),
     ylab="Prob. Default", xlab="Age",col='navy', pch=10,cex=1)
dev.off()

#gender
table(data[,c("SEX","default_f")])
table(data[,c("SEX","default_f")])/sum(table(data[,c("SEX","default_f")]))


apply(table(data[,c("SEX","default_f")])/sum(table(data[,c("SEX","default_f")])),
      2,function(x) x/sum(x))
apply(table(data[,c("SEX","default_f")])/sum(table(data[,c("SEX","default_f")])),
      1,function(x) x/sum(x))

#the probability of premature for sex=1 is 0.2515
#the probability of premature for sex=2 is 0.0.2179
#4% higher with sex=1(male) 

# Education
var='EDUCATION'
table(data[,c(var,"default_f")])
table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")]))


apply(table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")])),
      2,function(x) x/sum(x))
apply(table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")])),
      1,function(x) x/sum(x))

#the probability of premature for edu=1(graduate) is 0.1955
#the probability of premature for edu=3(high school) is 0.2586
#higher with = lower edu level 


#MARRIAGE
var='MARRIAGE'
table(data[,c(var,"default_f")])
table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")]))


apply(table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")])),
      2,function(x) x/sum(x))
apply(table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")])),
      1,function(x) x/sum(x))

#the probability of premature for marriage=1(married) is 0.2347
#the probability of premature for marriage=2(single) is 0.2093
#higher with = single people

#PAY_0 sept
ggplot(data,aes(y=default_f, x=(PAY_0), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_0",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

var='PAY_6'
table(data[,c(var,"default_f")])
table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")]))


apply(table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")])),
      2,function(x) x/sum(x))
apply(table(data[,c(var,"default_f")])/sum(table(data[,c(var,"default_f")])),
      1,function(x) x/sum(x))


#PAY_2 aug
ggplot(data,aes(y=default_f, x=(PAY_2), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_2",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#PAY_3 jul
ggplot(data,aes(y=default_f, x=(PAY_3), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_3",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")


#PAY_4 jun
ggplot(data,aes(y=default_f, x=(PAY_4), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_4",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#PAY_4 may
ggplot(data,aes(y=default_f, x=(PAY_5), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_5",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#PAY_4 apri
ggplot(data,aes(y=default_f, x=(PAY_6), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_6",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")




#bill amount
#BILL_AMT1 sept
ggplot(data,aes(y=default_f, x=BILL_AMT1, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="BILL_AMT1",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#BILL_AMT2 augest
ggplot(data,aes(y=default_f, x=BILL_AMT2, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="BILL_AMT2",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#BILL_AMT3 augest
ggplot(data,aes(y=default_f, x=BILL_AMT3, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="BILL_AMT3",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#BILL_AMT4 augest
ggplot(data,aes(y=default_f, x=BILL_AMT4, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="BILL_AMT4",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#BILL_AMT5 augest
ggplot(data,aes(y=default_f, x=BILL_AMT5, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="BILL_AMT5",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#BILL_AMT6 augest
ggplot(data,aes(y=default_f, x=BILL_AMT6, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="BILL_AMT6",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")
#almost the same




ggplot(data,aes(y=default_f, x=LIMIT_BAL, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_AMT1",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")





#previous pay 
#PAY_AMT1 sept
ggplot(data,aes(y=default_f, x=PAY_AMT1, fill=default_f)) +
geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_AMT1",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#PAY_AMT2 sept
ggplot(data,aes(y=default_f, x=PAY_AMT2, fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="PAY_AMT2",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")

#some outliers

#### interaction ----
ggplot(data,aes(y=default_f, x=(AGE), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="AGE",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")+
  facet_wrap( ~ MARRIAGE)
#not clear evidence

ggplot(data,aes(y=default_f, x=(AGE), fill=default_f)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(x="AGE",y="Prob. Default") + 
  theme_classic() + theme(legend.position="none")+
  facet_wrap( ~ SEX)
# I guess do not need to include interation term 



#### build model----
reg0<- glm(default ~ ., data = data[,c(2:25)], family = binomial(link=logit))
summary(reg0)


vif(reg0)




reg0<- glm(default ~ ., data = data[,c(2:13,19:25)], family = binomial(link=logit))
vif(reg0)
out<-summary(reg0)
out
out$coefficients[ , 3] 


#
Fullmodel=glm(default ~ ., data = data[,c(2:25)], family = binomial(link=logit))
Nullmodel=glm(default ~ 1, data = data[,c(2:25)], family = binomial(link=logit))

Model_stepwise <- step(Nullmodel, scope = formula(Fullmodel),direction="both",trace=0)
Model_stepwise$call

reg_aic=glm(formula = default ~ PAY_0 + LIMIT_BAL + PAY_3 + PAY_5 + MARRIAGE + 
      PAY_AMT1 + SEX + EDUCATION + PAY_AMT2 + PAY_6 + PAY_2 + BILL_AMT1 + 
     PAY_4, family = binomial(link = logit), data = data[,c(2:25)])                                                                    

anova(reg0,reg_aic,test = 'Chisq')



reg_bic=glm(formula = default ~ PAY_0 + LIMIT_BAL + PAY_3 + PAY_5 + MARRIAGE + 
              PAY_AMT1 + SEX + PAY_AMT2 + PAY_6, family = binomial(link = logit), 
            data = data[, c(2:25)])

anova(reg0,reg_bic,test = 'Chisq')

anova(reg_aic,reg_bic,test = 'Chisq')




n=nrow(data)
Model_stepwise <- step(Nullmodel, scope = formula(Fullmodel),direction="both",trace=0, k = log(n))
Model_stepwise$call


reg0<-reg_aic





#remove the Bill_AMT2~6 due to multicollinearity

png(filename = "figure/agr_resid.png",type = "cairo", res = 300, width = 3000,
    height = 1500,bg = "transparent")
binnedplot(data$AGE,residuals(reg0,"resp"),xlab="Age",
           col.int="red4",ylab="Avg. Residuals",col.pts="navy")
dev.off()


plot(tapply(residuals(reg0,"resp"), data$AGE, mean),col='blue4',pch=10)

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(reg0) >= mean(data$default), "1","0")),
                            as.factor(data$default),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
#accuracy
#0.6225547 
Conf_mat$byClass[c("Sensitivity","Specificity")]
#Sensitivity Specificity 
#0.5975610   0.6283688


#cross validationn=100
A<-matrix(0,nrow=n,ncol=1)
for (i in 1:n){
Data <- data[sample(nrow(data)),]
K <-10
AR <- matrix(0,nrow=K,ncol=1)
kth_fold <- cut(seq(1,nrow(Data)),breaks=K,labels=FALSE)
# Now write the for loop for the k-fold cross validation
for(k in 1:K){
  # Split your data into the training and test datasets
  test_index <- which(kth_fold==k)
  train <- Data[-test_index,]
  test <- Data[test_index,]
  N=length(test$default)
  reg<- glm(default ~ ., data = train[,c(2:13,19:25)], family = binomial(link=logit))
  AR[k, ] <- 1-sum(abs((test$default)-(predict(reg,test,type = "response")>=mean(train$default))))/N 
}
A[i,]<-mean(AR)}


write.csv(data,file='data/new_data.csv',row.names = FALSE)



png(filename = "figure/init_dist.png",type = "cairo", res = 300, width = 3000,
    height = 1000,bg = "transparent")
ggplot(data, aes(x = LIMIT_BAL, fill = default_f)) + geom_density(alpha = 0.2)+
  theme_classic()+labs(x="Initial credit ($thousand)",y="Density")+xlim(5, 210)
dev.off()

