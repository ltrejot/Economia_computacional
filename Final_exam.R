install.packages("kknn")
install.packages("caTools")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(kknn) 
library(ggplot2) 
library(caTools) 
library(gridExtra)
library(ggthemes)
library(randomForest)
library(gbm) 
library(glmnet)
#--------------------------------------------------


########### Datos

autos<- read.csv(file="C:/Users/Luis.Trejo/Documents/Luis/usedcarsf.csv", header=TRUE, sep=",")
head(autos,5)
summary(autos)
str(autos)

################ Problem 1 #############################
#######################################################

p1<- ggplot(autos, aes(mileage, price)) + 
  ggtitle("Kilometros vs Precio")+ 
  xlab("Mileage")+ 
  ylab("Precio")+
  geom_point()+theme_economist_white()
ggsave("Price_Kilometer.png", plot = p1)
p1

p2<- ggplot(autos, aes(mileage, price)) + 
  ggtitle("Kilometros vs Precio")+ 
  xlab("Mileage")+ 
  ylab("Precio")+geom_point()+ylim(0,100000)+
  geom_smooth(method="lm")+theme_economist_white()
ggsave("Price_Kilometer_lm.png", plot = p2)
p2

#p3 <- ggplot(autos, aes(kilometer, price, colour= gearbox)) + 
#  geom_smooth(mapping = aes(linetype = gearbox), method=lm, se= FALSE)+
#  theme_economist_white()+
#  xlab("Kilometros")+ 
#  ylab("Precio")+ 
#  ggtitle("Precio vs Km por tipo de transimisión")
#ggsave("Price_gearbox.png", plot = p3)


#p4 <- ggplot(autos, aes(kilometer, price, colour= brand)) + 
#  geom_smooth(method =lm, se= FALSE)+
#  theme_fivethirtyeight()
#p4

########################

set.seed(101)
muestra <- sample.split(autos$price, SplitRatio = 0.6)
train <- subset(autos,sample=TRUE)
test <- subset(autos,sample=FALSE)


kn1000 = kknn(price~mileage,train,test,k=1000)
kn800 = kknn(price~mileage,train,test,k=800)
kn100 = kknn(price~mileage,train,test,k=100)
bkn = kknn(price~mileage,train,test,kernel = "rectangular")

#KNN todos
p5 <- ggplot() +
  geom_point(aes(x=autos$mileage, y = autos$price ))+
  geom_line(aes(x= test$mileage,y = kn1000$fitted.values,col="Red"))+
  geom_line(aes(x= test$mileage,y = kn800$fitted.values,col="Blue")) +
  geom_line(aes(x= test$mileage,y = kn100$fitted.values,col="Green")) +
  ggtitle("Distintos KNN")+
  xlab("Mileage")+
  ylab("Precio")
ggsave("Price_dknns.png", plot = p5)
p5

#KNN 1000
p6 <- ggplot() +
  geom_point(aes(x=autos$mileage, y = autos$price ))+
  geom_line(aes(x= test$mileage,y = kn1000$fitted.values,col="Red"))+
  ggtitle("KNN 1000")+
  xlab("Mileage")+
  ylab("Precio")
ggsave("Price_kn1000.png", plot = p6)
p6

#KNN 800
p7 <- ggplot() +
  geom_point(aes(x=autos$mileage, y = autos$price ))+
  geom_line(aes(x= test$mileage,y = kn800$fitted.values,col="Blue"))+
  ggtitle("KNN 800")+
  xlab("Mileage")+
  ylab("Precio")
ggsave("Price_kn800.png", plot = p7)
p7

#KNN 100
p8 <- ggplot() +
  geom_point(aes(x=autos$mileage, y = autos$price ))+
  geom_line(aes(x= test$mileage,y = kn100$fitted.values,colour="green"))+
  ggtitle("KNN 100")+
  xlab("Mileage")+
  ylab("Precio")
ggsave("Price_kn100.png", plot = p8)
p8

vkm = data.frame(mileage=100000)
kbest= kknn(price~mileage,train,vkm,k=1000,kernel = "rectangular")
cat("El precio aproximado de un auto con 100,000 kms es de ",kbest$fitted,"\n")


mlm = lm(price~mileage,autos)
preciolm = predict(mlm,vkm)
cat("El precio aproximado de un auto con 100,000 kms es de ",preciolm,"\n")



########################### Problem 2 #################
###############################################################

set.seed(101)
val <- subset(autos, SplitRatio = 0.2)
train <- subset(autos,random=.5)
test <- subset(autos,random=.3)

########### Train
rffit = randomForest(price~mileage,data=train,ntree=5)
rfvalpred = predict(rffit,newdata=val)

boostfit = gbm(price~mileage,data=train,distribution="gaussian",
               interaction.depth=4,n.trees=5,shrinkage=.2)
boostvalpred=predict(boostfit,newdata=val,n.trees=5)

p9 <- ggplot() +
  geom_point(aes(x=val$price, y = boostvalpred ))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  ggtitle("Boosting Validation data")+
  xlab("Precio")+
  ylab("Validation")
ggsave("val_data.png", plot = p9)
p9

p10 <- ggplot() +
  geom_point(aes(x=val$price, y = rfvalpred ))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  ggtitle("Random Forest Validation data")+
  xlab("Precio")+
  ylab("Validation")
ggsave("val_data_RF.png", plot = p10)
p10

pairs(cbind(val$price,rfvalpred,boostvalpred))
print(cor(cbind(val$price,rfvalpred,boostvalpred)))

###################### Test
rffit_test = randomForest(price~mileage,data=test,ntree=5)
rfvalpred_test = predict(rffit_test,newdata=val)

boostfit_test = gbm(price~mileage,data=test,distribution="gaussian",
               interaction.depth=4,n.trees=5,shrinkage=.2)
boostvalpred_test=predict(boostfit_test,newdata=val,n.trees=5)

p11 <- ggplot() +
  geom_point(aes(x=val$price, y = rfvalpred_test ))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  ggtitle("Random Forest Validation data using Test data")+
  xlab("Precio")+
  ylab("Validation")
ggsave("val_data_RF_test.png", plot = p11)
p11

p12 <- ggplot() +
  geom_point(aes(x=val$price, y = boostvalpred_test ))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  ggtitle("Boosting Validation data using Test data")+
  xlab("Precio")+
  ylab("Validation")
ggsave("val_data_boosting_test.png", plot = p12)
p12


pairs(cbind(val$price,rfvalpred_test,boostvalpred_test))
print(cor(cbind(val$price,rfvalpred_test,boostvalpred_test)))

########## Using the 2 data frames
trainval = rbind(train,val)

boostfit2 = gbm(price~.,data=trainval,distribution="gaussian",
                interaction.depth=4,n.trees=5,shrinkage=.2)
boosttestpred=predict(boostfit2,newdata=test,n.trees=5)

p14 <- ggplot() +
  geom_point(aes(x=test$price, y = boosttestpred ))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  ggtitle("Boosting predict on Test data")+
  xlab("Precio")+
  ylab("Validation")
ggsave("val_data_boosting_test.png", plot = p14)
p14

rmse = sqrt(mean((test$price-boosttestpred)^2))
cat("rmse on test for boosting: ",rmse,"\n")


rffit2 = randomForest(price~.,data=trainval,mtry=3,ntree=5)
rftestpred = predict(rffit2,newdata=test)

rmse = sqrt(mean((test$price-rftestpred)^2))
cat("rmse on test for random forests: ",rmse,"\n")

p15 <- ggplot() +
  geom_point(aes(x=test$price, y = rftestpred ))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  ggtitle("Random Forest predict on Test data")+
  xlab("Precio")+
  ylab("Validation")
ggsave("val_data_Random_forest_test.png", plot = p15)
p15

########################## Problem 3 ##################
######################################################

autos$displacement=as.factor(autos$displacement)
summary(autos$displacement)

x=model.matrix(price~.,autos)
dim(x)
colnames(x)
head(x)

lm1=lm(price~.,autos)
lm2=lm(price~.,data.frame(price=autos$price,x))
summary(lm1$fitted-lm2$fitted)

########### Lasso usando gaussian porque precio no es factor
lasso_cv = cv.glmnet(x=x, y=autos$price, family="gaussian", nfolds=5)
fit_lasso = lasso_cv$glmnet.fit

plot(lasso_cv,main="Cross Validation",ylab="Mean Square Error") 
plot(fit_lasso,main="Coeficientes",ylab="Mean Square Error")

#Vector de precios usando lasso y la lamba mínima
prob = predict(lasso_cv$glmnet.fit, x, type="response", s=lasso_cv$lambda.min)
head(prob)

autos<-mutate(autos,lnprice=log(price))
########### Lasso usando lnprice
lasso_cv_ln = cv.glmnet(x=x, y=autos$lnprice, family="gaussian", nfolds=5)
fit_lasso_ln = lasso_cv_ln$glmnet.fit

plot(lasso_cv_ln,main="Cross Validation ln price",ylab="Mean Square Error") 
plot(fit_lasso_ln,main="Coeficientes ln price",ylab="Mean Square Error")

#Vector de precios usando lasso y la lamba mínima
prob_ln = predict(lasso_cv_ln$glmnet.fit, x, type="response", s=lasso_cv_ln$lambda.min)
head(prob_ln)



