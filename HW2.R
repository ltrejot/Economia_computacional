#--------------------------------------------------
#load libraries
install.packages("randomForest")
install.packages("gbm")
library(randomForest)
library(gbm) #boosting
library(tree)
library(tidyverse)
library(rpart)
set.seed(99)
#--------------------------------------------------
#read in Used Cars quitando NA
uc = read.csv("usedcars.csv")
uc<-select (uc,-c(county))
colSums(is.na(uc))
uc <- uc[!(uc$year %in% c(NA)),]
uc <- uc[!(uc$odometer %in% c(NA)),]
uc <- uc[!(uc$lat %in% c(NA)),]
uc <- uc[!(uc$long %in% c(NA)),]
colSums(is.na(uc))
uc<-uc[!(uc$price>300000),]

#--------------------------------------------------

######## Trees odometer

temp = tree(price~odometer,data=uc,mindev=.000001)
cat("first big tree size: \n")
print(length(unique(temp$where)))
#if the tree is too small, make mindev smaller!!
#--------------------------------------------------
#then prune it down to one with 5 leaves
uc.tree=prune.tree(temp,best=34)
cat("pruned tree size: \n")
print(length(unique(uc.tree$where)))
#--------------------------------------------------
#plot the tree
plot(uc.tree,type="uniform")
text(uc.tree,col="blue",label=c("yval"),cex=.8)
#--------------------------------------------------
#plot data with fit
#get fit
uc.fit = predict(uc.tree) #get training fitted values
#plot fit
plot(uc$odometer,uc$price,cex=.5,pch=16,  ylim=c(1,100000)) #plot data
oo=order(uc$odometer)
lines(uc$odometer[oo],uc.fit[oo],col="red",lwd=3) #step function fit



######## Trees year

temp = tree(price~year,data=uc,mindev=.001)
cat("first big tree size: \n")
print(length(unique(temp$where)))
#if the tree is too small, make mindev smaller!!
#--------------------------------------------------
#then prune it down to one with 8 leaves
uc.tree=prune.tree(temp,best=8)
cat("pruned tree size: \n")
print(length(unique(uc.tree$where)))
#--------------------------------------------------
#plot the tree
plot(uc.tree,type="uniform")
text(uc.tree,col="blue",label=c("yval"),cex=.8)
#--------------------------------------------------
#plot data with fit
#get fit
uc.fit = predict(uc.tree) #get training fitted values
#plot fit
plot(uc$year,uc$price,cex=.5,pch=16) #plot data
oo=order(uc$year)
lines(uc$year[oo],uc.fit[oo],col="red",lwd=3) #step function fit



######## Trees all

temp = tree(price~.,data=uc,mindev=.0001)
cat("first big tree size: \n")
print(length(unique(temp$where)))
#if the tree is too small, make mindev smaller!!
#--------------------------------------------------
#then prune it down to one with 8 leaves
uc.tree=prune.tree(temp,best=8)
cat("pruned tree size: \n")
print(length(unique(uc.tree$where)))
#--------------------------------------------------
#plot the tree
plot(uc.tree,type="uniform")
text(uc.tree,col="blue",label=c("yval"),cex=.8)
#--------------------------------------------------
#plot data with fit
#get fit
uc.fit = predict(uc.tree) #get training fitted values
#plot fit
plot(uc$odometer,uc$price,cex=.5,pch=16,  ylim=c(1,100000)) #plot data
oo=order(uc$odometer)
lines(uc$odometer[oo],uc.fit[oo],col="red",lwd=3) #step function fit



######### Best tree odometer

big.tree = rpart(price~odometer,method="anova",data=uc,
                 control=rpart.control(minsplit=5,cp=.0005))
nbig = length(unique(big.tree$where))
cat("size of big tree: ",nbig,"\n")
#--------------------------------------------------
#look at CV results
png(file="C:/Users/Luis.Trejo/Documents/Luis/saving_plot2.png",
    width=1000, height=700)
plotcp(big.tree)
iibest = which.min(big.tree$cptable[,"xerror"]) #which has the lowest error
bestcp=big.tree$cptable[iibest,"CP"]
bestsize = big.tree$cptable[iibest,"nsplit"]+1
dev.off()
#--------------------------------------------------
#prune to good tree
best.tree = prune(big.tree,cp=bestcp)
#plot tree
png(file="C:/Users/Luis.Trejo/Documents/Luis/saving_plot2.png",
width=1000, height=700)
plot(best.tree,uniform=TRUE,branch=.5,margin=.5)
text(best.tree,digits=4,use.n=TRUE,fancy=TRUE,bg="lightblue")
plot(best.tree,uniform=TRUE)
text(best.tree,digits=4,use.n=TRUE)
dev.off()

#--------------------------------------------------
#get fits
yhat = predict(best.tree)
png(file="C:/Users/Luis.Trejo/Documents/Luis/fit_odometer.png",
    width=1000, height=700)
plot(uc$price,yhat)
abline(0,1,col="red",lwd=3)
dev.off()



######### Best tree year

big.tree = rpart(price~year,method="anova",data=uc,
                 control=rpart.control(minsplit=5,cp=.0005))
nbig = length(unique(big.tree$where))
cat("size of big tree: ",nbig,"\n")
#--------------------------------------------------
#look at CV results
png(file="C:/Users/Luis.Trejo/Documents/Luis/CV_TREE_YEAR.png",
    width=1000, height=700)
plotcp(big.tree)
iibest = which.min(big.tree$cptable[,"xerror"]) #which has the lowest error
bestcp=big.tree$cptable[iibest,"CP"]
bestsize = big.tree$cptable[iibest,"nsplit"]+1
dev.off()
#--------------------------------------------------
#prune to good tree
best.tree = prune(big.tree,cp=bestcp)
#plot tree
png(file="C:/Users/Luis.Trejo/Documents/Luis/tree_year.png",
    width=1000, height=700)
plot(best.tree,uniform=TRUE,branch=.5,margin=.5)
text(best.tree,digits=4,use.n=TRUE,fancy=TRUE,bg="lightblue")
plot(best.tree,uniform=TRUE)
text(best.tree,digits=4,use.n=TRUE)
dev.off()
#--------------------------------------------------
#get fits
yhat = predict(best.tree)
png(file="C:/Users/Luis.Trejo/Documents/Luis/tree_fit_year.png",
    width=1000, height=700)
plot(uc$price,yhat)
abline(0,1,col="red",lwd=3)
dev.off()

############################################## Price vs odometer
#train, val , test
uc3=uc[,c(5,12)] #pick off dis,lstat,medv
print(names(uc3))
str(uc3)

set.seed(71) 
n=nrow(uc3)
n1=floor(n/2)
n2=floor(n/4)
n3=n-n1-n2
ii = sample(1:n,n)
catrain=uc[ii[1:n1],]
caval = uc[ii[n1+1:n2],]
catest = uc[ii[n1+n2+1:n3],]
#fit using random forests (on train, predict on val)
#mtry is the number of variables to try
rffit = randomForest(price~odometer,data=catrain,mtry=1,ntree=3)
rfvalpred = predict(rffit,newdata=caval)
#--------------------------------------------------
#fit using boosting
boostfit = gbm(price~odometer,data=catrain,distribution="gaussian",
               interaction.depth=4,n.trees=5,shrinkage=.2)
boostvalpred=predict(boostfit,newdata=caval,n.trees=5)
#--------------------------------------------------
#plot (out-of-sample) fits
pairs(cbind(caval$price,rfvalpred,boostvalpred))
print(cor(cbind(caval$price,rfvalpred,boostvalpred)))


#--------------------------------------------------
catrainval = rbind(catrain,caval) #stacks the two data frames
#--------------------------------------------------
#refit boosting
boostfit2 = gbm(price~odometer,data=catrainval,distribution="gaussian",
                interaction.depth=4,n.trees=5,shrinkage=.2)
boosttestpred=predict(boostfit2,newdata=catest,n.trees=5)
#--------------------------------------------------
#plot test y vs test predictions
plot(catest$price,boosttestpred)
abline(0,1,col="red",lwd=2)
#--------------------------------------------------
rmse = sqrt(mean((catest$price-boosttestpred)^2))
cat("rmse on test for boosting: ",rmse,"\n")
#--------------------------------------------------
#variable importance from boosting
summary(boostfit2)


#--------------------------------------------------
#refit random forests on train-val
rffit2 = randomForest(price~odometer,data=catrainval,mtry=3,ntree=5)
rftestpred = predict(rffit2,newdata=catest)
#--------------------------------------------------
rmse = sqrt(mean((catest$price-rftestpred)^2))
cat("rmse on test for random forests: ",rmse,"\n")
#--------------------------------------------------
#variable importance from Random Forests
varImpPlot(rffit2)



################################# Price vs year

rffit = randomForest(price~year,data=catrain,mtry=1,ntree=3)
rfvalpred = predict(rffit,newdata=caval)
#--------------------------------------------------
#fit using boosting
boostfit = gbm(price~year,data=catrain,distribution="gaussian",
               interaction.depth=4,n.trees=5,shrinkage=.2)
boostvalpred=predict(boostfit,newdata=caval,n.trees=5)
#--------------------------------------------------
#plot (out-of-sample) fits
pairs(cbind(caval$price,rfvalpred,boostvalpred))
print(cor(cbind(caval$price,rfvalpred,boostvalpred)))


#--------------------------------------------------
catrainval = rbind(catrain,caval) #stacks the two data frames
#--------------------------------------------------
#refit boosting
boostfit2 = gbm(price~year,data=catrainval,distribution="gaussian",
                interaction.depth=4,n.trees=5,shrinkage=.2)
boosttestpred=predict(boostfit2,newdata=catest,n.trees=5)
#--------------------------------------------------
#plot test y vs test predictions
plot(catest$price,boosttestpred)
abline(0,1,col="red",lwd=2)
#--------------------------------------------------
rmse = sqrt(mean((catest$price-boosttestpred)^2))
cat("rmse on test for boosting: ",rmse,"\n")
#--------------------------------------------------
#variable importance from boosting
summary(boostfit2)

#refit random forests on train-val
rffit2 = randomForest(price~year,data=catrainval,mtry=3,ntree=5)
rftestpred = predict(rffit2,newdata=catest)
#--------------------------------------------------
rmse = sqrt(mean((catest$price-rftestpred)^2))
cat("rmse on test for random forests: ",rmse,"\n")