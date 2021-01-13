#load libraries
install.packages("randomForest")
install.packages("gbm")
install.packages("ggthemes")
install.packages("hexbin")
install.packages("plotly")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("lattice")
install.packages("caret")

library(gridExtra)
library(ggpubr)
library(randomForest)
library(gbm) #boosting
library(tree)
library(tidyverse)
library(rpart)
library(ggplot2)
library(ggthemes)
library(hexbin)
library(plotly)
library(ggplot)
library(caret)

#--------------------------------------------------
### read in soccerbase quitando NA
sb = read.csv("soccerf.csv")

### Creación variable diferencia de goles
sb<-mutate(sb,goal_dif_total=abs(full_home_goals-full_away_goals))
sb<-mutate(sb,goal_dif_half=abs(half_time_score_home-half_time_score_away))
sb<-mutate(sb,goal_dif_half_nom=half_time_score_home-half_time_score_away)
sb<-mutate(sb,ratio_passes=home_accurate_pass/home_total_pass)
### Análisis gráfico

plot(sb$full_time_result,sb$home_possession_percentage, type="p",col="blue",ylab="Home posession percentage",xlab="Full time Result")

#posesión vs resultado
p1<-ggplot(data = sb) +
  geom_point(mapping = aes(x = away_possession_percentage, y = home_possession_percentage,
                           color=(full_time_result)))+xlab("Posesión visitante %")+ 
  ylab("Posesión local %")+ 
  ggtitle("Posesión del balón")+
    theme(legend.position = "none")+theme_fivethirtyeight()
    ggsave("Posesión del balón.png", plot = p1)

#Goles al medio tiempo vs full time result
p2<-ggplot(data = sb, mapping = aes(x =goal_dif_half_nom , y = full_time_result,colour = full_time_result)) +
  geom_count(alpha=0.7)+
 xlab("Diferencia de goles al medio tiempo")+ 
  ylab("Resultado final")+ 
  ggtitle("Resultado final vs goles al medio tiempo")
  theme(legend.position = "none") +
  facet_wrap(~full_time_result)
ggplotly(p2)
ggsave("Resultado_final_goles_medio_tiempo.png", plot = p2)

# Total score vs diferencia de goles al medio tiempo
p3<-ggplot(data = sb, mapping = aes(x =goal_dif_half_nom , y = full_time_score,colour = full_time_result)) +
  geom_count(alpha=0.7)+
  theme(legend.position = "none") +
  facet_wrap(~full_time_score) +
  xlab("Diferencia de goles al medio tiempo")+ 
  ylab("Marcador final")+ 
  ggtitle("Marcador final vs goles al medio tiempo")
ggplotly(p3)
ggsave("Marcador_final_goles_medio_tiempo.png", plot = p3)

# Resultados vs diferencia de goles al medio tiempo

p4<-ggplot(data = sb, mapping = aes(x =goal_dif_half_nom , y = full_time_result,colour = full_time_result)) +
  geom_count(alpha=0.7)+
  theme(legend.position = "none") +
  xlab("Diferencia de goles al medio tiempo")+ 
  ylab("Marcador final")+ 
  ggtitle("Marcador final vs goles al medio tiempo")+
  facet_wrap(~full_time_score)
ggplotly(p4)
ggsave("Marcador_final_goles_medio_tiempo_bueno.png", plot = p4)

summary(sb$goal_dif_half_nom==2)

###
p5<-ggplot(data = sb, mapping = aes(x =season_year , y = total_score_goals,colour = total_score_goals)) +
  geom_count(alpha=0.7)+
  theme(legend.position = "none") +
  xlab("Temporada")+ 
  ylab("Goles")+ 
  ggtitle("Goles anotados por temporada")+
  facet_wrap(~total_score_goals)
ggplotly(p5)
ggsave("Goles anotados por temporada.png", plot = p5)


######
p6<-ggplot(data = sb, mapping = aes(x =season_year , y = full_time_score,colour = full_time_score)) +
  geom_count(alpha=0.7)+
  theme(legend.position = "none") +
  xlab("Temporada")+ 
  ylab("Marcador final")+ 
  ggtitle("Marcador final por temporada")+
  facet_wrap(~full_time_score)
ggplotly(p6)
ggsave("Marcador final por temporada.png", plot = p6)


### Distribución goles por temporada
sb %>% count(season_year, total_score_goals) %>%
  ggplot(mapping = aes(x = season_year, y = total_score_goals)) +
  geom_tile(mapping = aes(fill = n))+
  xlab("Temporada")+ 
  ylab("Goles totales")+ 
  ggtitle("Goles totales por partido por temporada")+
  ggsave("Goles totales por partido y temporada.png")


### distribución de Resultados finales
p7<-ggplot(sb) +
  geom_bar(mapping=aes(x =full_time_score))+
  geom_bar(mapping=aes(x =half_time_score))+theme_economist_white()


### distribución de Resultados medio tiempo
p8<-ggplot(sb) +
  geom_bar(mapping=aes(x =half_time_score))+theme_fivethirtyeight()


figure <- grid.arrange(p8, p7, ncol=2)



### Corners vs goals

sb %>% count(home_won_corners, full_home_goals) %>%
  ggplot(mapping = aes(x = home_won_corners, y = full_home_goals)) +
  geom_tile(mapping = aes(fill = n))+
  xlab("Tiros de esquina")+ 
  ylab("Goles totales del local")+ 
  ggtitle("Goles totales del local vs tiros de esquina")+
  ggsave("Goles totales del local vs corner.png")

### home_ontarget_scoring_att vs goals

sb %>% count(home_ontarget_scoring_att, full_time_result) %>%
  ggplot(mapping = aes(x = home_ontarget_scoring_att, y = full_time_result)) +
  geom_tile(mapping = aes(fill = n))+
  xlab("Disparos al arco")+ 
  ylab("Resultado final")+ 
  ggtitle("Disparos al arco vs resultado final")+
  ggsave("Disparos al arco vs resultado final.png")




### Accurate pass vs Resultado final
ggplot(data = sb, aes(x = home_accurate_pass)) +
  geom_freqpoly(mapping = aes(colour = full_time_result))+
  xlab("Pases completos local")+ 
  ggtitle("Pases completos local y resultado final")+
  ggsave("Pases completos y resultado final.png")

####
ggplot(data = sb) +
  geom_boxplot(mapping = aes(x = full_time_result, y = home_possession_percentage,
                           color=(full_time_result)))+xlab("Resultado partido")+ 
  ylab("Ratio pases completos %")+ 
  ggtitle("Posesión del local vs resultado final")+
  theme(legend.position = "none")+theme_fivethirtyeight()
ggsave("Posesión del local vs resultado final_box.png")


### Home possession y resultado final
ggplot(data = sb, aes(x = home_possession_percentage)) +
  geom_freqpoly(mapping = aes(colour = full_time_result))+
xlab("Posesión equipo local")+ 
  ggtitle("Posesión equipo local y resultado final")+
 theme_fivethirtyeight()
  ggsave("Posesión equipo local y resultado final.png")



### Away possession y resultado final
ggplot(data = sb, aes(x = away_possession_percentage)) +
  geom_density(mapping = aes(colour = full_time_result))+theme_fivethirtyeight()



#################### TREE home_possession_percentage


temp = tree(full_time_result~home_possession_percentage,data=sb,mindev=.001)
cat("first big tree size: \n")
print(length(unique(temp$where)))
#if the tree is too small, make mindev smaller!!
#--------------------------------------------------
#then prune it down to one with 8 leaves
uc.tree=prune.tree(temp,best=9)
cat("pruned tree size: \n")
print(length(unique(uc.tree$where)))
#--------------------------------------------------
#plot the tree
plot(uc.tree,type="uniform")
text(uc.tree,col="blue",label=c("yval"),cex=.8)
#--------------------------------------------------
#get fit
uc.fit = predict(uc.tree,type = "class",newdata = sb) #get training fitted values
oo=order(sb$full_time_result)

#Confussion Matrix
table_mat <- table(sb$full_time_result, uc.fit[oo])
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#################### Best TREE home_possession_percentage

big.tree_pp = rpart(full_time_result~home_possession_percentage,method="anova",data=sb,
                    control=rpart.control(minsplit=5,cp=.0005))
nbig_pp = length(unique(big.tree_pp$where))
cat("size of big tree: ",nbig,"\n")
#--------------------------------------------------
#look at CV results
plotcp(big.tree_pp)
iibest_pp = which.min(big.tree_pp$cptable[,"xerror"]) #which has the lowest error
bestcp_pp=big.tree_pp$cptable[iibest_pp,"CP"]
bestsize_pp = big.tree_pp$cptable[iibest_pp,"nsplit"]+1

#--------------------------------------------------
#prune to good tree
best.tree_pp = prune(big.tree_pp,cp=bestcp_pp)
#plot tree
plot(best.tree_pp,uniform=TRUE)
text(best.tree_pp,digits=4,use.n=TRUE,col="blue",cex=.8)


############################################################33
# grow tree
fit_pp <- rpart(full_time_result~home_possession_percentage,
                method="anova", data=sb)

printcp(fit_pp) # display the results
plotcp(fit_pp) # visualize cross-validation results
summary(fit_pp) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit_pp) # visualize cross-validation results  

# plot tree
plot(fit_pp, uniform=TRUE)
text(fit_pp, use.n=TRUE, all=TRUE, cex=.8)


#############################################
#############################################

#################### TREE home_accurate_pass

temp_ap = tree(full_time_result~home_accurate_pass,data=sb,mindev=.001)
cat("first big tree size: \n")
print(length(unique(temp$where)))
#if the tree is too small, make mindev smaller!!
#--------------------------------------------------
#then prune it down to one with 8 leaves
uc.tree_ap=prune.tree(temp_ap,best=4)
cat("pruned tree size: \n")
print(length(unique(uc.tree_ap$where)))
#--------------------------------------------------
#plot the tree
plot(uc.tree_ap,type="uniform")
text(uc.tree_ap,col="blue",label=c("yval"),cex=.8)
#--------------------------------------------------
#plot data with fit
#get fit
uc.fit_ap = predict(uc.tree_ap,type = "class",newdata = sb) #get training fitted values
oo_ap=order(sb$home_accurate_pass)

#Confussion Matrix
table_mat_ap <- table(sb$full_time_result, uc.fit_ap[oo_ap])
accuracy_Test_ap <- sum(diag(table_mat_ap)) / sum(table_mat_ap)
accuracy_Test_ap

#################### BEST TREE home_accurate_pass

big.tree_ap = rpart(full_time_result~home_accurate_pass,method="anova",data=sb,
                    control=rpart.control(minsplit=5,cp=.0005))
nbig_ap = length(unique(big.tree_ap$where))
cat("size of big tree: ",nbig_ap,"\n")
#--------------------------------------------------
#look at CV results
plotcp(big.tree_ap)
iibest_ap = which.min(big.tree_ap$cptable[,"xerror"]) #which has the lowest error
bestcp_ap=big.tree_ap$cptable[iibest_ap,"CP"]
bestsize_ap = big.tree_ap$cptable[iibest_ap,"nsplit"]+1

#--------------------------------------------------
#prune to good tree
best.tree_ap = prune(big.tree_ap,cp=bestcp_ap)
#plot tree
plot(best.tree_ap,uniform=TRUE)
text(best.tree_ap,digits=4,use.n=TRUE,col="blue",cex=.8)


############################################################33
# grow tree
fit_ap <- rpart(full_time_result~home_accurate_pass,
                method="anova", data=sb)

printcp(fit_ap) # display the results
plotcp(fit_ap) # visualize cross-validation results
summary(fit_ap) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit_ap) # visualize cross-validation results  

# plot tree
plot(fit_ap, uniform=TRUE)
text(fit_ap, use.n=TRUE, all=TRUE, cex=.8)

################################
################################

#################### TREE goal_dif_half_nom
temp_gd = tree(full_time_result~goal_dif_half_nom,data=sb,mindev=.001)
cat("first big tree size: \n")
print(length(unique(temp$where)))
#if the tree is too small, make mindev smaller!!
#--------------------------------------------------
#then prune it down to one with 8 leaves
uc.tree_gd=prune.tree(temp_gd,best=bestsize_gd)
cat("pruned tree size: \n")
print(length(unique(uc.tree$where)))
#--------------------------------------------------
#plot the tree
plot(uc.tree_gd,type="uniform")
text(uc.tree_gd,col="blue",label=c("yval"),cex=.8)
#--------------------------------------------------
#plot data with fit
#get fit
uc.fit_gd = predict(uc.tree_gd,type = "class",newdata = sb) #get training fitted values
oo_gd=order(sb$goal_dif_half_nom)

#Confussion Matrix
table_gd <- table(sb$full_time_result, uc.fit_gd[oo])
accuracy_Test <- sum(diag(table_gd)) / sum(table_gd)
accuracy_Test




#################### BEST TREE goal_dif_half_nom

big.tree_gd = rpart(full_time_result~goal_dif_half_nom,method="anova",data=sb,
                    control=rpart.control(minsplit=5,cp=.0005))
nbig_gd = length(unique(big.tree_gd$where))
cat("size of big tree: ",nbig_gd,"\n")
#--------------------------------------------------
#look at CV results
plotcp(big.tree_gd) 
iibest_gd = which.min(big.tree_gd$cptable[,"xerror"]) #which has the lowest error
bestcp_gd=big.tree_gd$cptable[iibest_gd,"CP"]
bestsize_gd = big.tree_gd$cptable[iibest_gd,"nsplit"]+1

#--------------------------------------------------
#prune to good tree
best.tree_gd = prune(big.tree_gd,cp=bestcp_gd)
#plot tree
plot(best.tree_gd,uniform=TRUE)
text(best.tree_gd,digits=4,use.n=TRUE,col="blue",cex=.8)


############################################################33
# grow tree
fit_gd <- rpart(full_time_result~goal_dif_half_nom,
                method="anova", data=sb)

printcp(fit_gd) # display the results
plotcp(fit_gd) # visualize cross-validation results
summary(fit_gd) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit_gd) # visualize cross-validation results  

# plot tree
plot(fit_gd, uniform=TRUE)
text(fit_gd, use.n=TRUE, all=TRUE, cex=.8)

####################################
####################################

#################### TREE home_ontarget_scoring_att

temp_on = tree(full_time_result~home_ontarget_scoring_att,data=sb,mindev=.001)
cat("first big tree size: \n")
print(length(unique(temp$where)))
#if the tree is too small, make mindev smaller!!
#--------------------------------------------------
#then prune it down to one with 8 leaves
uc.tree_on=prune.tree(temp_on,best=bestsize_on)
cat("pruned tree size: \n")
print(length(unique(uc.tree$where)))
#--------------------------------------------------
#plot the tree
plot(uc.tree_on,type="uniform")
text(uc.tree_on,col="blue",label=c("yval"),cex=.8)
#--------------------------------------------------
#plot data with fit
#get fit
uc.fit_on = predict(uc.tree_on,type = "class",newdata = sb) #get training fitted values
oo_on=order(sb$home_ontarget_scoring_att)

#Confussion Matrix
table_on <- table(sb$full_time_result, uc.fit_on[oo])
accuracy_Test <- sum(diag(table_on)) / sum(table_on)
accuracy_Test


#################### BEST TREE home_ontarget_scoring_att

big.tree_on = rpart(full_time_result~home_ontarget_scoring_att,method="anova",data=sb,
                 control=rpart.control(minsplit=5,cp=.0005))
nbig = length(unique(big.tree_on$where))
cat("size of big tree: ",nbig,"\n")
#--------------------------------------------------
#look at CV results
plotcp(big.tree_on)
iibest_on = which.min(big.tree_on$cptable[,"xerror"]) #which has the lowest error
bestcp_on=big.tree_on$cptable[iibest_on,"CP"]
bestsize_on = big.tree_on$cptable[iibest_on,"nsplit"]+1

#--------------------------------------------------
#prune to good tree
best.tree_on = prune(big.tree_on,cp=bestcp_on)
#plot tree
plot(best.tree_on,uniform=TRUE)
text(best.tree_on,digits=4,use.n=TRUE,col="blue",cex=.8)


############################################################33
# grow tree
fit_on <- rpart(full_time_result~home_ontarget_scoring_att
             method="anova", data=sb)

printcp(fit_on) # display the results
plotcp(fit_on) # visualize cross-validation results
summary(fit_on) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit_on) # visualize cross-validation results  

# plot tree
plot(fit_on, uniform=TRUE)
text(fit_on, use.n=TRUE, all=TRUE, cex=.8)

####################################
####################################

#################################################################

#Full Time Result

#train, val , test
sb_2<-select (sb,-c(home_team_name,away_team_name,season,date_string,half_time_score,full_time_score ))

set.seed(14) 
n=nrow(sb_2)
n1=floor(n/2)
n2=floor(n/4)
n3=n-n1-n2
ii = sample(1:n,n)
sbtrain=sb_2[ii[1:n1],]
sbval = sb_2[ii[n1+1:n2],]
sbtest = sb_2[ii[n1+n2+1:n3],]

#--------------------------------------------------
#fit using random forests (on train, predict on val)
#mtry is the number of variables to try
rffit = randomForest(full_time_result~.,data=sb_2,mtry=3,ntree=5)
rfvalpred = predict(rffit,newdata=sbval)
#--------------------------------------------------
#fit using boosting
sbfit = gbm(full_time_result~.,data=sb_2,distribution="gaussian",
               interaction.depth=4,n.trees=5,shrinkage=.2)
sbvalpred=predict(sbfit,newdata=sbval,n.trees=5)
#--------------------------------------------------
#plot (out-of-sample) fits
pairs(cbind(sbval$full_time_result,rfvalpred,sbvalpred))
print(cor(cbind(sbval$full_time_result,rfvalpred,sbvalpred)))

#--------------------------------------------------
sbtrainval = rbind(sbtrain,sbval) #stacks the two data frames
#--------------------------------------------------
#refit boosting
sbfit2 = gbm(full_time_result~.,data=sbtrainval,distribution="gaussian",
                interaction.depth=4,n.trees=5,shrinkage=.2)
sbtestpred=predict(sbfit2,newdata=sbtest,n.trees=5)
#--------------------------------------------------
#plot test y vs test predictions

ggplot(data = sbtest) +
  geom_boxplot(mapping = aes(x = full_time_result, y = sbtestpred,
                           color=(full_time_result)))+
  theme(legend.position = "none")+theme_fivethirtyeight()

ggplot(data = sbval) +
  geom_boxplot(mapping = aes(x = full_time_result, y = sbvalpred,
                             color=(full_time_result)))+
  theme(legend.position = "none")+theme_fivethirtyeight()



#ggplot(sbtest$full_time_result,sbtestpred)
#abline(0,1,col="red",lwd=2)
#--------------------------------------------------
rmse = sqrt(mean((sbtest$full_time_result-sbtestpred)^2))
cat("rmse on test for boosting: ",rmse,"\n")
#--------------------------------------------------
#variable importance from boosting
summary(sbfit2)

par(mar = c(5, 8, 1, 1))
summary(
  sbfit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

#refit random forests on train-val
rffit2 = randomForest(full_time_result~.,data=sbtrainval,mtry=3,ntree=5)
rftestpred = predict(rffit2,newdata=sbtest)
#--------------------------------------------------
rmse = sqrt(mean((sbtest$full_time_result-rftestpred)^2))
cat("rmse on test for random forests: ",rmse,"\n")
#--------------------------------------------------
#variable importance from Random Forests
varImpPlot(rffit2)



############################################################33



sb_3<-select (sb,-c(home_team_name,away_team_name,season,date_string,half_time_score,full_time_score,full_home_goals,full_away_goals,goal_dif_total,X0,FIELD1,home_goals,total_score_goals,away_goals ))

set.seed(14) 
n=nrow(sb_2)
n1=floor(n/2)
n2=floor(n/4)
n3=n-n1-n2
ii = sample(1:n,n)
sbtrain=sb_3[ii[1:n1],]
sbval = sb_3[ii[n1+1:n2],]
sbtest = sb_3[ii[n1+n2+1:n3],]

#--------------------------------------------------
#fit using random forests (on train, predict on val)
#mtry is the number of variables to try
rffit = randomForest(full_time_result~.,data=sb_3,mtry=3,ntree=50)
rfvalpred = predict(rffit,newdata=sbval)
#--------------------------------------------------
#fit using boosting
sbfit = gbm(full_time_result~.,data=sb_3,distribution="gaussian",
            interaction.depth=4,n.trees=5,shrinkage=.2)
Boostvalpred=predict(sbfit,newdata=sbval,n.trees=5)



#--------------------------------------------------
#plot (out-of-sample) fits
pairs(cbind(sbval$full_time_result,rfvalpred,Boostvalpred))
print(cor(cbind(sbval$full_time_result,rfvalpred,Boostvalpred)))

#--------------------------------------------------
sbtrainval = rbind(sbtrain,sbval) #stacks the two data frames
#--------------------------------------------------


#refit boosting
sbfit2 = gbm(full_time_result~.,data=sbtrainval,distribution="gaussian",
             interaction.depth=4,n.trees=5,shrinkage=.2)
sbtestpred=predict(sbfit2,newdata=sbtest,n.trees=5)
#--------------------------------------------------
#plot test y vs test predictions Boosting

ggplot(data = sbtest) +
  geom_boxplot(mapping = aes(x = full_time_result, y = sbtestpred,
                             color=(full_time_result)))+
  theme(legend.position = "none")+theme_fivethirtyeight()+
  xlab("Resultado partido")+ 
  ylab("Resultado partido Pred")+ 
  ggtitle("Datos vs Predicción Boosting")+
  theme(legend.position = "none")+theme_fivethirtyeight()
ggsave("Datos vs Predicción.png")


#variable importance from boosting
summary(sbfit2)

par(mar = c(5, 8, 1, 1))
summary(
  sbfit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)



#refit random forests on train-val
rffit2 = randomForest(as.numeric(full_time_result)~.,data=sbtrainval,mtry=3,ntree=45,replace=T)
rftestpred = predict(rffit2,newdata=sbtest,n.trees=50)

#plot(rffit2)

#plot test y vs test predictions RF

ggplot(data = sbtest) +
  geom_boxplot(mapping = aes(x = full_time_result,
                             y = rftestpred,
                             color=(full_time_result)))+
  theme(legend.position = "none")+theme_fivethirtyeight()+
  xlab("Resultado partido")+ 
  ylab("Resultado partido Pred")+ 
  ggtitle("Datos vs Predicción Random Forest")+
  theme(legend.position = "none")+theme_fivethirtyeight()
ggsave("Datos vs Predicción RF.png")


#variable importance from Random Forests
varImpPlot(rffit2)

plot(randomForest(full_time_result ~ ., sbtrainval, keep.forest=FALSE, ntree=100), log="y")


####################### Logit


logitMod <- glm(as.numeric(full_time_result) ~ ., data=sbtest, family=binomial(link="logit"))

predicted <- predict(logitMod, sbtest, type="response")

ggplot(data = sbtest) +
  geom_boxplot(mapping = aes(x = full_time_result, y = predicted,
                             color=(full_time_result)))+
  theme(legend.position = "none")+theme_fivethirtyeight()
