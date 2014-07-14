#..........VALORACIÓN DE MODELOS..........#

###Logistic Regression Models

#First we make a double density plot
ggplot(data=titanic) + geom_density(aes(x=pred,color=Survived,linetype=Survived))

#Second we plot the ROC Curve
library('ROCR')
eval <- prediction(titanic$pred,titanic$Survived) 
plot(performance(eval,"tpr","fpr")) 
print(c('auc',attributes(performance(eval,'auc'))$y.values[[1]]))

#And calculate de AUC
print(c('auc',attributes(performance(eval,'auc'))$y.values[[1]]))

#Log likelihood
loglikelihood1<-sum(ifelse(titanic$Survived==1,log(titanic$pred),log(1-titanic$pred)))
print(c('Log likelihood 1',loglikelihood1))
loglikelihood2<-sum(ifelse(titanic$Survived==1,log(titanic$pred),
                           log(1-titanic$pred)))/dim(titanic)[[1]]
print(c('Log likelihood 2',loglikelihood2))
pNull<-sum(ifelse(titanic$Survived==1,1,0))/dim(titanic)[[1]]
loglikelihoodN<-sum(ifelse(titanic$Survived==1,1,0))*log(pNull)+
  sum(ifelse(titanic$Survived==1,0,1))*log(1-pNull)
print(c('Log likelihood pNull',loglikelihoodN))

#Plotting the threshold
library(ROCR)
library(grid)

predObj<-prediction(titanic$pred,titanic$Survived)
precObj<-performance(predObj,measure='prec')
recObj<-performance(predObj,measure='rec')

precision<-(precObj@y.values)[[1]]
prec.x<-(precObj@x.values)[[1]]
recall<-(recObj@y.values)[[1]]

rocFrame<-data.frame(threshold=prec.x,precision=precision,recall=recall)

nplot <- function(plist){
  n<-length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y){
    viewport(layout.pos.row=x,layout.pos.col=y)
  }
  for(i in 1:n){
    print(plist[[i]],vp=vplayout(i,1))
  }
}

pnull<-mean(as.numeric(titanic$Survived))

p1 <- ggplot(rocFrame, aes(x=threshold))+
  geom_line(aes(y=precision/pnull))+coord_cartesian(xlim=c(0.40,0.45),ylim=c(0,1))
p2 <- ggplot(rocFrame, aes(x=threshold))+
  geom_line(aes(y=recall))+coord_cartesian(xlim=c(0.40,0.45))

nplot(list(p1,p2))

#Evaluating the classifier with a threshold of 0.44

#Ctab.test(ctab.test[1,1]=TN;ctab.test[1,2]=FN;ctab.test[2,1]=FP;ctab.test[2,2]=TP;)
ctab.test <- table(pred=titanic$pred>0.44, Survived=titanic$Survived)
print(ctab.test)
#Precision(TP/(TP+FP))( De las que ha dicho como positivas, las que verdaderamente lo son)
precision <- ctab.test[2,2]/sum(ctab.test[2,])
print(c("precision:",precision))
#Recall(TP/(TP+FN))(Casos positivos que acierta)
recall <- ctab.test[2,2]/sum(ctab.test[,2])
print(c("recall:",recall))
#Enrich(precision/mean())(range times higher than the overall average)
enrich <- precision/mean(as.numeric(titanic$Survived))
print(c("enrichment:",enrich))
#Specificity(TN/TN+FP)(Casos negativos que acierta)
specificity <- ctab.test[1,1]/sum(ctab.test[,1])
specificity
#Accuracy(TP+TN/(TN+FP+TP+FN))(%Acierto)
accuracy <- (ctab.test[1,1]+ctab.test[2,2])/sum(ctab.test[])
accuracy
#False Positive Rate(FP/FP+TN)
fpr <- ctab.test[2,1]/(ctab.test[2,1]+ctab.test[1,1])
fpr
#False Negative Rate(FN/FN+TP)
fnr <- ctab.test[1,2]/(ctab.test[1,2]+ctab.test[2,2])
fnr


#..AIC(Aikake Information Criterion)(the lowest AIC   ->  the best fit)
#..FScore(You should expect it to converge in about six to eight iterations. 
#       If there are more iter- ations than that,
# then the algorithm may not have converged, and the model may not be valid.)
summary(logmodel)