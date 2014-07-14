#Logistic regression model no.1#
ctab.test1 <- table(pred=titanic$pred1>0.44, Survived=titanic$Survived)
precision <- ctab.test[2,2]/sum(ctab.test[2,])
recall <- ctab.test[2,2]/sum(ctab.test[,2])

#Logistic regression model no.2#
ctab.test2 <- table(pred=titanic$pred1>0.44, Survived=titanic$Survived)
precision <- ctab.test[2,2]/sum(ctab.test[2,])
recall <- ctab.test[2,2]/sum(ctab.test[,2])


parametros<-function(col){
  ctab.test<-table(pred=col>0.44,Survived=titanic$Survived)
  precision <- ctab.test[2,2]/sum(ctab.test[2,])
  recall <- ctab.test[2,2]/sum(ctab.test[,2])
  enrich <- precision/mean(as.numeric(titanic$Survived))
  specificity <- ctab.test[1,1]/sum(ctab.test[,1])
  accuracy <- (ctab.test[1,1]+ctab.test[2,2])/sum(ctab.test[])
  fpr <- ctab.test[2,1]/(ctab.test[2,1]+ctab.test[1,1])
  fnr <- ctab.test[1,2]/(ctab.test[1,2]+ctab.test[2,2])
  
  
 result <- c(precision,recall,enrich,specificity,accuracy,fpr,fnr)
}

miMatrix<-matrix(c(parametros(titanic$pred),parametros(titanic$pred1),parametros(titanic$pred2),parametros(titanic$pred3)),ncol=7,byrow=TRUE)
colnames(miMatrix)<-c('prec','rec','enrich','spec','accuracy','fpr','fnr')
rownames(miMatrix)<-c('method#0','method#1','method#2','method#3')
miMatrix<-as.table(miMatrix)
print(miMatrix)
