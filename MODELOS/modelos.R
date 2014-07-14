#..........TESTING MODELS..........#

#source('titanic.R');

#...........Confussion Matrix: 

#Survived vs. Sex#
titanicmatrizconfu <- table(Survived=titanic$Survived,Sex=titanic$Sex)
plot(titanicmatrizconfu)
print(titanicmatrizconfu)
sum(diag(titanicmatrizconfu)/sum(titanicmatrizconfu))
sum(titanicmatrizconfu[1,1])/sum(titanicmatrizconfu[,1])
sum(titanicmatrizconfu[1,1])/sum(titanicmatrizconfu[1,])
sum(titanicmatrizconfu[2,1])/sum(titanicmatrizconfu[2,])

# Survived vs. Pclass#
titanicmatrizconfu <- table(Survived=titanic$Survived,Sex=titanic$Pclass)
plot(titanicmatrizconfu)
print(titanicmatrizconfu)
sum(diag(titanicmatrizconfu)/sum(titanicmatrizconfu))
sum(titanicmatrizconfu[1,1])/sum(titanicmatrizconfu[,1])
sum(titanicmatrizconfu[1,1])/sum(titanicmatrizconfu[1,])
sum(titanicmatrizconfu[2,1])/sum(titanicmatrizconfu[2,])


#........Linear Regression Model: Age vs Survived & Pclass

linmodel <- lm(Age ~ Survived + Pclass, data=titanic)
titanicpred<-titanic
titanicpred$pred<-predict(linmodel,newdata=titanic)
library(ggplot2)
ggplot(data=titanicpred,aes(x=pred,y=Age))+
  geom_point(alpha=0.2,color="black")+
  geom_smooth(aes(x=pred,y=Age),color="black")+
  geom_line(aes(x=Age,y=Age),color="blue",linetype=2)+
  scale_x_continuous(limits=c(0,100))+scale_y_continuous(limits=c(0,100))


#.........Logistic Regression Model: Survived = f(Pclass, Sex, Age)

formula <- paste("Survived",paste(c("Pclass","Sex","Age"),collapse='+'),sep=' ~ ')
logmodel <- glm(formula,data=titanic,family=binomial(link='logit'))
titanic$pred<-predict(logmodel,newdata=titanic,type='response')
library(ggplot2)
ggplot(titanic,aes(x=pred,color=Survived,linetype=Survived))+geom_density()
cm <- table(truth=titanic$Survived,prediction=titanic$pred>0.41)
print(cm)
plot(cm)
cm <- table(truth=titanic$Survived,prediction=titanic$pred>0.44)
print(cm)
plot(cm)
cm <- table(truth=titanic$Survived,prediction=titanic$pred>0.45)
print(cm)
plot(cm)

##....Otros modelos
formula <- paste("Survived",paste(c("Pclass","Sex","Age","Parch","SibSp","Embarked"),collapse='+'),sep=' ~ ')
logmodel <- glm(formula,data=titanic,family=binomial(link='logit'))
titanic$pred3<-predict(logmodel,newdata=titanic,type='response')

formula <- paste("Survived",paste(c("Pclass","Sex","Age","Parch","SibSp"),collapse='+'),sep=' ~ ')
logmodel <- glm(formula,data=titanic,family=binomial(link='logit'))
titanic$pred2<-predict(logmodel,newdata=titanic,type='response')

formula <- paste("Survived",paste(c("Pclass","Sex","Age","Parch"),collapse='+'),sep=' ~ ')
logmodel <- glm(formula,data=titanic,family=binomial(link='logit'))
titanic$pred1<-predict(logmodel,newdata=titanic,type='response')



#........Threshold(Lo usamos para calcular los umbrales en la regresión logistica)

library(SDMTools)
threshold<-optim.thresh(titanic$Survived,titanic$pred,threshold=100)
threshold

#.........Decision tree Model

library(rpart)
treez<-rpart(Survived ~ Age + Sex,data=titanic)
plot(treez)
text(treez,use.n=TRUE)

#............Serie Temporal
#(En nuestro caso la variable numérica de la edad, no nos dice nada con respecto el tiempo)

library(forecast)
sertempmodel <- auto.arima(titanic$Age)
plot(forecast(sertempmodel,h=1))

#..............Building single-variable models

tableSingSex <- table(titanic$Survived,titanic$Sex)
print(tableSingSex)
plot(tableSingSex)
tableSingPclass <- table(titanic$Survived,titanic$Pclass)
print(tableSingPclass)
plot(tableSingPclass)
tableSingParch <- table(titanic$Survived,titanic$Parch)
print(tableSingParch)
plot(tableSingParch)

#.............Reglas de asociacion
#(Si subimos el soporte -> Exigimos mayor numero de datos a comparar)(Si subimos la confianza -> Exigimos mayor numero de coincidencias)

library(Matrix)
library(arules)
library(arulesViz)
titanicAsoc <- titanic[,c("Survived","Sex","Pclass")]
rules <- apriori(titanicAsoc,parameter=list(support=0.02,confidence=0.95),appearance=list(rhs=c("Survived=0","Survived=1"),default="lhs"))
inspect(rules)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
rules <- apriori(titanicAsoc,parameter=list(support=0.3,confidence=0.75),appearance=list(rhs=c("Survived=0","Survived=1"),default="lhs"))
inspect(rules)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
rules <- apriori(titanicAsoc,parameter=list(support=0.5,confidence=0.65),appearance=list(rhs=c("Survived=0","Survived=1"),default="lhs"))
inspect(rules)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
rules <- apriori(titanicAsoc,parameter=list(support=0.1,confidence=0.90),appearance=list(rhs=c("Survived=0","Survived=1"),default="lhs"))
inspect(rules)
plot(rules)
plot(rules, method="graph", control=list(type="items"))



## ...Random Forests
library(randomForest)
set.seed(5123512)
fmodel <- randomForest(x=titanic[,c("Age","Sex","Pclass")],y=titanic$Survived,ntree=100,nodesize=7,importance=T)


