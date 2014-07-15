#..........titanic_test TEST..........#

#DATA LOADING#

titanic_test<-read.csv('/Users/jorgeramirezcarrasco/Desktop/COM/ML-Experiments-master/datasets/test.csv',header=TRUE,sep=',')
titanic_test$Pclass<-as.factor(titanic_test$Pclass)


#Age
mean1<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==1)])
mean1f<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==1)&(titanic_test$Sex=='female')])
mean1m<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==1)&(titanic_test$Sex=='male')])
sd1f<-sd(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==1)&(titanic_test$Sex=='female')])
sd1m<-sd(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==1)&(titanic_test$Sex=='male')])
ct1f<-length(titanic_test$Age[(is.na(titanic_test$Age))&(titanic_test$Pclass==1)&(titanic_test$Sex=='female')])
ct1m<-length(titanic_test$Age[(is.na(titanic_test$Age))&(titanic_test$Pclass==1)&(titanic_test$Sex=='male')])

mean2<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==2)])
mean2m<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==2)&(titanic_test$Sex=='male')])
mean2f<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==2)&(titanic_test$Sex=='female')])
sd2f<-sd(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==2)&(titanic_test$Sex=='female')])
sd2m<-sd(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==2)&(titanic_test$Sex=='male')])
ct2f<-length(titanic_test$Age[(is.na(titanic_test$Age))&(titanic_test$Pclass==2)&(titanic_test$Sex=='female')])
ct2m<-length(titanic_test$Age[(is.na(titanic_test$Age))&(titanic_test$Pclass==2)&(titanic_test$Sex=='male')])

mean3<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==3)])
mean3m<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==3)&(titanic_test$Sex=='male')])
mean3f<-mean(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==3)&(titanic_test$Sex=='female')])
sd3f<-sd(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==3)&(titanic_test$Sex=='female')])
sd3m<-sd(titanic_test$Age[(!is.na(titanic_test$Age))&(titanic_test$Pclass==3)&(titanic_test$Sex=='male')])
ct3f<-length(titanic_test$Age[(is.na(titanic_test$Age))&(titanic_test$Pclass==3)&(titanic_test$Sex=='female')])
ct3m<-length(titanic_test$Age[(is.na(titanic_test$Age))&(titanic_test$Pclass==3)&(titanic_test$Sex=='male')])

titanic_test$Age[(is.na(titanic_test$Age)&(titanic_test$Pclass==1)&(titanic_test$Sex=="female"))]<-seq(mean1f,mean1f,length.out=ct1f)+runif(ct1f,max=1,min=-1)*sd1f
print('Age of female people in class 1 adapted')
titanic_test$Age[(is.na(titanic_test$Age)&(titanic_test$Pclass==1)&(titanic_test$Sex=="male"))]<-seq(mean1m,mean1m,length.out=ct1m)+runif(ct1m,max=1,min=-1)*sd1m
print('Age of male people in class 1 adapted')
titanic_test$Age[(is.na(titanic_test$Age)&(titanic_test$Pclass==2)&(titanic_test$Sex=="female"))]<-seq(mean2f,mean2f,length.out=ct2f)+runif(ct2f,max=1,min=-1)*sd2f
print('Age of female people in class 2 adapted')
titanic_test$Age[(is.na(titanic_test$Age)&(titanic_test$Pclass==2)&(titanic_test$Sex=="male"))]<-seq(mean2m,mean2m,length.out=ct2m)+runif(ct2m,max=1,min=-1)*sd2m
print('Age of male people in class 2 adapted')
titanic_test$Age[(is.na(titanic_test$Age)&(titanic_test$Pclass==3)&(titanic_test$Sex=="female"))]<-seq(mean3f,mean3f,length.out=ct3f)+runif(ct3f,max=1,min=-1)*sd3f
print('Age of female people in class 3 adapted')
titanic_test$Age[(is.na(titanic_test$Age)&(titanic_test$Pclass==3)&(titanic_test$Sex=="male"))]<-seq(mean3m,mean3m,length.out=ct3m)+runif(ct3m,max=1,min=-1)*sd3m
print('Age of male people in class 3 adapted')

#Eliminamos los que no tienen puerto 
titanic_test<-titanic_test[!(titanic_test$Embarked==""),]
titanic_test$Embarked<-factor(titanic_test$Embarked)

#Adaptamos el precio del ticket para evitar Nan
mean_fare<-mean(titanic_test$Fare[!(is.na(titanic_test$Fare))])
length_fare <- length(titanic_test$Fare[is.na(titanic_test$Fare)])
titanic_test$Fare[(is.na(titanic_test$Fare))]<-seq(mean_fare,mean_fare,length.out=length_fare)


#Creamos modelo

#.........Logistic Regression Model: Survived = f(Pclass, Sex, Age)

##....Predecimos

##Modelo Regresión logística
#titanic_test$Survived<-ifelse(predict(logmodel,newdata=titanic_test,type='response')>0.44,1,0)
##Random Forest
titanic_test$Survived <- predict(fit_rand_forest, titanic_test, OOB=TRUE, type = "response")
##Random Forest de Party
##titanic_test$Survived<- predict(fmodel, titanic_test)
             
#....Creamos dataframe de test

titanic_test <- titanic_test[c("PassengerId","Survived")]
write.csv(titanic_test,file="titanic_test.csv",row.names=FALSE)