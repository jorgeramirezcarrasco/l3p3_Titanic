#..........TITANIC..........#

if(getwd()!="~/R"){
  setwd("~/R")
}

#DATA LOADING#
titanic<-read.csv('/Users/jorgeramirezcarrasco/Desktop/COM/ML-Experiments-master/datasets/titaniccomplete.csv',header=TRUE,sep=',')
titanic$Pclass<-as.factor(titanic$Pclass)
titanic$Survived<-as.factor(titanic$Survived)

#DATA ADAPTATION#

#Age
mean1<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==1)])
mean1f<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==1)&(titanic$Sex=='female')])
mean1m<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==1)&(titanic$Sex=='male')])
sd1f<-sd(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==1)&(titanic$Sex=='female')])
sd1m<-sd(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==1)&(titanic$Sex=='male')])
ct1f<-length(titanic$Age[(is.na(titanic$Age))&(titanic$Pclass==1)&(titanic$Sex=='female')])
ct1m<-length(titanic$Age[(is.na(titanic$Age))&(titanic$Pclass==1)&(titanic$Sex=='male')])

mean2<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==2)])
mean2m<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==2)&(titanic$Sex=='male')])
mean2f<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==2)&(titanic$Sex=='female')])
sd2f<-sd(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==2)&(titanic$Sex=='female')])
sd2m<-sd(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==2)&(titanic$Sex=='male')])
ct2f<-length(titanic$Age[(is.na(titanic$Age))&(titanic$Pclass==2)&(titanic$Sex=='female')])
ct2m<-length(titanic$Age[(is.na(titanic$Age))&(titanic$Pclass==2)&(titanic$Sex=='male')])

mean3<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==3)])
mean3m<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==3)&(titanic$Sex=='male')])
mean3f<-mean(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==3)&(titanic$Sex=='female')])
sd3f<-sd(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==3)&(titanic$Sex=='female')])
sd3m<-sd(titanic$Age[(!is.na(titanic$Age))&(titanic$Pclass==3)&(titanic$Sex=='male')])
ct3f<-length(titanic$Age[(is.na(titanic$Age))&(titanic$Pclass==3)&(titanic$Sex=='female')])
ct3m<-length(titanic$Age[(is.na(titanic$Age))&(titanic$Pclass==3)&(titanic$Sex=='male')])

titanic$Age[(is.na(titanic$Age)&(titanic$Pclass==1)&(titanic$Sex=="female"))]<-seq(mean1f,mean1f,length.out=ct1f)+runif(ct1f,max=1,min=-1)*sd1f
print('Age of female people in class 1 adapted')
titanic$Age[(is.na(titanic$Age)&(titanic$Pclass==1)&(titanic$Sex=="male"))]<-seq(mean1m,mean1m,length.out=ct1m)+runif(ct1m,max=1,min=-1)*sd1m
print('Age of male people in class 1 adapted')
titanic$Age[(is.na(titanic$Age)&(titanic$Pclass==2)&(titanic$Sex=="female"))]<-seq(mean2f,mean2f,length.out=ct2f)+runif(ct2f,max=1,min=-1)*sd2f
print('Age of female people in class 2 adapted')
titanic$Age[(is.na(titanic$Age)&(titanic$Pclass==2)&(titanic$Sex=="male"))]<-seq(mean2m,mean2m,length.out=ct2m)+runif(ct2m,max=1,min=-1)*sd2m
print('Age of male people in class 2 adapted')
titanic$Age[(is.na(titanic$Age)&(titanic$Pclass==3)&(titanic$Sex=="female"))]<-seq(mean3f,mean3f,length.out=ct3f)+runif(ct3f,max=1,min=-1)*sd3f
print('Age of female people in class 3 adapted')
titanic$Age[(is.na(titanic$Age)&(titanic$Pclass==3)&(titanic$Sex=="male"))]<-seq(mean3m,mean3m,length.out=ct3m)+runif(ct3m,max=1,min=-1)*sd3m
print('Age of male people in class 3 adapted')

#Eliminamos los que no tienen puerto
titanic<-titanic[!(titanic$Embarked==""),]
titanic$Embarked<-factor(titanic$Embarked)
#Creamos data frames de las tres clases
titanic1<-titanic[(titanic$Pclass=="1"),]
titanic2<-titanic[(titanic$Pclass=="2"),]
titanic3<-titanic[(titanic$Pclass=="3"),]

#Creamos data frames de los tres puertos
titanicQ<-titanic[(titanic$Embarked=='Q'),]
titanicS<-titanic[(titanic$Embarked=='S'),]
titanicC<-titanic[(titanic$Embarked=='C'),]