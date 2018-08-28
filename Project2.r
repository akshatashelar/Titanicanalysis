setwd("C:/Users/AKSHATA SHELAR/Documents/R/data analysis/Titanic Dataset")
dataset=read.csv('train.csv')
View(dataset)
dataset$Pclass = as.factor(dataset$Pclass)
summary(dataset$Pclass)
summary(dataset$Sex)
summary(dataset$Age)
summary(dataset$Embarked)
agelessthanone<-subset(dataset,Age<1)
agelessthanone
dataset$Survived = as.factor(dataset$Survived)
summary(dataset$Survived)
head(dataset)

dataset = na.omit(dataset)
rownames(dataset) <- 1:nrow(dataset)
dataset$Age[dataset$Age <= 18] = "child"
dataset$Age[(dataset$Age > 18) & (dataset$Age <= 60) & (dataset$Age != "child")] = "adult"
dataset$Age[(dataset$Age != "child") & (dataset$Age != "adult")] = "senior"
dataset$Age = as.factor(dataset$Age)

head(dataset)
dataset = dataset[,c(2,3,5,6,12)]
View(dataset)

dataset$Pclass = as.integer(dataset$Pclass)
dataset$Sex = as.integer(dataset$Sex)
dataset$Age = as.integer(dataset$Age)
dataset$Embarked = as.integer(dataset$Embarked)
dataset$Survived = as.integer(dataset$Survived)
head(dataset)



me_pclass = c(0,0,0)
me_pclass[1] = mean(dataset$Survived[dataset$Pclass==1])
me_pclass[2] = mean(dataset$Survived[dataset$Pclass==2])
me_pclass[3] = mean(dataset$Survived[dataset$Pclass==3])
plot(me_pclass, type="o", main="Main Effect of Passenger Class", xlab="Passenger Class", ylab="Main Effect",
     xaxt="n")
axis(1, at=c(1,2,3), labels=c("1st", "2nd", "3rd"))

class1<-subset(dataset,Survived==2 & Pclass==1)
nrow(class1)
class1<-subset(dataset,Survived==2 & Pclass==2)
nrow(class1)
class1<-subset(dataset,Survived==2 & Pclass==3)
nrow(class1)



me_sex = c(0,0)
me_sex[1] = mean(dataset$Survived[dataset$Sex==1])
me_sex[2] = mean(dataset$Survived[dataset$Sex==2])
plot(me_sex, type="o", main="Main Effect of Sex", xlab="Sex", ylab="Main Effect", xaxt="n")
axis(1, at=c(1,2), labels=c("Female", "Male"))
dev.off()

female<-subset(dataset,Survived==2 & Sex==1)
nrow(female)
male<-subset(dataset,Survived==2 & Sex==2)
nrow(male)









me_age = c(0,0,0)
me_age[1] = mean(dataset$Survived[dataset$Age==1])
me_age[2] = mean(dataset$Survived[dataset$Age==2])
me_age[3] = mean(dataset$Survived[dataset$Age==3])
plot(me_age, type="o", main="Main Effect of Age", xlab="Age", ylab="Main Effect", xaxt="n")
axis(1, at=c(1,2,3), labels=c("Adult", "Children", "Senior Citizen"))




me_emb = c(0,0,0)
me_emb[1] = mean(dataset$Survived[dataset$Embarked==1])
me_emb[2] = mean(dataset$Survived[dataset$Embarked==2])
me_emb[3] = mean(dataset$Survived[dataset$Embarked==3])
plot(me_emb, type="o", main="Main Effect of Port of Embarkment", xlab="Port of Embarkment", ylab="Main Effect",
     xaxt="n")
axis(1, at=c(1,2,3), labels=c("Cherbourg", "Queenstown", "Southampton"))

barplot(table(dataset$Pclass), xlab="Class", ylab="Frequency", main="Histogram of Passenger Class")
barplot(table(dataset$Sex), xlab="Sex", ylab="Frequency", main="Histogram of Sex")
barplot(table(dataset$Age), xlab="Age", ylab="Frequency", main="Histogram of Age")
barplot(table(dataset$Embarked), xlab="Port of Embarkment", ylab="Frequency", main="Histogram of Port of Embarkment")



min(dataset$Age)
max(dataset$Age)

setwd("C:/Users/AKSHATA SHELAR/Documents/R/data analysis/Titanic Dataset")
dataset=read.csv('train.csv')
dataset$Survived = as.integer(dataset$Survived)
#dataset = na.omit(dataset)
#rownames(dataset) <- 1:nrow(dataset)
input<-dataset[,c('Age','Survived')]
input
png(file="scatterplot1.png")
plot(x=input$Age,y=input$Survived,
     xlab="Age",
     ylab="Survived",
     xlim=c(0,80),
     ylim=c(0,1),
     main="Age vs Survived"
)
dev.off()



dataset$Sex = as.integer(dataset$Sex)

input<-dataset[,c('Sex','Survived')]
input
png(file="scatterplot2.png")
plot(x=input$Sex,y=input$Survived,
     xlab="Sex",
     ylab="Survived",
     xlim=c(1,2),
     ylim=c(0,1),
     main="Gender vs Survived"
)
dev.off()


dataset$Pclass = as.integer(dataset$Pclass)

input<-dataset[,c('Pclass','Survived')]
input
png(file="scatterplot3.png")
plot(x=input$Pclass,y=input$Survived,
     xlab="Pclass",
     ylab="Survived",
     xlim=c(1,3),
     ylim=c(1,2),
     main="Class vs Survived"
)
dev.off()

dataset$Embarked = as.integer(dataset$Embarked)

input<-dataset[,c('Embarked','Survived')]
input

png(file="scatterplot4.png")
plot(x=input$Embarked,y=input$Survived,
     xlab="Embarked",
     ylab="Survived",
     xlim=c(1,4),
     ylim=c(1,2),
     main="embarked vs Survived"
)
dev.off()






















