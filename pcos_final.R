df <- read.csv(file = "C:/Users/jugvi/OneDrive/Documents/Sem 2/Adv Stat Project/data_final.csv", header = TRUE, sep = ',')
View(df)
library(dplyr)
library(ggplot2)
ggplot(df) +geom_bar(aes(x = PCOS..Y.N.)) +ggtitle(label = 'PCOS negative v/s positive')


df_with_pcos <- df[df$PCOS..Y.N. != "0",]
View(df_with_pcos)

df_without_pcos <- df[df$PCOS..Y.N. != "1",]
df_without_pcos

#to check if the dist is normal
par(mfrow = c(1,2))
hist(df_with_pcos$Weight..Kg.)
hist(df_without_pcos$Weight..Kg.)

#H0: weight of with pcos = weight of without pcos
#H1: weight of with pcos != weight of with pcos
t.test(df_with_pcos$Weight..Kg. , df_without_pcos$Weight..Kg. , var.equal = FALSE)

#sample data
c_with<-c(df_with_pcos$Weight..Kg.)
c_without<-c(df_without_pcos$Weight..Kg.)


#make a data frame with two columns named name and value
c_with_df<-data.frame(name="c_with", value=c_with)
c_without_df<-data.frame(name="c_without", value=c_without)

#combine separate data frames into one large dataframe
combined<-rbind(c_with_df, c_without_df)
View(combined)

#plot the values grouped by name
par(mfrow = c(1,1))
boxplot(value ~ name, data=combined, xlab = 'pcos status', ylab = 'weight')


#thyroid (0.5 - 5)
with_1 <-c(df_with_pcos$TSH..mIU.L.)
without_1 <-c(df_without_pcos$TSH..mIU.L.)

with_1_df<-data.frame(name="with_1", value=with_1)
without_1_df<-data.frame(name="without_1", value=without_1)

combined_1<-rbind(with_1_df,without_1_df)
View(combined_1)
boxplot(value ~ name, data=combined_1, xlab = 'data points', ylab = 'Thyroid level', ylim =c(0,10))


#insulin (2-20)
with_2 <-c(df_with_pcos$Insulin.levels..æIU.ml.)
without_2 <-c(df_without_pcos$Insulin.levels..æIU.ml.)

with_2_df<-data.frame(name="with_2", value=with_2)
without_2_df<-data.frame(name="without_2", value=without_2)

combined_2<-rbind(with_2_df,without_2_df)
View(combined_2)
boxplot(value ~ name, data=combined_2, xlab = 'data points', ylab = 'Insulin level')


#endometrium (<13)
with_3 <-c(df_with_pcos$Endometrium..mm.)
without_3 <-c(df_without_pcos$Endometrium..mm.)

with_3_df<-data.frame(name="with_3", value=with_3)
without_3_df<-data.frame(name="without_3", value=without_3)

combined_3<-rbind(with_3_df,without_3_df)
View(combined_3)
boxplot(value ~ name, data=combined_3, xlab = 'data points', ylab = 'Endometrium size')

#For regression

#1.to check for multicollinearity
cor(df_with_pcos$FSH.mIU.mL.,df_with_pcos$LH.mIU.mL.) #negative low
cor(df_with_pcos$TSH..mIU.L.,df_with_pcos$PRG.ng.mL.) #positive low

#2.to check if the dependent variable is normally distributed
hist(df_with_pcos$Insulin.levels..æIU.ml.) #no
hist(df_with_pcos$Weight..Kg.) #yes

shapiro.test(df_with_pcos$Weight..Kg.)
#value is less than 0.05. Hence weight is not normally distributed

#therefore, transforming

log_1 <-log10((df_with_pcos$Weight..Kg.))
shapiro.test(log_1)
#the value is greater than 0.05,so now it is normally distributed
hist(log_1)

#3.to check if relation is linear
plot(df_with_pcos$Weight..Kg.~df_with_pcos$FSH.mIU.mL.) #no
plot(df_with_pcos$Weight..Kg.~df_with_pcos$LH.mIU.mL.) #no
plot(df_with_pcos$Weight..Kg.~df_with_pcos$AMH.ng.mL.) #little
plot(df_with_pcos$Weight..Kg.~df_with_pcos$FSH.LH) #no
plot(df_with_pcos$Weight..Kg.~df_with_pcos$TSH..mIU.L.) #little
plot(df_with_pcos$Weight..Kg.~df_with_pcos$BMI) #yes
plot(df_with_pcos$Weight..Kg.~df_with_pcos$PRG.ng.mL.) #little

#hence, cannot use linear regression

#multiple linear regression
model_1 <-lm(log_1~ df_with_pcos$TSH..mIU.L. + df_with_pcos$PRG.ng.mL.)
summary(model_1)

par(mfrow=c(2,2))
plot(model_1)
par(mfrow=c(1,1))

#this is a badly performing multiple regression model.

#Mann whitney u test
#H0: Insulin levels in people with pcos and without pcos is similar
#H1: Insulin levels in people with pcos and without pcos is not similar

wilcox.test(df_with_pcos$Insulin.levels..æIU.ml.,df_without_pcos$Insulin.levels..æIU.ml.)
#Enough evidence to reject null hypothesis.

#H0: FSH levels in people with pcos and without pcos is similar
#H1: FSH levels in people with pcos and without pcos is not similar

wilcox.test(df_with_pcos$FSH.mIU.mL.,df_without_pcos$FSH.mIU.mL.)
#Enough evidence to reject null hypothesis.

#H0: RBS levels in people with pcos and without pcos is similar
#H1: RBS levels in people with pcos and without pcos is not similar
wilcox.test(df_with_pcos$RBS.mg.dl.,df_without_pcos$RBS.mg.dl.)
#not enough evidence to reject null hypothesis


#Polynomial regression model 1
plot(df_with_pcos$Weight..Kg.~df_with_pcos$TSH..mIU.L.)
plot(df_with_pcos$Weight..Kg.~df_with_pcos$PRG.ng.mL.)

k <- 10
degree <- 5
folds <-cut(seq(1,nrow(df_with_pcos)),breaks=k,labels=FALSE)

mse = matrix(data=NA,nrow=k,ncol=degree)

for(i in 1:k){
  
  testIndexes <- which(folds ==i,arr.ind=TRUE)
  testData <- df_with_pcos[testIndexes, ]
  trainData <- df_with_pcos[-testIndexes, ]
  
  for (j in 1:degree){
    fit.train = lm(df_with_pcos$Weight..Kg. ~ poly(df_with_pcos$TSH..mIU.L.,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$Weight..Kg.)^2)
  }
}
colMeans(mse)

#lowest is 148.4277. Hence, h=1

best = lm(df_with_pcos$Weight..Kg. ~ poly(df_with_pcos$TSH..mIU.L.,1,raw=T))
summary(best)

#since the r2 value is very low, this is a badly performin polynomial regression model


#Polynomial regression model 2

plot(df_with_pcos$Weight..Kg.,df_with_pcos$Insulin.levels..æIU.ml.)
polyreg_model <- lm(formula = df_with_pcos$Weight..Kg.~ poly(df_with_pcos$Insulin.levels..æIU.ml.,10,raw=TRUE))
summary(polyreg_model)$r.squared

#since  the r2 value is very low, this is a badly performing model.

#Logistic1

sample <- sample(c(TRUE,FALSE),nrow(df),replace=TRUE,prob=c(0.7,0.3))
train <- df[sample,]
test <- df[!sample,]

model_logistic <- glm(train$PCOS..Y.N.~train$Insulin.levels..æIU.ml. + train$Weight.gain.Y.N., family='binomial')
options(scipen=999)
summary(model_logistic)
pscl::pR2(model_logistic)["McFadden"]
library(pscl)

ggplot(train, aes(x=train$Insulin.levels..æIU.ml. + train$Weight.gain.Y.N., y=train$PCOS..Y.N)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))


#logistic 2
sample <- sample(c(TRUE,FALSE),nrow(df),replace=TRUE,prob=c(0.7,0.3))
train <- df[sample,]
test <- df[!sample,]

model_logistic <- glm(train$PCOS..Y.N.~train$PRG.ng.mL. + train$hair.growth.Y.N., family='binomial')
options(scipen=999)
summary(model_logistic)
pscl::pR2(model_logistic)["McFadden"]
library(pscl)

ggplot(train, aes(x=train$PRG.ng.mL. + train$hair.growth.Y.N., y=train$PCOS..Y.N)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))


#logistic 3
model_logistic <- glm(train$PCOS..Y.N.~train$Insulin.levels..æIU.ml. + train$Reg.Exercise.Y.N. + train$Skin.darkening..Y.N., family='binomial')
options(scipen=999)
summary(model_logistic)
pscl::pR2(model_logistic)["McFadden"]
library(pscl)

ggplot(train, aes(x=train$Insulin.levels..æIU.ml. + train$Reg.Exercise.Y.N. + train$Skin.darkening..Y.N., y=train$PCOS..Y.N)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))

#logistic 4
model_logistic <- glm(train$PCOS..Y.N.~train$hair.growth.Y.N. + train$Hair.loss.Y.N., family='binomial')
options(scipen=999)
summary(model_logistic)
pscl::pR2(model_logistic)["McFadden"]

ggplot(train, aes(x=train$hair.growth.Y.N. + train$Hair.loss.Y.N., y=train$PCOS..Y.N)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))

#logistic 5
model_logistic <- glm(train$PCOS..Y.N.~train$Fast.food..Y.N. + train$Reg.Exercise.Y.N., family='binomial')
options(scipen=999)
summary(model_logistic)
pscl::pR2(model_logistic)["McFadden"]

ggplot(train, aes(x=train$Fast.food..Y.N. + train$Reg.Exercise.Y.N., y=train$PCOS..Y.N)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))

#logistic 6
model_logistic <- glm(train$PCOS..Y.N.~train$Skin.darkening..Y.N. + train$Pimples.Y.N., family='binomial')
options(scipen=999)
summary(model_logistic)
pscl::pR2(model_logistic)["McFadden"]

ggplot(train, aes(x=train$Skin.darkening..Y.N. + train$Pimples.Y.N., y=train$PCOS..Y.N)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))
