library(dplyr)
library(tidyr)

df <- read.csv("C:/Users/jugvi/OneDrive/Documents/Sem 2/Adv Stat Project/data_final.csv", header = TRUE, sep = ',')
View(df)

#Data Cleaning

clean_df <- data.frame(df)
typeof(clean_df)
print(clean_df)
clean_df <- subset(clean_df, select = -c(AMH.ng.mL.))
print(clean_df)
str(clean_df)
clean_df[is.na(clean_df)] <- 0
View(clean_df)

clean <- clean_names(df)
colnames(clean)

clean_df <- data.frame(clean_df)
typeof(clean_df)
print(clean_df)

clean_df <- subset(clean_df, select = -c(AMH.ng.mL.,II....beta.HCG.mIU.mL.))
print(clean_df)

str(clean_df) 


clean_df[is.na(clean_df)] <- 0
View(clean_df)

df[] <- lapply(clean_df, function(x) as.numeric(as.character(x)))
df
sapply(df, class)
str(df)
all( is.finite( df ) )

View(df)
na.omit(df)


sapply(df, mode)  #some column datatypes are wrongly interpreted as character, supposed to be numeric
#Data Cleaning and changing of the datatype was done using python jupyter.nb

#transform(pcos_df, num1 = as.numeric(pcos_df$II....beta.HCG.mIU.mL.), num2 = as.numeric(pcos_df$AMH.ng.mL.))
#transform(num1 = as.numeric(pcos_df$II....beta.HCG.mIU.mL.))
#pcos_df$II....beta.HCG.mIU.mL. = as.numeric(df$II....beta.HCG.mIU.mL.)
#sapply(df,class)


#importing clean PCOS dataframe
clean_df <- read.csv("C:\\Users\\Soham\\Dropbox\\My PC (LAPTOP-AGG4JAFB)\\Desktop\\2nd Sem\\JK PCOS\\PCOS_data.csv")
clean_df.
sapply(clean_df,mode)
View(clean_df)


str(clean_df)

## Chi Square test

attach(df)
table(df)

#H0: Presence of pcos is independent of type of blood group.
#H1: Presence of pcos is not independent of type of blood group.
chisq.test(table(df$PCOS..Y.N., df$Blood.Group))
#p-value is greater than 0.05, hence we do not have enough evidence to reject H0. Therefore, accept H0.

#H0: Presence of pcos is independent of number of follicles.
#H1: Presence of pcos is not independent of number of follicles.
chisq.test(table(df$PCOS..Y.N., df$Follicle.No...R.))

#H0: Presence of pcos is independent of HCG level
#H1: Presence of pcos is not independent of HCG level.
chisq.test(table(df$PCOS..Y.N., df$I...beta.HCG.mIU.mL.))

#H0: Presence of pcos is independent of BMI.
#H1: Presence of pcos is not independent of BMI.
chisq.test(table(df$PCOS..Y.N., df$BMI))
#since p-value=0.3247 is greater than 0.05, we do not have sufficient evidence to reject H0. Hence presence of pcos is independent of BMI.

#H0: Presence of pcos is independent of menstrual cycle repetition.
#H1: Presence of pcos is not independent of menstrual cycle repetiton.
chisq.test(table(df$PCOS..Y.N., df$Cycle.R.I.))
#since p-value=0.00000000000000022 is less than 0.05, presence of pcos is significantly dependent on menstrual cycle repetition.

#H0: Presence of pcos is independent of length of menstrual cycle.
#H1: Presence of pcos is not independent of length of menstrual cycle.
chisq.test(table(df$PCOS..Y.N., df$Cycle.length.days.))
#since p-value=0.000000000000001126 is less than 0.05, presence of pcos is significantly dependent on length of menstrual cycle.

## Performing PCA

pca_df <- princomp(na.omit(clean_df), cor = TRUE, scores = TRUE)

summary(pca_df)
View(pca_df)

unclass(pca_df$loadings)
head(pca_df$scores)
View(pca_df$scores)[1:44]
View(head(pca_df$scores))



screeplot(pca_df, type = "l", npcs = 43, main = "Screeplot of all the PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(pca_df$sdev^2 / sum(pca_df$sdev^2))  

plot(cumpro[0:44], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 18, col="blue", lty=5)  
abline(h = 0.70759, col="blue", lty=5)  
legend("topleft", legend=c("Cut-off @ PC18"),
       col=c("blue"), lty=5, cex=0.6)







