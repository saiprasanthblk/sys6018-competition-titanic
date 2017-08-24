# Assign 1

library(stringr)
library(dplyr)
library(readr)
library(stringi)


dat1 <- read_csv("train.csv")

dat2 <- read_csv("test.csv")

dat1$Sex <- ifelse(dat1$Sex == 'male', 1, 0) # coding gender as 1's and 0's

dat2$Sex <- ifelse(dat2$Sex == 'male', 1, 0) # coding gender as 1's and 0's

#We need to factorize certian variables

dat1$Survived <- factor(dat1$Survived)
dat1$Pclass <- factor(dat1$Pclass)
dat1$Sex <- factor(dat1$Sex)
dat1$Embarked <- factor(dat1$Embarked)

dat2$Pclass <- factor(dat2$Pclass)
dat2$Sex <- factor(dat2$Sex)
dat2$Embarked <- factor(dat2$Embarked)

# Model

re.lg <- glm(Survived ~ PassengerId + Pclass + Sex + Age + SibSp + Parch + Fare + Embarked , data=dat1, family = "binomial")

summary(re.lg)

# Let me build a model with only the significant variables

final.lg <- glm(Survived ~ Pclass + Sex + SibSp, data=dat1, family = "binomial")

# In terms of training accuracy, I have found through multiple combinations that above model is best

probs<-as.vector(predict(final.lg, newdata = dat1, type="response"))
preds <- rep(0, 891)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,dat1$Survived)

# I got close to 80% accuracy

# Let me apply the same model on test set

dat2$Survived <- NA

probs<-as.vector(predict(final.lg, newdata = dat2, type="response"))
preds2 <- rep(0, 418)  # Initialize prediction vector
preds2[probs>0.5] <- 1 # p>0.5 -> 1
dat2$Survived <- preds2

# Obtained an accuracy of 77% on Kaggle


# Writing output to an excel file

final <- cbind(dat2$PassengerId, dat2$Survived)

colnames(final) <- c("PassengerId", "Survived")

write.csv(final, file="predout3.csv", row.names = FALSE, col.names = c("PassengerId", "Survived"))