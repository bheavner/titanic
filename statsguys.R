# Following the logistic regression model approach from statsguys.wordpress.com/2014/01/03/first-post/

trainData <- read.csv("data/train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("data/test.csv", header = TRUE, stringsAsFactors = FALSE)

head(trainData)

plot(density(trainData$Age, na.rm = TRUE))
plot(density(trainData$Fare, na.rm = TRUE))

# explore on my own
plot(density(trainData$Pclass, na.rm = TRUE))

(counts <- table(trainData$Survived, trainData$Sex))

barplot(counts, xlab = "Gender", ylab = "Number", main = "survived or not, male/female")
counts[2] / (counts[1] + counts[2]) # 74.2% probability of survival for women
counts[4] / (counts[3] + counts[4]) # 18.9% probability of survival for men

# survival rate by passenger class
Pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People",
        main = "survived and deceased by cabin class")
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2]) # 63.0% of first class
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4]) # 47.3% of second class
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6]) # 24.2% of third class

# I'd like to see survived/deceased by class for men and for women...

# Other feature selection things to brainstorm...
# survival rate based on fare rages, survival rate based on age ranges etc. The key idea is that we’re trying to determine if any/which of our variables are related to what we’re trying to predict: Survived

# This is the end of the first lesson.