# Following the logistic regression model approach from
# statsguys.wordpress.com/2014/01/11/data-analytics-for-beginners-pt-2/

# load the data
trainData <- read.csv("data/train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("data/test.csv", header = TRUE, stringsAsFactors = FALSE)

## cleaning the TRAIN data

# remove unused variables - ID, ticket, fare, cabin, embarked
str(trainData)
trainData <- trainData[-c(1,9:12)]
str(trainData)

# replace qualitative variables with quantitative variables 
# (I'm not sure why not to just use factors)
trainData$Sex <- gsub("female", 1, trainData$Sex)
trainData$Sex <- gsub("^male", 0, trainData$Sex)
str(trainData$Sex)

# try to infer some ages based on title. Assume that people with similar titles are similar ages.

master_vector = grep("Master.",trainData$Name, fixed=TRUE)
miss_vector = grep("Miss.", trainData$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", trainData$Name, fixed=TRUE)
mr_vector = grep("Mr.", trainData$Name, fixed=TRUE)
dr_vector = grep("Dr.", trainData$Name, fixed=TRUE)

for(i in master_vector) {
  trainData$Name[i] = "Master"
}
for(i in miss_vector) {
  trainData$Name[i] = "Miss"
}
for(i in mrs_vector) {
  trainData$Name[i] = "Mrs"
}
for(i in mr_vector) {
  trainData$Name[i] = "Mr"
}
for(i in dr_vector) {
  trainData$Name[i] = "Dr"
}

master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,5])) {
    if (trainData$Name[i] == "Master") {
      trainData$Age[i] = master_age
    } else if (trainData$Name[i] == "Miss") {
      trainData$Age[i] = miss_age
    } else if (trainData$Name[i] == "Mrs") {
      trainData$Age[i] = mrs_age
    } else if (trainData$Name[i] == "Mr") {
      trainData$Age[i] = mr_age
    } else if (trainData$Name[i] == "Dr") {
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}


## Next, create new variables that might be useful features: child, family, mother