# Following the logistic regression model approach from
# statsguys.wordpress.com/2014/01/03/first-post/

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
(Pclass_survival <- table(trainData$Survived, trainData$Pclass))
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People",
        main = "survived and deceased by cabin class")
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2]) # 63.0% of first class
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4]) # 47.3% of second class
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6]) # 24.2% of third class

## This is the end of the first lesson. Following is my exploration.
require(ggplot2)

# Other feature selection things to brainstorm...
# survival rate based on fare rages, survival rate based on age ranges etc. The key idea is that we’re trying to determine if any/which of our variables are related to what we’re trying to predict: Survived

# I'd like to see survived/deceased by class for men and for women...
head(trainData$Survived & (trainData$Sex == 'female')) # logical test for living women
head((trainData$Sex == 'female') & trainData$Pclass == 1)) # women in first class
# how many women in first class?
sum(((trainData$Sex == 'female') & trainData$Pclass == 1) == "TRUE") #94

# how many of these survived?
sum(((trainData$Sex == 'female') & trainData$Pclass == 1 & trainData$Survived) == "TRUE") #91

# okay, so do that for men and women in each class. Make a data frame for the
# info. Sure seems like a for loop, but there's probably a better way in R...
#
# Really, this seems like a very silly and redundant approach. I look forward to learning the better ways to do it.

is_female_first_class <- trainData$Sex == 'female' & trainData$Pclass == 1
female_first_number <- sum(is_female_first_class) # 94
female_first_survivors <- sum(is_female_first_class & trainData$Survived) # 91

is_male_first_class <- trainData$Sex == 'male' & trainData$Pclass == 1
male_first_number <- sum(is_male_first_class) # 122
male_first_survivors <- sum(is_male_first_class & trainData$Survived) # 45

is_female_second_class <- trainData$Sex == 'female' & trainData$Pclass == 2
female_second_number <- sum(is_female_second_class) # 76
female_second_survivors <- sum(is_female_second_class & trainData$Survived) # 70

is_male_second_class <- trainData$Sex == 'male' & trainData$Pclass == 2
male_second_number <- sum(is_male_second_class) # 108
male_second_survivors <- sum(is_male_second_class & trainData$Survived) # 17

is_female_third_class <- trainData$Sex == 'female' & trainData$Pclass == 3
female_third_number <- sum(is_female_third_class) # 144
female_third_survivors <- sum(is_female_third_class & trainData$Survived) # 72

is_male_third_class <- trainData$Sex == 'male' & trainData$Pclass == 3
male_third_number <- sum(is_male_third_class) # 347
male_third_survivors <- sum(is_male_third_class & trainData$Survived) # 47

# now try to make a bar plot of the above with ggplot. 
# refs: www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
# www.r-tutor.com/r-introduction/data-frame
# stackoverflow.com/questions/12040245/how-to-increase-the-space-between-the-bars-in-a-bar-plot-inggplot2

# data has to be in data frame for ggplot. So, I want to build one. See www.cyclismo.org/tutorial/R/types.html

# I want to plot 3 pairs of bars on a bar plot: % survived by 1st, 2nd, and 3rd class, divided between men and women for each class.

percent_women_first_survived <- (female_first_survivors / female_first_number) * 100
percent_men_first_survived <- (male_first_survivors / male_first_number) * 100
percent_women_second_survived <- (female_second_survivors / female_second_number) * 100
percent_men_second_survived <- (male_second_survivors / male_second_number) * 100
percent_women_third_survived <- (female_third_survivors / female_third_number) * 100
percent_men_third_survived <- (male_third_survivors / male_third_number) * 100

df <- data.frame(gender = as.factor(c('Women', 'Men', 'Women', 'Men', 'Women', 'Men')),
                 class = as.factor(c('First', 'First', 'Second', 'Second', 'Third', 'Third')), 
                 percent_survived = c(percent_women_first_survived,
                       percent_men_first_survived,
                       percent_women_second_survived,
                       percent_men_second_survived,
                       percent_women_third_survived,
                       percent_men_third_survived)) 
str(df)
# a data frame with 6 observations of 3 variables.... and viewing it, it is wrong.

x.seq <- c(1, 2, 4, 5, 7, 8)

ggplot(data = transform(df, x = x.seq), aes(x = x, y = percent_survived, width = .85)) + 
  geom_bar(stat = "identity", aes(fill = gender)) + 
  labs(x="Percent Survived by Class", y="") +
  theme(legend.title=element_blank()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(breaks = NULL) +
  geom_text(aes(x = c(sum(x.seq[1:2])/2, sum(x.seq[3:4])/2, sum(x.seq[5:6])/2), 
                y = 0,
                label = c("First", "Second", "Third")),
                vjust = 1.2, size = 4)

