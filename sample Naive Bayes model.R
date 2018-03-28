library("mlbench")
library("e1071")
library("klaR")


#Importing data "HouseVotes84"

data("HouseVotes84")

typeof(HouseVotes84)
head(HouseVotes84)
str(HouseVotes84)

#finding the sum of total NA values in one of the columns for one of the class
sum(is.na(HouseVotes84[HouseVotes84$Class == 'democrat',2]))

#sample plot
plot(as.factor(HouseVotes84[,2]))


#Function to get the sum of NA values for the corresponding column 'col' and c
#orresponding class 'cls'

na_by_col_class <- function(col, cls)
 {
  return(sum(is.na(HouseVotes84[HouseVotes84$Class == cls,col])))
 
}

na_by_col_class(2,'republican')

#function to get the probability of YES for the corresponding column 'col' and 
#corresponding class 'cls'

p_y_col_class <- function (col, cls) 
{
  sum_y <- sum(HouseVotes84[,col]=='y' & HouseVotes84$Class==cls, na.rm = T)
  sum_n <- sum(HouseVotes84[,col]=='n' & HouseVotes84$Class==cls, na.rm = T)
  return(sum_y/(sum_y+sum_n))
}


p_y_col_class(2,'republican')


NA_col <- function(i) 
{
  is.na(HouseVotes84[,i])
}


#Imputing missing values: NA values for a given issue and party by looking at how majarity of the
#other representatives from the same party voted on the issue.

for (i in 2:ncol(HouseVotes84))
{
  if(sum(NA_col(i)>0))
  {
    cR <- which(NA_col(i)& HouseVotes84$Class == 'republican',arr.ind= T)
    cD <- which(NA_col(i)& HouseVotes84$Class == 'democrat',arr.ind= T)
    
    HouseVotes84[cR,i] <- ifelse(p_y_col_class(i,'republic') > p_y_col_class(i,'republic'),'y','n')
    HouseVotes84[cD,i] <- ifelse(p_y_col_class(i,'democrat') > p_y_col_class(i,'democrat'),'y','n')
    
  }
  
}

na_by_col_class(2,'democrat')

p_y_col_class(2,'democrat')



#Splittiing data into TEST and TRAINING

set.seed(123)

TrainInd <- sample(1:nrow(HouseVotes84),0.8*nrow(HouseVotes84))

TrainData <- HouseVotes84[TrainInd,]
TestData <- HouseVotes84[-TrainInd,]


#Training the Navie Bayes Model
head(TrainData)
nb_model <- naiveBayes(Class~., data = TrainData)
nb_model
summary(nb_model)
str(nb_model)


#Testing the model

nb_test_predict <- predict(nb_model, TestData[,-1])


#confusion Matrix
table(pred=nb_test_predict, true= TestData$Class)


#fraction of correct predictions
mean(nb_test_predict == TestData$Class)


#Repeating the model for 'n' times to check the repeatability of the model

nb_multi_runs <- function(train_fraction, n)
{
  fraction_correct = rep(NA,n)
  
  for(i in 1:n)
  {
    
    TrainInd <- sample(1:nrow(HouseVotes84),train_fraction*nrow(HouseVotes84))
    
    TrainData <- HouseVotes84[TrainInd,]
    TestData <- HouseVotes84[-TrainInd,]
    
    nb_model <- naiveBayes(Class~., data = TrainData)
    
    nb_test_predict <- predict(nb_model, TestData[,-1])
    
    fraction_correct[i] <- mean(nb_test_predict == TestData$Class)
    
  }
  return(fraction_correct)
  
}


Fraction_correct_predictions <- nb_multi_runs(0.8,20)
Fraction_correct_predictions

summary(Fraction_correct_predictions)

sd(Fraction_correct_predictions)






























