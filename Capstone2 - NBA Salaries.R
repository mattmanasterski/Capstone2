################################
# 2019/12/20
# Matthew Manasterski, MBA
# PH125.9x - Capstone Project - Your Own Project - Predicting NBA players salary
#

# Load necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

# Read in the dataset
nba_players <- read.csv('nba_final.csv')

nrow(nba_players)
# Explore the structure of the dataset by looking at the head columnns
head(nba_players)

# Explore the structure of the dataset by looking at its structure
str(nba_players)
#From the structure we can see some stats we might want to use like Age, G, GS, PTS

#Display Summary of the set
summary(nba_players)

# From the summary and the structure we can see right away there are some columns that will not be useful to us at all
# Rk, Player.x Player_ID, Pos2 as it has minimal values and 1396 NA's, 
# We will keep the variables that we think how good the player is and salary he could command
nba_players <- select(nba_players, Pos1, Age, Tm, G, GS, MP, FG, X3P, X2P, FT, TRB, AST, STL, BLK, PTS, Salary, Season, Play)

# Before exploring the data check for missing values in nba_players, strip if any found
nba_players <- na.omit(nba_players)

# Review the new structure
str(nba_players)


# Plot Players Salary
ggplot(nba_players,aes(Salary)) + geom_histogram(color='darkblue',fill='lightblue') + ggtitle('NBA Salaries')
# We see that majority of the players make under 5 Milliom dollars in fact we know from our earlier summary medium salary is $3,384,298 well below mean salary of $6,790,048 with max salary $37,457,154


#Lets a explore some predictors that might good indicators of Salary 
# Plot Salary by Position
ggplot(nba_players,aes(factor(Pos1),Salary)) + geom_boxplot(aes(color=factor(Pos1))) +theme_bw() + ggtitle('Salary by Position')


#Plot Salary by Age
ggplot(nba_players,aes(Age,Salary)) + 
  geom_point(aes(color=Salary),alpha=0.5)  + 
  scale_color_continuous(low='lightgreen',high='darkgreen') + 
  geom_smooth() + theme_bw()
#From this plot we see that on average player earn their at their peak between ages of 30 to 34 so this is good indicator to keep

# Does it matter what team you play on Plot Salary by team
ggplot(nba_players,aes(factor(Tm),Salary)) + 
  geom_boxplot(aes(color=factor(Tm))) +theme_bw() + 
  ggtitle('Salaries by team') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Plot G-Games Played 
ggplot(nba_players,aes(G,Salary)) + 
  geom_point(alpha=0.2) + 
  geom_smooth() + 
  theme_bw()
# Games played seems like a good steady indicator

# Plot Games Started
ggplot(nba_players,aes(GS,Salary)) + 
  geom_point(alpha=0.2) + 
  geom_smooth() + 
  theme_bw()
# We see that Games started is a good indicator, althought it peaks at around 73 Games, this probably to account for injuries so we might 

#  PlotMinutes played 
ggplot(nba_players,aes(MP,Salary)) + 
  geom_point(alpha=0.2) + 
  geom_smooth() + 
  theme_bw()
# We see that players that earn more defenitely play more minutes, players earning over $10 Mil play over 30 minutes

# Lets explore some offensive stats

# Plot FG = Field Goals Per Game
ggplot(nba_players,aes(FG,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()

# Plot X2P = 2-Point Field Goals Per Game
ggplot(nba_players,aes(X2P,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()

# Plot X3P = 3-Point Field Goals Per Game
ggplot(nba_players,aes(X3P,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()

# Plot FT - Free throws
ggplot(nba_players,aes(FT,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()

# Plot PTS - Points Per Game
ggplot(nba_players,aes(PTS,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()
# The more poits player scores the better he will earn

# Defensive stats TRB, AST, STL, BLK,
# Some players are better defensive players then offensive and get payed well 
# Plot TRB = Total Rebounds Per Game 
ggplot(nba_players,aes(TRB,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()

# AST = Assits Per Game
ggplot(nba_players,aes(AST,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()
# Assists also look like a good indicator.

# STL= Steals Per Game 
ggplot(nba_players,aes(STL,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()
# Steals also look like a good indicator.

# BLK = Blocks Per Game
ggplot(nba_players,aes(BLK,Salary)) + 
  geom_point(alpha=0.2) +  
  geom_smooth() + 
  theme_bw()


# plot 3 Seasons
ggplot(nba_players,aes(factor(Season),Salary)) + 
  geom_boxplot(aes(color=factor(Season))) + 
  theme_bw()
# As seasons progress the salaries are going up slighly especially for the outliers

#Played in allstar game
ggplot(nba_players,aes(factor(Play),Salary)) + 
  geom_boxplot(aes(color=factor(Play))) + 
  theme_bw()
#Although there are few outliers for those that did not play in allstar game we can see that on Average those that played in the allstar game earn more then double

# The indicators we will get rid of to avoid over fitting.
nba_players <- select(nba_players, -GS, -FG)

# What is the Average salary in the NBA
mu_salary <- mean(nba_players$Salary)
mu_salary

# Function that determines if the player is above average salary
aboveAvg <- function(x){
  if (x>mu_salary){
    return("Yes")
  }else{
    return("No")
  }
}

# Add a new column AboveAvg to the dataset
nba_players$AboveAvg <-sapply(nba_players$Salary,aboveAvg)

# Refactor the new column
nba_players$AboveAvg <- factor(nba_players$AboveAvg)

# Drop the Salary column
nba_players <- select(nba_players, -Salary)

# Check the structure of the Dataset before proceeding with the model
str(nba_players)


# Set a random seed 
set.seed(101) 


# Split data nba_players assigning split ratio to TRUE
sample_nba <- sample.split(nba_players$AboveAvg, SplitRatio = 0.70) # SplitRatio = percent of sample_nba==TRUE

# Training Data
train = subset(nba_players, sample_nba == TRUE)

# Testing Data
test = subset(nba_players, sample_nba == FALSE)

# Run Logistic Regression model
model_lgr = glm(AboveAvg ~ ., family = binomial(logit), data = train)

# Print Summary of the model
summary(model_lgr)
# We see that Pos1PG (Point Guard), Age, and MP (Minutes played are biggest factors), with Age being the bigggest

# Predict the values using the model and the test data
test$predicted.AboveAvg = predict(model_lgr, newdata=test, type="response")

# Remeber the test data was
table(test$AboveAvg)

#Lets see how well we predicted the results with confusion matrix
table(test$AboveAvg, test$predicted.AboveAvg > 0.5)

# Lets Calculate the the accuracy of our model
(233+100)/(233+100+34+37)



# Lets look at the stats with confusion matrix function
# we will first have to refactor our test$predicted.AboveAvg result so its the same factor
# Refactor function
aboveAvgTest <- function(x){
  if (x > 0.5){
    return("Yes")
  }else{
    return("No")
  }
}

test$predicted.AboveAvg <-sapply(test$predicted.AboveAvg,aboveAvgTest)
# Factor test$predicted.AboveAvg now with 2 values.
test$predicted.AboveAvg <- factor(test$predicted.AboveAvg)
str(test$predicted.AboveAvg)

# Print Confusion Matrix
confusionMatrix(data = test$predicted.AboveAvg, reference = test$AboveAvg)

#Lets see if we can improve this Model step function
model_lgr_step <- step(model_lgr)

# Print Summary
summary(model_lgr_step)
# We see our step model only kept the most important factors

# First we need to drop test$predicted.AboveAvg column from our previous interation before running new prediction
test <- select(test, -predicted.AboveAvg)

# Get new predicted AboveAvg values
test$predicted.AboveAvg = predict(model_lgr_step, newdata=test, type="response")

table(test$AboveAvg, test$predicted.AboveAvg > 0.5)

(224+96)/(224+96+43+41)
# Here we see that our accuracy actually got worst with the new step model

# Lets see if we can improve our prediction with svm model

# call the svm model
model_svm <- svm(AboveAvg ~ ., data = train)

# print summary of the model
summary(model_svm)

# Again we need to drop test$predicted.AboveAvg column from our previous interation before running new prediction
test <- select(test, -predicted.AboveAvg)

# Call predict
test$predicted.AboveAvg <- predict(model_svm,test[1:15])

table(test$predicted.AboveAvg,test$AboveAvg)

#We see that SVM model was a little better than our original logistic regression model at predicting those players with below/equal average salary, getting 239 out of 267 players right, but it did worst for those players above league average, getting only 90 out 137 right.
#Let’s Calculate the accuracy of our model to see how close they are in accuracy.


# Lets Calculate the the accuracy of our model
(239+90)/(239+90+47+28)

# We see that the accuracy of our SVM model is actually around 1% lower at 81.44% than our Logistic model which had accuracy of 82.43%.

