ls()

# uploading both test and train data in to R session
raw_train_data <- read.csv("train.csv", header = TRUE)
raw_test_data <- read.csv("test.csv", header = TRUE)

# Analysing the variables
str(raw_train_data)
str(raw_test_data)

names(raw_train_data)
dim(raw_train_data)
head(raw_train_data)

# Checking the no. of NAs & empty values from each columns in the data set.
colSums(is.na(raw_train_data))
# Age = 177
colSums(is.na(raw_test_data))
# Age = 86 , Fare = 1 

colSums(raw_train_data == '')
# Cabin = 687, Embarked = 2 , removed cabin & fixed Embarked
colSums(raw_test_data == '')
# Cabin = 327 , removed Cabin

# Too many cabin missing values & do not see the purpose retaining this columns. 
# Ticket number is also not much meaningful hence removing it from both test abd train
data_col.drop <- c("Ticket","Cabin")
raw_train_data <- raw_train_data[,!(names(raw_train_data) %in% data_col.drop)]
raw_test_data  <- raw_test_data[,!(names(raw_test_data) %in% data_col.drop)]

# Converting few numbers to factors.
raw_train_data$Survived <- as.factor(raw_train_data$Survived)
raw_train_data$Pclass <- as.factor(raw_train_data$Pclass)
raw_train_data$Name <- as.character(raw_train_data$Name)

raw_test_data$Pclass <- as.factor(raw_test_data$Pclass)
raw_test_data$Name <- as.character(raw_test_data$Name)

# Checking the Spread or Range of few Variables
fivenum(raw_train_data$Age)
summary(raw_train_data$Age)


# Plotting Age & Fare Frequncy 
plot(density(raw_train_data$Age,na.rm = TRUE), main = 'Age Frequency - Train Data', xlab = 'Age')
plot(density(raw_train_data$Fare,na.rm = TRUE),main = 'Fare Frequency - Train Data',xlab = 'Fare')

plot(density(raw_test_data$Age,na.rm = TRUE), main = 'Age Frequency - Test Data', xlab = 'Age')
plot(density(raw_test_data$Fare,na.rm = TRUE),main = 'Fare Frequency - Test Data',xlab = 'Fare')

# Plotting Gender Across Survival count
# It is found that more females survived when compared to the men 
Gender_Survival <- table(raw_train_data$Survived, raw_train_data$Sex)
barplot(Gender_Survival, xlab = "Gender", ylab = "Survival Count", col = c("red","blue"),
        legend.text = c("Died","Survived"), main = "Survival Count Across Gender - Train Data",        
        args.legend = list(x = "topleft"))

# Plotting Pclass Across Survival count
# Survival count is very minimal, many from Pclass = 3 died , more from 1st class survived 
# comparatively and the 2nd class passengers equally survived.
Pclass_Survival <- table(raw_train_data$Survived, raw_train_data$Pclass)
barplot(Pclass_Survival, xlab = "Gender", ylab = "Survival Count", col = c("red","blue"),
        legend.text = c("Died","Survived"), main = "Survival Count Across Passenger Class - Train Data",        
        args.legend = list(x = "topleft"))

# Plotting Embarked Across Survival count
# Can be seen there are 2 missing values in this level. hence replacing this level to most frequently
# Occurring Embarked status , ie., 'S'
Embarked_Survival <- table(raw_train_data$Survived, raw_train_data$Embarked)
barplot(Embarked_Survival, xlab = "Gender", ylab = "Survival Count", col = c("red","blue"),
        legend.text = c("Died","Survived"), main = "Survival Count Across Passenger Class - Train Data",        
        args.legend = list(x = "topleft"))

prop.table( table(raw_train_data$Survived, raw_train_data$Embarked),1)

raw_train_data$Embarked[raw_train_data$Embarked == ''] <- 'S'
# Embarked missing value is replaced, Still having issues with number of levels pointing to 4 when
# logically it shoud only be 3. hence fixing this converting data types.

raw_train_data$Embarked <- as.character(raw_train_data$Embarked)
raw_train_data$Embarked <- as.factor(raw_train_data$Embarked)

# Splitting the Name as first, last name & title, to validate if any analysis can be done.
# train_data
raw_train_data$first_name <- sapply(strsplit(as.character(raw_train_data$Name),', '), "[", 1)
raw_train_data$last_name1 <- sapply(strsplit(as.character(raw_train_data$Name),',.'), "[", 2)
raw_train_data$last_name  <- sapply(strsplit(as.character(raw_train_data$last_name1),'. '), "[", 2)
raw_train_data$title <- sapply(strsplit(as.character(raw_train_data$last_name1),'. '), "[", 1)

# test_data
raw_test_data$first_name <- sapply(strsplit(as.character(raw_test_data$Name),', '), "[", 1)
raw_test_data$last_name1 <- sapply(strsplit(as.character(raw_test_data$Name),',.'), "[", 2)
raw_test_data$last_name  <- sapply(strsplit(as.character(raw_test_data$last_name1),'. '), "[", 2)
raw_test_data$title <- sapply(strsplit(as.character(raw_test_data$last_name1),'. '), "[", 1)

# Droping unwanted columns created in the data sets
data_col.drop <- c("first_name","last_name1","last_name")
raw_train_data <- raw_train_data[,!(names(raw_train_data) %in% data_col.drop)]
raw_test_data  <- raw_test_data[,!(names(raw_test_data) %in% data_col.drop)]

#Replacing the countess level
raw_train_data$title[raw_train_data$Name == "Rothes, the Countess. of (Lucy Noel Martha Dyer-Edwards)"] <- "the Countess"

str(raw_train_data)
count(raw_train_data$title)
aggr <- aggregate(raw_train_data$Age, by = list(raw_train_data$title) , FUN = mean)
table(raw_train_data$Survived,raw_train_data$title)

str(raw_test_data)
count(raw_test_data$title)
aggregate(raw_test_data$Age, by = list(raw_test_data$title) , FUN = mean)

# grouping title of similar Ranks
raw_train_data$title[raw_train_data$title %in% c("Capt", "Don", "Major", "Sir")] <- "Sir"
raw_train_data$title[raw_train_data$title %in% c("Dona", "Lady", "the Countess")] <- "Lady"
raw_train_data$title[raw_train_data$title %in% c("Mme", "Mlle")] <- "Mlle"

raw_test_data$title[raw_test_data$title %in% c("Dona", "Lady", "the Countess", "Jonkheer")] <- "Lady"

# converting title in both test and train
raw_train_data$title <- as.factor(raw_train_data$title)
raw_test_data$title  <- as.factor(raw_test_data$title)

#Fixing Missing Age Data
#-----------------------

# Taking a count of the missing Age data
sum(is.na(raw_train_data$Age)) # 177
sum(is.na(raw_test_data$Age))  # 86

#creating a data frame for title & its mean value
mean_age_title_df <- data.frame("title_name" = character(0), "title_variable"  = character(0), "mean_age" = numeric(0), stringsAsFactors=FALSE)

#character list of the unique tile in the data frame
title.age.aggr <- aggregate(raw_train_data$Age, by = list(raw_train_data$title) , FUN = mean)
unique_title <- title.age.aggr$Group.1[is.na(title.age.aggr$x)]

# Function to store data in data frame.
mean_age_tile <- function() { 
  for (i in 1:length(unique_title)) { 
    title_name <- toString(unique_title[i])
    title_variable <- paste0("mean_", tolower(unique_title[i]), "_age")
    mean_age <- round(mean(raw_train_data$Age[raw_train_data$title == unique_title[i]], na.rm = TRUE))
    mean_age_title_df[nrow(mean_age_title_df) + 1, ] <- c(title_name, title_variable, mean_age)
  } 
  return (mean_age_title_df) 
}

mean_age_title_df <- mean_age_tile()
mean_age_title_df
[1] title          title_variable	 mean_age   

#Replacing Missing values in Age 
for (i in 1:nrow(mean_age_title_df)) {
  for (j in 1:nrow(raw_train_data))
  {if (is.na(raw_train_data[j,"Age"]))
  { if (raw_train_data[j,"title"] == mean_age_title_df[i,"title_name"])
  {raw_train_data[j,"Age"] = mean_age_title_df[i,"mean_age"]
  }}
  }}

# Checking missed values after replacing missed values
sum(is.na(raw_train_data$Age)) # 0

rm(mean_age_title_df)

# Since only one data point for "Ms" record , replacing missing age by mean of female
aggregate(raw_test_data$Age, by = list(raw_test_data$title) , FUN = mean)
count(raw_test_data$title)

mean_age_female <- mean(raw_test_data$Age[raw_test_data$Sex=="female"],na.rm = TRUE)
mean_age_female
raw_test_data$Age[raw_test_data$title == "Ms"] <- mean_age_female

#character list of the unique tile in the data frame
title.age.aggr <- aggregate(raw_test_data$Age, by = list(raw_test_data$title) , FUN = mean)
unique_title <- title.age.aggr$Group.1[is.na(title.age.aggr$x)]


# Function to store data in data frame.
mean_age_tile <- function() { 
  for (i in 1:length(unique_title)) { 
    title_name <- toString(unique_title[i])
    title_variable <- paste0("mean_", tolower(unique_title[i]), "_age")
    mean_age <- round(mean(raw_test_data$Age[raw_test_data$title == unique_title[i]], na.rm = TRUE))
    mean_age_title_df[nrow(mean_age_title_df) + 1, ] <- c(title_name, title_variable, mean_age)
  } 
  return (mean_age_title_df) 
}

mean_age_title_df <- mean_age_tile()
mean_age_title_df
[1] title          title_variable	 mean_age   

#Replacing Missing values in Age 
for (i in 1:nrow(mean_age_title_df)) {
  for (j in 1:nrow(raw_test_data))
  {if (is.na(raw_test_data[j,"Age"]))
  { if (raw_test_data[j,"title"] == mean_age_title_df[i,"title_name"])
  {raw_test_data[j,"Age"] = mean_age_title_df[i,"mean_age"]
  }}
  }}

# Checking missed values after replacing missed values
sum(is.na(raw_test_data$Age))  # 0

# Converting to int & rounding the values
raw_train_data$Age <- as.integer(raw_train_data$Age)
raw_train_data$Age <- round(raw_train_data$Age,0)

raw_test_data$Age <- as.integer(raw_test_data$Age)
raw_test_data$Age <- round(raw_test_data$Age,0)


# most frequently occurring Age before and after replacing missing values
# Median
> names(sort(table(train_stage_data$Age),decreasing = TRUE))[1]
[1] "24"
> names(sort(table(train_data$Age),decreasing = TRUE))[1]
[1] "32.3680904522613"

# Anyone travelling at zero Fare, assuming, people who travelled at zero fare, where all ship crew 
# member except "Reuchlin". All are Male and embarked at "S". No Sibsp & Parch. Travelled Alone.
# Survival % is 6.70. Very very low. Hence marking them
raw_train_data.sub <- subset(raw_train_data,raw_train_data$Fare == 0)
sum(raw_train_data.sub$Survived == 1)/ nrow(raw_train_data.sub)  * 100

raw_train_data$ZeroFare <- 0
for (j in 1:nrow(raw_train_data))
{
  if(raw_train_data[j,"Fare"] == 0)
    raw_train_data[j,"ZeroFare"] <- 1
}

# There is a datapoint with NA Fare 
sum(is.na(raw_test_data$Fare))
raw_test_data.sub <- subset(raw_test_data,is.na(raw_test_data$Fare))

# This datapoint has a similar pattern. No SibSp,Parch,Embarked @ S and is a Male. Hence assuming
# the datapoint to have travelled at zero fare. Replaced with Zero fare

raw_test_data$Fare[is.na(raw_test_data$Fare)] <- 0
raw_test_data.sub <- subset(raw_test_data,raw_test_data$Fare == 0)

raw_test_data$ZeroFare <- 0
for (j in 1:nrow(raw_test_data))
{
  if(raw_test_data[j,"Fare"] == 0)
    raw_test_data[j,"ZeroFare"] <- 1
}

# Analysing the Sibsp and Parch variables to make them useful.
SibSp_Survival <- table(raw_train_data$Survived, raw_train_data$SibSp)
barplot(SibSp_Survival, xlab = "Siblings & Spouse", ylab = "Survival Count", col = c("red","blue"),
        legend.text = c("Died","Survived"), main = "Survival Count Across Siblings & Spouse - Train Data",        
        args.legend = list(x = "topright"))

Parch_Survival <- table(raw_train_data$Survived, raw_train_data$Parch)
Parch_Survival <- prop.table(table(raw_train_data$Survived, raw_train_data$Parch))

barplot(Parch_Survival, xlab = "Siblings & Spouse", ylab = "Survival Count", col = c("red","blue"),
        legend.text = c("Died","Survived"), main = "Survival Count Across Parent & Child - Train Data",        
        args.legend = list(x = "topright"))

# Not much of a learning from these variables. 
# Hence considering that children had the highest probability to survive.

raw_train_data$Child <- 0
for (j in 1:nrow(raw_train_data))
{
  if(raw_train_data[j,"Age"] <= 8)
    raw_train_data[j,"Child"] <- 1
}

count(raw_train_data$Child)
prop.table(table(raw_train_data$Child,raw_train_data$Survived),1)
table(raw_train_data$Child,raw_train_data$Survived)

prop.table(table(raw_train_data$Child,raw_train_data$Survived))

raw_test_data$Child <- 0
for (j in 1:nrow(raw_test_data))
{
  if(raw_test_data[j,"Age"] <= 8)
    raw_test_data[j,"Child"] <- 1
}

# Assuming Mother's survival rate is more.
raw_train_data$Mother <- 0
for (j in 1:nrow(raw_train_data))
{
  if(raw_train_data[j,"Parch"] >= 1)
  {
    if(raw_train_data[j,"title"] == "Mrs" )
      raw_train_data[j,"Mother"] <- 1
  }}

prop.table(table(raw_train_data$Survived,raw_train_data$Mother),2)

# Assuming Mother's survival rate is more.
raw_test_data$Mother <- 0
for (j in 1:nrow(raw_test_data))
{
  if(raw_test_data[j,"Parch"] >= 1)
  {
    if(raw_test_data[j,"title"] == "Mrs" )
      raw_test_data[j,"Mother"] <- 1
  }}

# Identifying Family members
raw_train_data$Famly <- 1
for (j in 1:nrow(raw_train_data))
{
  if(raw_train_data[j,"Parch"] >= 1)
  {
    if(raw_train_data[j,"SibSp"] >= 1)
      raw_train_data[j,"Famly"] <- raw_train_data[j,"Parch"] + raw_train_data[j,"SibSp"] 
  }}

prop.table(table(raw_train_data$Survived,raw_train_data$Famly),2)

raw_test_data$Famly <- 1
for (j in 1:nrow(raw_test_data))
{
  if(raw_test_data[j,"Parch"] >= 1)
  {
    if(raw_test_data[j,"SibSp"] >= 1)
      raw_test_data[j,"Famly"] <- raw_test_data[j,"Parch"] + raw_test_data[j,"SibSp"]
  }}

# Checking the Fare variable. The Variable is continuous and does not seem to have much correlation
# to the survival rate. Trying to improve the correlation factor by grouping the datapoints to a 
#sense making combination.
train.logit <- glm(formula = Survived ~ Fare, data= raw_train_data, family = binomial)
plot(density(raw_train_data$Fare,raw_train_data$Survived))

summary(raw_train_data$Fare)

length(raw_train_data$Fare[raw_train_data$Fare <= 7.9104 & raw_train_data$Fare > 0])
length(raw_train_data$Fare[raw_train_data$Fare > 7.9104 & raw_train_data$Fare <= 14.4542])
length(raw_train_data$Fare[raw_train_data$Fare <= 31 & raw_train_data$Fare > 14.4542])
length(raw_train_data$Fare[raw_train_data$Fare > 31 & raw_train_data$Fare <= 512.3292])

raw_train_data$Fare.Grp <- 0
raw_train_data$Fare.Grp[raw_train_data$Fare <= 7.9104 & raw_train_data$Fare >= 0]      <- 1
raw_train_data$Fare.Grp[raw_train_data$Fare > 7.9104 & raw_train_data$Fare <= 14.4542] <- 2
raw_train_data$Fare.Grp[raw_train_data$Fare <= 31 & raw_train_data$Fare > 14.4542]     <- 3
raw_train_data$Fare.Grp[raw_train_data$Fare > 31 & raw_train_data$Fare <= 512.3292]    <- 4

count(raw_train_data$Fare.Grp)

train.logit <- glm(formula = Survived ~ Fare.Grp + Fare, data= raw_train_data, family = binomial)
summary(train.logit) <- 1100.9

train.logit <- glm(formula = Survived ~ Fare.Grp, data= raw_train_data, family = binomial)
summary(train.logit)

raw_test_data$Fare.Grp <- 0
raw_test_data$Fare.Grp[raw_test_data$Fare <= 7.9104 & raw_test_data$Fare >= 0]      <- 1
raw_test_data$Fare.Grp[raw_test_data$Fare > 7.9104 & raw_test_data$Fare <= 14.4542] <- 2
raw_test_data$Fare.Grp[raw_test_data$Fare <= 31 & raw_test_data$Fare > 14.4542]     <- 3
raw_test_data$Fare.Grp[raw_test_data$Fare > 31 & raw_test_data$Fare <= 512.3292]    <- 4

str(raw_test_data)
str(raw_train_data)

data_col.drop <- c("Name","SibSp","Parch","Parch2","Survived")
raw_train_data <- raw_train_data[,!(names(raw_train_data) %in% data_col.drop)]
raw_test_data  <- raw_test_data[,!(names(raw_test_data) %in% data_col.drop)]

# Factoring the Family size as big and small
raw_train_data$FamlySize <- 'Large'
raw_train_data$FamlySize <- as.character(raw_train_data$FamlySize)
raw_train_data$FamlySize[raw_train_data$Famly <= 3] <- 'Small'
raw_train_data$FamlySize <- factor(raw_train_data$FamlySize)

raw_test_data$FamlySize <- 'Large'
raw_test_data$FamlySize <- as.character(raw_test_data$FamlySize)
raw_test_data$FamlySize[raw_test_data$Famly <= 3] <- 'Small'
raw_test_data$FamlySize <- factor(raw_test_data$FamlySize)

# ---
raw_test_data$Child <- as.factor(raw_test_data$Child)
raw_test_data$ZeroFare <- as.factor(raw_test_data$ZeroFare)
raw_test_data$Mother <- as.factor(raw_test_data$Mother)
raw_test_data$Fare.Grp <- as.factor(raw_test_data$Fare.Grp)
raw_test_data$Survived <- as.factor(raw_test_data$Survived)

raw_train_data$Child <- as.factor(raw_train_data$Child)
raw_train_data$ZeroFare <- as.factor(raw_train_data$ZeroFare)
raw_train_data$Mother <- as.factor(raw_train_data$Mother)
raw_train_data$Fare.Grp <- as.factor(raw_train_data$Fare.Grp)

###################################################################################################
###################################################################################################

# Building the Model
raw_test_data$Survived <- 0
train.logit <- glm(formula = Survived ~ Pclass + Sex + Age + Fare + Embarked + Famly + ZeroFare + 
                     Child + Mother + Fare.Grp , data= raw_train_data, family = binomial(link = 'logit'))

p.hats <- predict.glm(train.logit, newdata = raw_test_data, type = "response")

summary(train.logit)

survival <- vector()

for(i in 1:length(p.hats)) {
  if(p.hats[i] > 0.5) 
    survival[i] <- 1
  else 
    survival[i] <- 0
}

result.glm <- data.frame(PassengerId = test_data$PassengerId, Survived = survival)

write.csv(result.glm, file = "result.csv", row.names = FALSE)

cnt <- 0
for (i in 1:nrow(test))
{
  if(survival[i] == test$Survived[i])
    cnt <- cnt + 1
}
accuracy <- cnt/nrow(test) * 100
accuracy

###################################################################################################

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + title + Famly + ZeroFare +
               Child + Mother + Fare.Grp , data=raw_train_data, method="class")
printcp(fit)	

fit <- randomForest(Survived ~ Pclass + Sex + Age + Fare + Embarked + title + Famly + ZeroFare +
                      Child + Mother + Fare.Grp , data=train, importance=TRUE, ntree=2000)
fit
summary(fit)
raw_test_data$Survived<-predict(fit,raw_test_data)

cor(raw_train_data$Pclass,raw_train_data$Age)

summary(train.logit)
fancyRpartPlot(fit)
varImpPlot(fit)

# 
total <- rbind(raw_train_data,raw_test_data)

tail(total) 

train <- total[1:891,]
test <- total[892:1309,]

####################################################################################################

install.packages('party')
library(party)

#Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + title + Child:Mother + Embarked + 
                 Pclass:Sex + Pclass:Age + Age:Sex + Fare.Grp,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
result.glm <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(result.glm, file = "result.csv", row.names = FALSE)

####################################################################################################

# Best fit model 

model <- randomForest(Survived ~ Pclass + Sex + Age + title + Child:Mother + Embarked + 
                        Pclass:Sex + Pclass:Age + Age:Sex + Fare.Grp + Famly , data=train, ntree=20000)
# Accuracy -> .79904

model <- randomForest(Survived ~ Pclass + Sex + Age + title + Child:Mother + Embarked + 
                        Pclass:Sex + Pclass:Age + Age:Sex + Fare.Grp + FamlySize + Family
                        , data=train, ntree=20000)  # .79406

model <- randomForest(Survived ~ Pclass + Sex + Age + title + Child:Mother + Embarked + 
                        Pclass:Sex + Pclass:Age + Age:Sex + Fare.Grp + FamlySize + ZeroFare
                      , data=train, ntree=20000)  # .80383

model <- randomForest(Survived ~ Pclass + Sex + Age + title + Child:Mother + Embarked + 
                        Pclass:Sex + Pclass:Age + Age:Sex + Fare.Grp + Famly: FamlySize + ZeroFare
                      , data=train, ntree=20000)  # .79406

model <- randomForest(Survived ~ Pclass + Sex + Age + title + Child:Mother + Embarked + 
                        Pclass:Sex + Pclass:Age + Age:Sex + Fare.Grp , data=train, ntree=20000)
# Accuracy -> .80861

rm(Prediction)

Prediction <- predict(model, newdata=test, type= 'class')

result.glm <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(result.glm, file = "result.csv", row.names = FALSE)

