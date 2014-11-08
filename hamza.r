
temporaryFile <- tempfile() # Create a new temp file

getDf <- function(path.name) {
  	read.csv(path.name)
}



train.column.types <- c(
	'integer',   # PassengerId
    'factor',    # Survived
    'factor',    # Pclass
    'character', # Name
    'factor',    # Sex
    'numeric',   # Age
    'integer',   # SibSp
    'integer',   # Parch
    'character', # Ticket
    'numeric',   # Fare
    'character', # Cabin
    'factor'     # Embarked
)

test.column.types <- train.column.types[-2]     # no Survived column in test.csv

df.train <- read.csv("cheatTrain.csv")
df.test <- read.csv("test.csv")


# 	There are better ways to impute data, we will let the Hmisc library do this
df.train$Age[is.na(df.train$Age)] <- median(df.train$Age, na.rm=TRUE)

cabin_to_deck <- function(data) {
	data = as.character(data)
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		data[i] <- substr(data[i], 1, 1)
	}
	return (as.factor(data))
}


#	summary(df.train$Embarked)
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

df.test$Embarked[which(is.na(df.test$Embarked))] <- 'S'

rm(temporaryFile) # remove from environment

#search() # what's in our environment
#ls(train) # what's in the global environment

#set.seed(44)


#	target <- which(names(df.test) == 'PassengerId')[1]
#	survived.column <- data.frame(Survived = matrix("", nrow = nrow(df.test), ncol = 1))
#	df.test <- cbind(df.test[,1:target, drop=F], survived.column, df.test[,(target+1):length(df.test),drop=F])
#	df.test$Survived <- as.factor(df.test$Survived)
#	for(index in 1:ncol(df.train)) {
#		print(typeof(df.train[, index]))
#		print(typeof(df.test[, index]))
#	}

#	names(df.train)
#	names(df.test)
#	sapply(df.train, 'class')
#	sapply(df.test, 'class')

model.fit = glm(Survived ~ Pclass + Sex + Age + Cabin + SibSp + Parch + Embarked + Pclass:Sex + Pclass:Age + Sex:Age + Sex:SibSp + Sex:Parch + Sex:Cabin + Age:Cabin, data = df.train, family = binomial)
#	model.fit = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Pclass:Sex + Pclass:Age + Sex:Age + Sex:SibSp + Sex:Parch, data=df.train, ntree=2000)
model.prediction = predict(model.fit, newdata=df.test, type='response')
#	model.prediction = predict(model.fit, newdata=df.test, type="class")

#df.test$Survived <- model.prediction

#	model.fit = randomForest(
#		Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked,
#		data=df.train,
#		ntree=2002,
#		mtry=2,
#		replace=FALSE,
#		importance=TRUE,
#		proximity=TRUE,
#		# we should have 0 na's so die loudly if we find any
#		na.action=na.fail
#	)

#	df.test$Survived <- model.prediction
df.test$Survived <- round(model.prediction)

# Write submission file
write.csv(df.test[,c("PassengerId", "Survived")], file="pred.csv", row.names=FALSE, quote=FALSE)
