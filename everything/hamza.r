
temporaryFile <- tempfile() # Create a new temp file

getDf <- function(path.name, file.name, column.types, destination.file) {
  	download.file(paste(path.name, file.name, sep=""), destfile=destination.file, method="curl")
  	read.csv(destination.file, colClasses=column.types)
}

Repository.path <- "https://raw.githubusercontent.com/miratepuffin/SinkingShipSolutionsIncorperated/master/"
train.raw <- "cheatTrain.csv?token=AGW2CwT6uIz3wbVvSLwRdd-KV01JW-E6ks5UZ9FzwA%3D%3D"
test.raw <- "test.csv?token=AGW2CyMnCpON__L56bqregRlxPFB838eks5UZ9JIwA%3D%3D"

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

df.train <- getDf(Repository.path, train.raw, train.column.types, dest=temporaryFile)
df.test <- getDf(Repository.path, test.raw, test.column.types, dest=temporaryFile)


# 	There are better ways to impute data, we will let the Hmisc library do this
df.train$Age[is.na(df.train$Age)] <- median(df.train$Age, na.rm=TRUE)
df.train
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
df.test$Survived <- round(predict(model.fit, newdata=df.test, type='response'))
# Write submission file
write.csv(df.test[,c("PassengerId", "Survived")], file="pred.csv", row.names=FALSE, quote=FALSE)
