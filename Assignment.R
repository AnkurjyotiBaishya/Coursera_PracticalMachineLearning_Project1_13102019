#LOADING DATA
fitness.train <- read.csv(paste(getwd(),"/Raw Data/pml-training.csv", sep = ""), header = TRUE)
fitness.test <- read.csv(paste(getwd(),"/Raw Data/pml-testing.csv", sep = ""), header = TRUE)

#PACKAGES
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)

#PREPROCESSING
str(fitness.train)
class(fitness.train$classe)
levels(fitness.train$classe)
#exclude identifier, timestamp, and window data (they cannot be used for prediction)
del1 <- grep("name|timestamp|window|X", colnames(fitness.train), value=F) 
fitness.train2 <- fitness.train[,-del1]
#exclude variables with over 95% missing data (NA)
fitness.train2[fitness.train2 == ""] <- NA
NArate <- apply(fitness.train2, 2, function(x) sum(is.na(x)))/nrow(fitness.train2)
fitness.train3 <- fitness.train2[!(NArate > 0.95)]
str(fitness.train3)

#EXPLORATORY ANALYSIS
plot(training$classe)

#DATA SPLIT
set.seed(1334)
part <- createDataPartition(fitness.train3$classe, p = 0.7, list = FALSE)
training <- fitness.train3[part,]
validation <- fitness.train3[-part,]

#number of variables is still large, and some variables seem to be related to each other.
#so, PCA is done

#PCA
PCA.1 <- preProcess(training[,1:52], method = "pca", thresh = 0.95)
PCA.1
#24 components were used
PCA.2 <- preProcess(training[,1:52], method = "pca", pcaComp = 24)
PCA.2$rotation
training.PCA <- predict(PCA.2, training[,1:52])

#MODELLING
model1 <- randomForest(training$classe ~ ., data = training.PCA, do.trace = FALSE)
model1

#CROSS VALIDATION
valid.PCA <- predict(PCA.2, validation[,-53])
valid.1 <- predict(model1, valid.PCA)
confusionMatrix(validation$classe, valid.1)

#PREDICTION on TEST DATA
View(fitness.test)
del1 <- grep("name|timestamp|window|X", colnames(fitness.test), value=FALSE) 
fitness.test2 <- fitness.test[,-del1]
fitness.test2[fitness.test2 == ""] <- NA
NArate <- apply(fitness.test2, 2, function(x) sum(is.na(x)))/nrow(fitness.test2)
fitness.test3 <- fitness.test2[!(NArate > 0.95)]
testing.PCA <- predict(PCA.2, fitness.test3[,1:52])
fitness.test3$Classe <- predict(model1, testing.PCA)
fitness.test3$Classe

#APPENDIX
png("plot1.png")
plot(fitness.train3$classe, main = "Frequency of Each Class \n(activites in the test)", 
     col = "Red", xlab = "Classes", ylab = "Count")
dev.off()