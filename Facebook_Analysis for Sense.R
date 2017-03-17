#Facebook Analysis for Sense

#Importing Data
dataset_fcbk = read.csv(xxxx)

#Removing N/A values
dataset_fcbk[is.na(dataset_fcbk)] = 0

#Feature Scaling - Must always do this in SVR
dataset_fcbk[14] = scale(dataset_fcbk[14])
dataset_fcbk[17] = scale(dataset_fcbk[17])

#Adding Engagements column in Dataset
dataset_fcbk$Engagements = dataset_fcbk$Like + dataset_fcbk$Share + dataset_fcbk$Comment
dataset_fcbk[18] = scale(dataset_fcbk[18])

#Fitting SVR Regression Model to Dataset for Engagements v Word Count
install.packages("e1071")
library(e1071)
regressor = svm(Engagements ~ Word.count, 
                data = dataset_fcbk,
                type = 'eps-regression')

# Visualising the SVR Regression Model results for Engagements against Word Count
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset_fcbk$Word.count, y = dataset_fcbk$Engagements),
             colour = 'red') +
  geom_line(aes(x = dataset_fcbk$Word.count, y = predict(regressor, newdata = dataset_fcbk)),
            colour = 'blue') +
  ggtitle('Engagements against Word Count (SVR Regression Model)') +
  xlab('Word.count') +
  ylab('Engagements')

#Up-Sampling the content data to get more accurate results
table(dataset_fcbk[6])

dataset_content_Blog <- dataset_fcbk[dataset_fcbk$Content.type=="Blog",]
up <- sample(5,24,replace=T)
dataset_up_Blog <- dataset_content_Blog[up,]

dataset_content_fcbkphoto <- dataset_fcbk[dataset_fcbk$Content.type=="Facebook Photo",]
up <- sample(9,24,replace=T)
dataset_up_fcbkphoto <- dataset_content_fcbkphoto[up,]

dataset_content_JobAd <- dataset_fcbk[dataset_fcbk$Content.type=="Job Ad",]
up <- sample(4,24,replace=T)
dataset_up_JobAd <- dataset_content_JobAd[up,]

dataset_content_Story <- dataset_fcbk[dataset_fcbk$Content.type=="Story",]
up <- sample(9,24,replace=T)
dataset_up_Story <- dataset_content_Story[up,]

dataset_up_News <- dataset_fcbk[dataset_fcbk$Content.type=="News",]

dataset_fcbk_2 = rbind(dataset_up_Blog,
                       dataset_up_fcbkphoto,
                       dataset_up_JobAd,
                       dataset_up_Story,
                       dataset_up_News)

# Fitting Random Forest Regression to the dataset to find correlation with Content.Type
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor_randmfrst = randomForest(x = dataset_fcbk_2[6],
                                   y = dataset_fcbk_2$Engagements,
                                   ntree = 500)

#Need to find out how many factors in new dataset content.type
str(dataset_fcbk_2[6])

# Predicting a new result with Random Forest Regression
for (category in c(1:13)){
  +     print(predict(regressor_randmfrst, category))
}

#Up-Sampling the media data to get more accurate results
table(dataset_fcbk[11])

dataset_media_Graphic <- dataset_fcbk[dataset_fcbk$Media.type=="Graphic",]
up <- sample(12,60,replace=T)
dataset_up_Graphic <- dataset_media_Graphic[up,]

dataset_media_Video <- dataset_fcbk[dataset_fcbk$Media.type=="Video",]
up <- sample(7,60,replace=T)
dataset_up_Video <- dataset_media_Video[up,]

dataset_up_Photo <- dataset_fcbk[dataset_fcbk$Media.type=="Photo",]

dataset_fcbk_3 = rbind(dataset_up_Graphic,
                       dataset_up_Video,
                       dataset_up_Photo)

# Fitting Random Forest Regression to the dataset to find correlation with Media.Type
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor_randmfrst = randomForest(x = dataset_fcbk_3[11],
                                   y = dataset_fcbk_3$Engagements,
                                   ntree = 500)

# Predicting a new result with Random Forest Regression
for (category in c(1:4)){
  +     print(predict(regressor_randmfrst, category))
}

#Finding out sample size per content media
table(dataset_fcbk$Content.type,dataset_fcbk$Media.type)
