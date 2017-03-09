#Twitter Analysis for Sense

#Importing Data - Imported data without the actual words for now
dataset = read.csv('Sense_Sample_data_2016_Twitter.csv')

#Don't need a few columns as not relevant for correlations
dataset$Link = NULL
dataset$Engagement.rate = NULL
dataset$Reply = NULL
dataset$Media.details = NULL

#Feature Scaling - Must always do this in SVR
dataset[12:14] = scale(dataset[12:14])

#Fitting SVR Regression Model to Dataset for Engagements v Word Count
install.packages("e1071")
library(e1071)
regressor = svm(Engagements ~ Word.count, 
                data = dataset,
                type = 'eps-regression')

# Visualising the SVR Regression Model results for Engagements against Word Count
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Word.count, y = dataset$Engagements),
             colour = 'red') +
  geom_line(aes(x = dataset$Word.count, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Engagements against Word Count (SVR Regression Model)') +
  xlab('Word.count') +
  ylab('Engagements')

# Visualising whether media is more engaging or not
ggplot() +
  geom_point(aes(x = dataset$New.media, y = dataset$Engagements),
             colour = 'red') +
  ggtitle('Does Media links make a difference?') +
  xlab('Was there media in the post?') +
  ylab('Engagements')

#Up-Sampling the content data to get more accurate results
table(dataset_up1[6])

dataset_content_blog <- dataset[dataset$Content.type=="Blog",]
up <- sample(23,121,replace=T)
dataset_up_blog <- dataset_content_blog[up,]

dataset_content_Campaign <- dataset[dataset$Content.type=="Campaign",]
up <- sample(16,121,replace=T)
dataset_up_Campaign <- dataset_content_Campaign[up,]

dataset_content_Course <- dataset[dataset$Content.type=="Course",]
up <- sample(2,121,replace=T)
dataset_up_Course <- dataset_content_Course[up,]

dataset_content_Event <- dataset[dataset$Content.type=="Event",]
up <- sample(33,121,replace=T)
dataset_up_Event <- dataset_content_Event[up,]

dataset_content_Fundraising <- dataset[dataset$Content.type=="Fundraising",]
up <- sample(111,121,replace=T)
dataset_up_Fundraising <- dataset_content_Fundraising[up,]

dataset_content_Information <- dataset[dataset$Content.type=="Information",]
up <- sample(111,121,replace=T)
dataset_up_Information <- dataset_content_Information[up,]

dataset_content_Information <- dataset[dataset$Content.type=="Information",]
up <- sample(56,121,replace=T)
dataset_up_Information <- dataset_content_Information[up,]

dataset_content_JobAd <- dataset[dataset$Content.type=="Job Ad",]
up <- sample(24,121,replace=T)
dataset_up_JobAd <- dataset_content_JobAd[up,]

dataset_content_News <- dataset[dataset$Content.type=="News",]
up <- sample(59,121,replace=T)
dataset_up_News <- dataset_content_News[up,]

dataset_content_Promo <- dataset[dataset$Content.type=="Promo",]
up <- sample(2,121,replace=T)
dataset_up_Promo <- dataset_content_Promo[up,]

dataset_content_Report <- dataset[dataset$Content.type=="Report",]
up <- sample(9,121,replace=T)
dataset_up_Report <- dataset_content_Report[up,]

dataset_content_Service <- dataset[dataset$Content.type=="Service",]
up <- sample(40,121,replace=T)
dataset_up_Service <- dataset_content_Service[up,]

dataset_content_Shop <- dataset[dataset$Content.type=="Shop",]
up <- sample(1,121,replace=T)
dataset_up_Shop <- dataset_content_Shop[up,]

dataset_content_Social <- dataset[dataset$Content.type=="Social",]
up <- sample(48,121,replace=T)
dataset_up_Social <- dataset_content_Social[up,]

dataset_content_Story <- dataset[dataset$Content.type=="Story",]
up <- sample(1,121,replace=T)
dataset_up_Story <- dataset_content_Story[up,]

dataset_content_Trading <- dataset[dataset$Content.type=="Trading",]
up <- sample(10,121,replace=T)
dataset_up_Trading <- dataset_content_Trading[up,]

#Recombining dataset removing really small samples such as Course, Promo, Resource, Shop, Story
dataset1 = rbind(dataset_up_blog,
                dataset_up_Campaign,
                dataset_up_Event,
                dataset_up_Fundraising,
                dataset_up_Information,
                dataset_up_JobAd,
                dataset_up_News,
                dataset_up_Report,
                dataset_up_Service,
                dataset_up_Social,
                dataset_up_Trading)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor_randmfrst = randomForest(x = dataset1[6],
                         y = dataset1$Engagements,
                         ntree = 500)

# Predicting a new result with Random Forest Regression
for (category in c(1:17)){
  +     print(predict(regressor_randmfrst, category))
}

#Up-Sampling the media data to get more accurate results
table(dataset[10])

dataset_media_Emoticon <- dataset[dataset$Media.type=="Emoticon",]
up <- sample(19,200,replace=T)
dataset_up_Emoticon <- dataset_media_Emoticon[up,]

dataset_media_Graphic <- dataset[dataset$Media.type=="Graphic",]
up <- sample(19,200,replace=T)
dataset_up_Graphic <- dataset_media_Graphic[up,]

dataset_media_Video <- dataset[dataset$Media.type=="Video",]
up <- sample(15,200,replace=T)
dataset_up_Video <- dataset_media_Video[up,]

dataset_media_Photo <- dataset[dataset$Media.type=="Photo",]

dataset2 = rbind(dataset_up_Emoticon,
                 dataset_up_Graphic,
                 dataset_up_Video,
                 dataset_media_Photo)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor_randmfrst_media = randomForest(x = dataset2[10],
                                   y = dataset2$Engagements,
                                   ntree = 500)

# Predicting a new result with Random Forest Regression
for (category in c(1:6)){
  +     print(predict(regressor_randmfrst_media, category))
}

