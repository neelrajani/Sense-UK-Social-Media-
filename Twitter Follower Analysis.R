#Finding most popular Twitter followers

#Installing TwitteR package
install.packages("twitteR")
library(twitteR)
consumer_key <- xxxx
consumer_secret <- 	xxxx
access_token <- 	xxxx
access_secret <- 	xxxx
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Getting List of Followers
User = getUser('andigital')
Followers = User$getFollowers

# create a data frame with 4 columns and no rows initially
df_result <- data.frame(t(rep(NA, 4)))
names(df_result) <- c('id', 'name', 's_name', 'fol_count')
df_result <- df_result[0:0,]

# you can replace this vector with whatever set of Twitter users you want
users <- c("sensecharity")

# iterate over the vector of users and aggregate each user's results
sapply(users, function(x) {
  user <- getUser(x)
  followers <- user$getFollowers()
  if (length(followers) > 0) {        # ignore users with no followers
    b <- twListToDF(followers)
    f_count <- as.data.frame(b$followersCount)
    u_id <- as.data.frame(b$id)
    u_sname <- as.data.frame(b$screenName)
    u_name <- as.data.frame(b$name)
    final_df <- cbind(u_id,u_name,u_sname,f_count)
    sort_fc <- final_df[order(-f_count),]
    colnames(sort_fc) <- c('id','name','s_name','fol_count')
    df_result <<- rbind(df_result, sort_fc)
  }
})
