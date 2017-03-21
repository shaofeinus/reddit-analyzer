source("analyse_util.R")

# Subreddit word freq counts
wordFreqData.df = makeWordFreqDf(path = "subreddit_wordcount", 
                              groupNames = c("group_1_subreddits", 
                                             "group_2_subreddits", 
                                             "group_3_subreddits"), 
                              maxRows = 50, minFreq = 0.0001)
# User word/subreddit freq counts
wordFreqData.df = makeWordFreqDf(path = "user_wordcount", maxRows = 20, minFreq = 0.0001)
usersSubredditFreqData.df = makeUserSubredditFreqDf(path = "user_wordcount", groupNames = c("users"), maxRows = 50)

# Compute distance
d = dist(wordFreqData.df)
d = dist(usersSubredditFreqData.df)

# Plot 2D distance
plotDist2D(d, groupNames = c( "group_1_subreddits", 
                              "group_2_subreddits", 
                              "group_3_subreddits"))
