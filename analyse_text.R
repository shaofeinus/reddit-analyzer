source("analyse_util.R")

# Subreddit word freq counts
subreddits.wordFreqData.df = makeWordFreqDf(path = "subreddit_wordcount", 
                                            groupNames = c("group_1_subreddits", 
                                                           "group_2_subreddits", 
                                                           "group_3_subreddits"), 
                                            maxRows = 50, minFreq = 0.0001)
# User word/subreddit freq counts
users.wordFreqData.df = makeWordFreqDf(path = "user_wordcount", groupNames = c("users"), maxRows = 30, minFreq = 0.0001)
users.subredditFreqData.df = makeUserSubredditFreqDf(path = "user_data", groupNames = c("users"), maxRows = 30)

# Compute distance
subreddits.wordFreq.dist = dist(subreddits.wordFreqData.df)
users.wordFreq.dist = dist(users.wordFreqData.df)
users.subredditFreq.dist = dist(users.subredditFreqData.df)

# Plot 2D distance
plotDist2D(subreddits.wordFreq.dist, groupNames = c( "group_1_subreddits", 
                                                     "group_2_subreddits", 
                                                     "group_3_subreddits"))
plotDist2D(users.wordFreq.dist)
plotDist2D(users.subredditFreq.dist)

# Get the most related
getRelated("worldnews", top = 3, distance = subreddits.wordFreq.dist)
getRelated("coldishfellow", top = 5, distance = users.wordFreq.dist)
getRelated("coldishfellow", top = 5, distance = users.subredditFreq.dist)

# Get the least related
getRelated("worldnews", top = -3, distance = subreddits.wordFreq.dist)
getRelated("coldishfellow", top = -5, distance = users.wordFreq.dist)
getRelated("coldishfellow", top = -5, distance = users.subredditFreq.dist)