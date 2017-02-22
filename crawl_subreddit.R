source("util.R")

# Define subreddit
baseUrl = "https://www.reddit.com/r/worldnews/"
# Crawl threads in subreddit
subreddit.df = crawlSubredditPage(baseUrl = baseUrl, numPages = 1)
# Crawl comments of each thread
comments.df = crawlCommentsFromUrls(commentsUrls.vec = subreddit.df$commentsUrl[1:2])
# Get word counts from title and comments
wordCounts.df = getWordCounts(strings = c(as.character(comments.df$comment), 
                                          as.character(subreddit.df$title)))
