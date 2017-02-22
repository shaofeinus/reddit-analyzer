source("util.R")

# Define subreddit
baseUrl = "https://www.reddit.com/r/worldnews/"
# Crawl threads in subreddit
subreddit.df = crawlSubredditPage(baseUrl = baseUrl, numPages = 1)
# Get word counts for each user in subreddit
wordCounts.df.list = lapply(subreddit.df$author,
                            function(username) {
                              print(paste("Crawling user", username))
                              user.df = crawlRedditUser(username = username, numPages = 2)
                              getWordCounts(strings = c(as.character(user.df$title),
                                                        as.character(user.df$body)))
                            })
names(wordCounts.df.list) = subreddit.df$author





