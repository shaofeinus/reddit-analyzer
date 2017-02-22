# This files declares functions to crawl different data from Reddit

# Obtain information of threads from a subreddit
# @param baseUrl: Url of subreddit e.g. https://www.reddit.com/r/worldnews/
# @param numPages: Number of pages to crawl
# @return Data frame where each row is info about a thread
crawlSubredditPage = function(baseUrl, numPages) {
  
  baseUrl = paste0(baseUrl, ".json")
  
  library("httr")
  library("plyr")
  
  subreddit.df = data.frame()
  currentPage = 1
  
  while(currentPage <= numPages &
        ifelse(currentPage == 1, TRUE, !is.null(response$data$after))) {
    
    url = ifelse(currentPage == 1, baseUrl,  
                 paste0(baseUrl, 
                        "?after=", 
                        response$data$after))
    request <- GET(url,
                   user_agent("dsc4217-contentsuggestor/0.1"))
    response = content(request, "parsed", "application/json")
    
    for(i in 1:length(response$data$children)) {
      row = list()
      row["name"] = response$data$children[[i]]$data$name
      row["title"] = response$data$children[[i]]$data$title
      row["author"] = response$data$children[[i]]$data$author
      # row["domain"] = response$data$children[[i]]$data$domain
      row["commentsUrl"] = response$data$children[[i]]$data$permalink
      # row["url"] = response$data$children[[i]]$data$url
      # row["numComments"] = response$data$children[[i]]$data$num_comments
      # row["upVotes"] = response$data$children[[i]]$data$ups
      # row["downVotes"] = response$data$children[[i]]$data$downs
      subreddit.df = rbind.fill(subreddit.df, as.data.frame(row))
    }
    currentPage = currentPage + 1
  }
  
  # Remove duplicated threads
  subreddit.df[!duplicated(subreddit.df$name),]
}

# Obtain comments from a list of comments url
# @param commentsUrls.vec: Vector of comments url
# @return Data frame of each row is info of a comment
crawlCommentsFromUrls = function(commentsUrls.vec) {
  # Remove duplicated urls
  commentsUrls.vec = commentsUrls.vec[!duplicated(commentsUrls.vec)]
  # Add base url if needed
  redditUrl = "https://www.reddit.com/"
  commentsUrls.vec = ifelse(grepl(baseUrl, commentsUrls.vec), commentsUrls.vec, paste0(redditUrl, commentsUrls.vec))
  # Crawl
  library(RedditExtractoR)
  result = reddit_content(commentsUrls.vec)
  result[,c("user",
            "comment"
            # "comment_score"
            )]
}

# Obtain information of posts/comments from a user
# @param username: Username of user
# @param numPages: Number of pages to crawl
# @return Data frame where each row is info about a post/comment
crawlRedditUser = function(username, numPages) {
  
  library("httr")
  library("plyr")
  
  baseUrl = "https://www.reddit.com/user/"
  user.df = data.frame()
  currentPage = 1
  
  while(currentPage <= numPages & 
        ifelse(currentPage == 1, TRUE, !is.null(response$data$after))) {
    
    url = ifelse(currentPage == 1, paste0(baseUrl, username, ".json"),  
                 paste0(baseUrl, 
                        username,
                        ".json",
                        "?after=", 
                        response$data$after))
    request <- GET(url,
                   user_agent("dsc4217-contentsuggestor/0.1"))
    response = content(request, "parsed", "application/json")
    
    for(i in 1:length(response$data$children)) {
      row = list()
      row["name"] = response$data$children[[i]]$data$name
      row["body"] = response$data$children[[i]]$data$body
      row["title"] = response$data$children[[i]]$data$title
      # row["author"] = response$data$children[[i]]$data$author
      # row["domain"] = response$data$children[[i]]$data$domain
      # row["commentsUrl"] = response$data$children[[i]]$data$permalink
      # row["url"] = response$data$children[[i]]$data$url
      # row["numComments"] = response$data$children[[i]]$data$num_comments
      # row["upVotes"] = response$data$children[[i]]$data$ups
      # row["downVotes"] = response$data$children[[i]]$data$downs
      user.df = rbind.fill(user.df, as.data.frame(row))
    }
    currentPage = currentPage + 1
  }
  
  user.df[!duplicated(user.df$name),]
}

# Clean up words by removing nonsensical/unimportant strings and characters
# Then obtain count stats of each word
# @param strings: A character or vector of characters to clean
# @return  A character or vector of characters of cleaned words
getWordCounts <- function(strings) {
  # Clean up strings first
  strings = tolower(gsub("[^[:alpha:] ]", "", strings))
  
  # TODO: Remove:
  #   Ban list
  #   Length > 30
  #   Adjectives, pronouns -> anything that is not a noun
  
  # Get count
  as.data.frame(table(unlist(sapply(strings,
                                    function(string) {
                                      if(is.na(string)) c()
                                      strsplit(string, " ")
                                    }))))
}
