# This files declares functions to crawl different data from Reddit

#'@description Obtain information of threads from a subreddit
#'@param baseUrl: Url of subreddit e.g. https://www.reddit.com/r/worldnews/
#'@param numPages: Number of pages to crawl
#'@return Data frame where each row is info about a thread
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
    
    if(length(response$data$children) > 0) {
      for(i in 1:length(response$data$children)) {
        row = list()
        row["name"] = response$data$children[[i]]$data$name
        row["title"] = response$data$children[[i]]$data$title
        row["author"] = response$data$children[[i]]$data$author
        row["domain"] = response$data$children[[i]]$data$domain
        row["commentsUrl"] = response$data$children[[i]]$data$permalink
        # row["url"] = response$data$children[[i]]$data$url
        # row["numComments"] = response$data$children[[i]]$data$num_comments
        # row["upVotes"] = response$data$children[[i]]$data$ups
        # row["downVotes"] = response$data$children[[i]]$data$downs
        subreddit.df = rbind.fill(subreddit.df, as.data.frame(row))
      }
    }
    currentPage = currentPage + 1
  }
  
  # Remove duplicated threads
  subreddit.df = subreddit.df[!duplicated(subreddit.df$name),]
  print(paste(nrow(subreddit.df), "threads found"))
  subreddit.df
}

#'@description  Obtain comments from a list of comments url
#'@param commentsUrls.vec: Vector of comments url
#'@param numComments: limit to number of comments to crawl
#'@return Data frame of each row is info of a comment
crawlCommentsFromUrls = function(commentsUrls.vec, numComments = .Machine$integer.max) {
  # Remove duplicated urls
  commentsUrls.vec = commentsUrls.vec[!duplicated(commentsUrls.vec)]
  # Limit number of urls
  commentsUrls.vec = commentsUrls.vec[1:min(numComments, length(commentsUrls.vec))]
  print(paste("Crawling", length(commentsUrls.vec), "comment URLs"))
  # Add reddit url if needed
  redditUrl = "https://www.reddit.com/"
  commentsUrls.vec = ifelse(grepl(redditUrl, commentsUrls.vec), commentsUrls.vec, paste0(redditUrl, commentsUrls.vec))
  
  # Crawl
  tryCatch(library(RedditExtractoR), 
           error = function(e) {
             print("RedditExtractoR lib not found, use source")
             source("RedditExtractoR.R")})
  result = reddit_content(commentsUrls.vec)
  result = result[,c("user",
                     "comment"
                     # "comment_score"
  )]
  print(paste(nrow(result), "comments found"))
  result
}

#'@description  Obtain information of posts/comments from a user
#'@param username: Username of user
#'@param numPages: Number of pages to crawl
#'@return Data frame where each row is info about a post/comment
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
    
    if(length(response$data$children) > 0) {
      for(i in 1:length(response$data$children)) {
        row = list()
        row["name"] = response$data$children[[i]]$data$name
        row["body"] = response$data$children[[i]]$data$body
        row["title"] = response$data$children[[i]]$data$title
        row["subreddit"] = response$data$children[[i]]$data$subreddit
        # row["author"] = response$data$children[[i]]$data$author
        row["domain"] = response$data$children[[i]]$data$domain
        # row["commentsUrl"] = response$data$children[[i]]$data$permalink
        # row["url"] = response$data$children[[i]]$data$url
        # row["numComments"] = response$data$children[[i]]$data$num_comments
        # row["upVotes"] = response$data$children[[i]]$data$ups
        # row["downVotes"] = response$data$children[[i]]$data$downs
        user.df = rbind.fill(user.df, as.data.frame(row))
      }
    }
    currentPage = currentPage + 1
  }
  
  user.df = user.df[!duplicated(user.df$name),]
  print(paste(nrow(user.df), "threads/comments found"))
  user.df
}

#'@description 
#'Clean up words by removing nonsensical/unimportant strings and character. 
#'Then obtain count stats of each word
#'@param strings: A character or vector of characters to clean
#'@return  A character or vector of characters of cleaned words
getWordCounts <- function(strings) {
  # Split strings
  strings = unlist(strsplit(strings, " "))
  
  # Clean up strings first
  strings = tolower(gsub("[^[:alpha:] ]", "", strings))
  
  # Remove words from super ban list
  superbanlist = read.csv("superbanlist.csv", header = FALSE, as.is = TRUE)[,1]
  removeStrings = intersect(strings, superbanlist)
  if(length(removeStrings) > 0) {
    for(i in 1:length(removeStrings))
      strings = strings[strings != removeStrings[i]]
  }
  
  # Remove length > 30
  strings = strings[nchar(strings) <= 30]
  
  # Remove blank
  strings = strings[strings != ""]
  
  # Get count
  result = as.data.frame(table(strings),
                         stringsAsFactors = FALSE)
  colnames(result) = c("word", "freq")
  result
}

getSubjectFromUrl = function(url) {
  sub("/", "", 
      sub("https://www.reddit.com/r/", "", 
          sub("https://www.reddit.com/user", "", url)))
}
