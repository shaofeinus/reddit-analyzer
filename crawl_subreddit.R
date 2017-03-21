source("crawl_util.R")

getSubredditWordcount = function(subredditUrl, numPages = 25, numCommentsPerSubreddit = .Machine$integer.max) {
  # Crawl threads in subreddit
  subreddit.df = crawlSubredditPage(baseUrl = subredditUrl, numPages = numPages)
  # Crawl comments of each thread
  comments.df = crawlCommentsFromUrls(commentsUrls.vec = subreddit.df$commentsUrl, 
                                      numComments = numCommentsPerSubreddit)
  
  # Get word counts from title and comments
  wordCount.df = getWordCounts(strings = c(as.character(comments.df$comment), 
                                            as.character(subreddit.df$title)))
  wordCount.df
}

#'@description 
#'Save @param wordcount.df into file of name specified by subject in @param subredditUrl
#'If @param wordcount.df not supplied, then download them
saveSubredditWordcount = function(subredditUrl, wordcount.df = NULL, saveFolder = "subreddit_wordcount", ...) {
  # Crawl data if not crawled
  if(is.null(wordcount.df))
    wordcount.df = getSubredditWordcount(subredditUrl, ...)
  # Get name of subreddit by replacing base url and / symbol
  subredditName = getSubjectFromUrl(subredditUrl)
  # Save
  saveRDS(wordcount.df, file = paste0(saveFolder, "/", subredditName, ".rds"))
}

#'@description 
#'Get word counts from all subreddits in @param subredditUrl.vec.
#'Save the word count for each subreddit in a list of URL
getAndSaveAllSubredditsWordcount = function(subredditUrl.vec, 
                                            groupName = "group", 
                                            saveFolder = "subreddit_wordcount", ...) {
  # Get word counts
  for(subredditUrl in subredditUrl.vec) {
    # Download subreddit if does not already exists
    if(file.exists(paste0(saveFolder, "/", getSubjectFromUrl(subredditUrl), ".rds"))) {
      print(paste("Subreddit already downloaded:", getSubjectFromUrl(subredditUrl)))
    } else {
      print(paste("Downloading subreddit:", getSubjectFromUrl(subredditUrl)))
      wordcount.df = getSubredditWordcount(subredditUrl, ...)
      saveSubredditWordcount(subredditUrl, wordcount.df)
    }
  }

  # Save subreddit names for this group
  saveRDS(getSubjectFromUrl(subredditUrl.vec), file = paste0(groupName, "_subreddits.rds"))
}

getAndSaveAllSubredditsData = function(subredditUrl.vec, 
                                   saveFolder = "subreddit_data", ...) {
  # Get word counts
  for(subredditUrl in subredditUrl.vec) {
    # Download subreddit if does not already exists
    if(file.exists(paste0(saveFolder, "/", getSubjectFromUrl(subredditUrl), ".rds"))) {
      print(paste("Subreddit already downloaded:", getSubjectFromUrl(subredditUrl)))
    } else {
      print(paste("Downloading subreddit:", getSubjectFromUrl(subredditUrl)))
      subreddit.df = crawlSubredditPage(baseUrl = subredditUrl, ...)
      saveRDS(subreddit.df, file = paste0(saveFolder, "/", getSubjectFromUrl(subredditUrl), ".rds"))
    }
  }
}

#------------------ Program starts here ------------------#
subreddits_relevance1 = read.csv("subreddits_1.csv", as.is = TRUE, header = FALSE)[,1]
subreddits_relevance2 = read.csv("subreddits_2.csv", as.is = TRUE, header = FALSE)[,1]
subreddits_relevance3 = read.csv("subreddits_3.csv", as.is = TRUE, header = FALSE)[,1]

getAndSaveAllSubredditsWordcount(subreddits_relevance1, groupName = "group_1", numPages = 25)
getAndSaveAllSubredditsWordcount(subreddits_relevance2, groupName = "group_2", numPages = 25)
getAndSaveAllSubredditsWordcount(subreddits_relevance3, groupName = "group_3", numPages = 25)

getAndSaveAllSubredditsData(subreddits_relevance1, numPages = 25)
getAndSaveAllSubredditsData(subreddits_relevance2, numPages = 25)
getAndSaveAllSubredditsData(subreddits_relevance3, numPages = 25)
