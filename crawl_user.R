source("crawl_util.R")

findUsers = function(subredditUrl, numUsers = 100) {
  print(paste("Finding users from subreddit:", getSubjectFromUrl(subredditUrl)))
  MAX_ITER = 5
  iter = 0
  numUsersFound = 0
  # Give some slack for duplicated users
  numPagesToFind = ceiling(numUsers / 25) * 2
  while(numUsersFound < numUsers && iter < MAX_ITER) {
    print(paste("Looking into", numPagesToFind, "pages"))
    subreddit.df = crawlSubredditPage(baseUrl = subredditUrl, numPages = numPagesToFind)
    user.vec = subreddit.df$author[!duplicated(subreddit.df$author)]
    numUsersFound = length(user.vec)
    numPagesToFind = numPagesToFind * 2
    iter = iter + 1
  }
  as.character(user.vec[1:numUsers])
}

getUserWordcount = function(user,  numPages = 5) {
  user.df = crawlRedditUser(username = user, numPages = numPages)
  if(nrow(user.df) == 0) 
    return (data.frame(word = c(), freq = c()))
  getWordCounts(strings = c(as.character(user.df$title),
                            as.character(user.df$body)))
}

saveUserWordcount = function(user, wordcount.df = NULL, saveFolder = "user_wordcount", ...) {
  # Crawl data if not crawled
  if(is.null(wordcount.df))
    wordcount.df = getUserWordcount(user, ...)
  # Save
  saveRDS(wordcount.df, file = paste0(saveFolder, "/", user, ".rds"))
}

getAndSaveAllUsersWordcount = function(user.vec, 
                                       groupName = "group", 
                                       saveFolder = "user_wordcount", ...) {
  # Get word counts
  for(user in user.vec) {
    # Download user if does not already exists
    if(file.exists(paste0(saveFolder, "/", user, ".rds"))) {
      print(paste("User already downloaded:", user))
    } else {
      print(paste("Downloading user:", user))
      wordcount.df = getUserWordcount(user, ...)
      saveUserWordcount(user, wordcount.df)
    }
  }
}

getAndSaveAllUsersData = function(user.vec, 
                                  saveFolder = "user_data", ...) {
  for(user in user.vec) {
    # Download user if does not already exists
    if(file.exists(paste0(saveFolder, "/", user, ".rds"))) {
      print(paste("User already downloaded:", user))
    } else {
      print(paste("Downloading user:", user))
      user.df = crawlRedditUser(user, ...)
      saveRDS(user.df, paste0(saveFolder, "/", user, ".rds"))
    }
  }
}

#------------------ Program starts here ------------------#
# Get the list of users from subreddit
if(file.exists("users.rds")) {
  print("Users loaded from saved file")
  users = readRDS("users.rds")
} else {
  print("Finding users")
  subreddits = c(read.csv("subreddits_1.csv", as.is = TRUE, header = FALSE)[,1], 
                 read.csv("subreddits_2.csv", as.is = TRUE, header = FALSE)[,1],
                 read.csv("subreddits_3.csv", as.is = TRUE, header = FALSE)[,1])
  users = unlist(lapply(subreddits, findUsers, numUsers = 100))
  users = users[!duplicated(users)]
  saveRDS(users, file = paste0("users.rds"))
}

# Get and save user word count
getAndSaveAllUsersWordcount(users, numPages = 10)
getAndSaveAllUsersData(users, numPages = 10)
