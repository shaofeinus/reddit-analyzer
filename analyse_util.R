library("tools")
library("ggplot2")
library("plyr")

#'@description Prepares subreddit/user word count data for dsitance calculation
#'@param path Directory where data is stored
#'@param groupNames A list of names of groups of subreddits/users. If not specified, process all data from directory until @param maxRows
#'@param maxRows Max number of subreddits/users to process. If not specified, process all data from directory
#'@param minFreq Minimum frequency probability of word before eleminating the word
#'@return A matrixes where rows are subreddits/users, columns are words and cell contain word freq probabilities
makeWordFreqDf <- function(path, groupNames = NULL, maxRows = .Machine$double.xmax, minFreq = 0.0001) {
  
  # Check if path exist first
  if(!dir.exists(path)) {
    print("Path specified does not exist. Nothing is done.")
    return (NULL)
  }
  
  # Prepare data used
  # If groups is not specified, consider all .rds file in the path
  if(is.null(groupNames)) {
    # Find all .rds files in path
    groups = list.files(path = path, pattern = "\\.rds$")
    groups = list(sub("\\.rds$", "", groups))
  } else {
    groups <- lapply(groupNames, 
                     function(fgName) {
                       readRDS(paste0(fgName, ".rds"))
                     })
  }
  # For each fileGroup, only process maxRows files
  groups = lapply(groups, maxRows = maxRows, 
                  function (fg, maxRows) {fg[1:min(maxRows, length(fg))]})
  
  # Make matrix
  data.df <- data.frame(word=c(NA))
  names = c()
  for (i in unlist(groups)) {
    temp.df <- readRDS(paste0(path, "/", i,".rds"))
    if(nrow(temp.df) < 250)  # Do no use data with too few words
      next
    names = c(names, i)
    temp.df$freq <- temp.df$freq/sum(temp.df$freq)
    temp.df <- temp.df[order(temp.df$freq,decreasing = TRUE),]
    temp.df <- temp.df[temp.df$freq>minFreq,]
    data.df <- merge(data.df,temp.df,by="word",all=TRUE)
  }
  data.df[is.na(data.df)] <- 0
  colnames(data.df) <- c("word",names)
  words <- data.df$word
  data.df <- t(data.df[,-1])
  colnames(data.df) <- words
  data.df
}

#'@description Prepares user subreddit data for dsitance calculation
#'@param path Directory where data is stored
#'@param groupNames A list of names of groups of user. If not specified, process all data from directory until @param maxRows
#'@param maxRows Max number of subreddits/users to process. If not specified, process all data from directory
#'@param minFreq Minimum frequency probability of word before eleminating the word
#'@return A matrixes where rows are subreddits/users, columns are words and cell contain word freq probabilities
makeUserSubredditFreqDf <- function(path, groupNames = NULL, maxRows = .Machine$double.xmax, minFreq = 0.1, normalizeCount = F) {
  # Check if path exist first
  if(!dir.exists(path)) {
    print("Path specified does not exist. Nothing is done.")
    return (NULL)
  }
  
  # Prepare data used
  # If groups is not specified, consider all .rds file in the path
  if(is.null(groupNames)) {
    # Find all .rds files in path
    groups = list.files(path = path, pattern = "\\.rds$")
    groups = list(sub("\\.rds$", "", groups))
  } else {
    groups <- lapply(groupNames, 
                     function(fgName) {
                       readRDS(paste0(fgName, ".rds"))
                     })
  }
  # For each fileGroup, only process maxRows files
  groups = lapply(groups, maxRows = maxRows, 
                  function (fg, maxRows) {fg[1:min(maxRows, length(fg))]})
  
  # Make matrix
  users.df.list = lapply(unlist(groups), 
                         function(user) {
                           user.df = readRDS(paste0(path, "/", user, ".rds"))
                           subredditCounts = table(user.df$subreddit)
                           df = data.frame(subredditCounts)
                           if(ncol(df) == 1) return (NULL)
                           colnames(df) = c("subreddit", "freq")
                           if(normalizeCount)
                             df$freq <- df$freq/sum(df$freq)
                           df <- df[df$freq>minFreq,]
                           df
                         })
  
  validDf.vec = !sapply(users.df.list, is.null)
  users.df.list = users.df.list[validDf.vec]
  users.list = unlist(groups)[validDf.vec]
  
  usersSubredditFreq.df = Reduce(function(x, y) {merge(x, y, by = "subreddit", all = T)}, users.df.list)
  usersSubredditFreq.df[is.na(usersSubredditFreq.df)] <- 0
  colnames(usersSubredditFreq.df) = c("subreddit", users.list)
  subreddits <- usersSubredditFreq.df$subreddit
  usersSubredditFreq.df <- t(usersSubredditFreq.df[,-1])
  colnames(usersSubredditFreq.df) <- subreddits
  
  usersSubredditFreq.df
}

#'@description Plot the 2D distance between subreddits/users as a scatter plot
#'@param distance Distance matrix returned by dist().
#'@param groupNames A list of names of groups of subreddits/users. Used to separate the points in the plot by groups.
#'@return A ggplot class of scatter plot
plotDist2D <- function(distance, groupNames = NULL) {
  
  fit <- cmdscale(distance, eig = T, k = 2)
  x = fit$points[,1]
  y = fit$points[,2]
  
  plotData.df = data.frame(name = row.names(fit$points), x = x, y = y)
  if(is.null(groupNames)) {
    plotData.df$group = "data"
  } else {
    groups <- lapply(groupNames, 
                     function(fgName) {
                       readRDS(paste0(fgName, ".rds"))
                     })
    plotData.df$group = sapply(plotData.df$name, 
                               groups = groups,
                               groupNames = groupNames,
                               function(name, groups, groupNames) {
                                 select = sapply(groups, 
                                                 name = name,
                                                 function(fg, name) {
                                                   name %in% fg
                                                 })
                                 ifelse(sum(select) == 0, "data",
                                        groupNames[select])
                               })
  }
  
  xlims = c(min(plotData.df$x), max(plotData.df$x))
  ylims = c(min(plotData.df$y), max(plotData.df$y))
  ggplot(data = plotData.df, 
         mapping = aes(x = x, y = y, color = group)) + 
    coord_cartesian(xlim = xlims*1.1, ylim = ylims*1.1) +
    geom_point() + 
    geom_text(aes(label = name), hjust=-0.1, vjust=1)
}

#'@description Get the most related entity based on distance vector
#'@param subreddit The name of the entity to find the related entities for
#'@param top The number of most related entities to return
#'@param distance Distance matrix, must contain entity which the function is finding related entities for
#'@return Vector of most related entities names, ordered by descending order of relatedness
getRelated = function(name, top, distance) {
  d.mat = as.matrix(distance)
  if(abs(top) > nrow(d.mat) - 1) {
    print("top variable too large, nothing is returned")
    return (c())
  }
  if(!name %in% row.names(d.mat)) {
    print("name not in distance matrix, nothing is returned")
    return (c())
  }
  if(top > 0) 
    indexes = 2:(top + 1)
  else 
    indexes = (nrow(d.mat) + top + 1):nrow(d.mat)
  names(d.mat[order(d.mat[, name]), name][indexes])
}