#' Plot altmetrics
#' 
#' This function plots metrics on persons or articles from many sources.
#' 
#' @param diggname Digg username
#' @param gitid GitHub username
#' @param gittype GitHub type, 'user' or 'org'
#' @author Scott Chamberlain
#' @export
altmetrics <- function (diggname = "cboettig", gitid = 'cboettig', 
      gittype = "user") 
{
    #ggplot is easier to load.
    require(ggplot2); require(RCurl); require(RJSONIO); require(plyr)
	
    diggs <- digg_user(usernames = diggname)
    gits <- gitstats(id=gitid, type=gittype)
    diggs_ <- data.frame(names(diggs), diggs[[1]])
    names(diggs_) <- c("metric","value")
    gits_ <- data.frame(names(gits), gits)
    names(gits_) <- c("metric","value")
    df <- rbind(diggs_, gits_)
    
    myplot <- ggplot(df, aes(metric, value)) + geom_bar() + theme_bw(base_size = 18)
    print(myplot)
    invisible();
}

#' Get Digg metrics on a user.
#' 
#' Given a single or multiple Digg usernames or user ID's, returns for each
#'  number of diggs, comments, followers, following, and submissions.
#' @import RCurl RJSONIO
#' @param usernames Comma separated list of usernames (e.g., 'kevinrose,leolaporte').
#' @param userids Comma separated list of user IDs (e.g., '59,60').
#' @param url The base Digg url (leave to default).
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @return A data.frame with results.
#' @details Only supply one or the other of usernames or userids.
#' @export
#' @examples \dontrun{
#' digg_user(usernames = 'kevinrose,leolaporte')
#' digg_user(userids = '59,60')
#' }
digg_user <- function(usernames = NA, userids = NA,
      url = "http://services.digg.com/2.0/user.getInfo", 
      ..., curl = getCurlHandle())
{
  args <- list()
  if(!is.na(usernames))
    args$usernames <- usernames
  if(!is.na(userids))
    args$user_ids <- userids
  message(paste(url,"?",names(args[1]),"=",args[1],sep=''))
  xx <- getForm(url, 
   .params = args,
   ...,
   curl = curl)
  tt <- fromJSON(xx)
  out <- laply(tt$users, function(x) x$diggs)[[1]]
  names(out) <- 'diggs'
  out
}

#' Plot github stats
#' 
#' This function plots the total number of forks and followers for all repositories of a certain user or organization.
#' 
#' @param id name of the github user or organization
#' @param type either "user" or "org"
#' @author Scott Chamberlain
#' @export
gitstats <- function (id = "hadley", type = "user") 
{
  
  if (type == "user") {
    url = "https://api.github.com/users/"
  }
  else if (type == "org") {
    url = "https://api.github.com/orgs/"
  }
  else stop("parameter 'type' has to be either 'user' or 'org' ")
  url2 <- paste(url, id, "/repos?per_page=100", sep = "")
  xx <- RCurl::getURL(url2)
  tt <- RJSONIO::fromJSON(xx)
  if (!length(tt) == 1) {
    tt <- tt
  }
  else {
    stop("user or organization not found - search GitHub? - https://github.com/")
  }
  out <- plyr::ldply(tt, function(x) t(c(x$forks, x$watchers)))
  names(out) <- c("Forks", "Watchers")
  out$Forks <- as.integer(out$Forks)
  out$Watchers <- as.integer(out$Watchers)
  c(gitforks=sum(out$Forks), gitwatchers=sum(out$Watchers))
}