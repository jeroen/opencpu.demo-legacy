#' Plot altmetrics
#' 
#' This function plots metrics on persons or articles from many sources.
#' 
#' @param diggname Digg username
#' @param gitid GitHub username
#' @param gittype GitHub type, 'user' or 'org'
#' @author Scott Chamberlain
#' @export
altmetrics <- function(diggname = list("cboettig","lo0ol"), 
                       gitid = list("cboettig", "holman"), gittype = "user") 
{
  #ggplot is easier to load.
  require(ggplot2); require(RCurl); require(RJSONIO); require(plyr)
  
  digout <- laply(diggname, digg_user)
  diggs_ <- data.frame(ldply(gitid), digout, rep("diggs", length(gitid)))
  gitout <- laply(gitid, git_wf)
  gits_ <- data.frame(ldply(gitid), ldply(gitout), 
        rep(c("gitforks","gitwatchers"), each=length(gitid)))
  names(diggs_) <- c("user","value","metric")
  names(gits_) <- c("user","value","metric")
  df <- rbind(diggs_, gits_)
  myplot <- ggplot(df, aes(metric, value, fill=user)) + 
    geom_bar(position="dodge") +
    theme_bw(base_size = 18)
  message("names default to Github user names")
  print(myplot)
  invisible();
}

#' Get Digg metrics on a user.
#' 
#' Given a single or multiple Digg usernames or user ID's, returns for each
#'  number of diggs, comments, followers, following, and submissions.
#' @param usernames Comma separated list of usernames (e.g., 'kevinrose,leolaporte').
#' @param userids Comma separated list of user IDs (e.g., '59,60').
#' @param url The base Digg url (leave to default).
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @author Scott Chamberlain
#' @export
digg_user <- function(usernames = NA, userids = NA,
      url = "http://services.digg.com/2.0/user.getInfo", 
      ..., curl = getCurlHandle())
{
  args <- list()
  if(!is.na(usernames))
    args$usernames <- usernames
  if(!is.na(userids))
    args$user_ids <- userids
#   message(paste(url,"?",names(args[1]),"=",args[1],sep=''))
  xx <- getForm(url, 
   .params = args,
   ...,
   curl = curl)
  tt <- fromJSON(xx)
  out <- laply(tt$users, function(x) x$diggs)[[1]]
  names(out) <- 'diggs'
  out
}

#' Get github stats (watchers and forks).
#' 
#' This function gets watchers and forks of github repos for a user or organization.
#' 
#' @param id name of the github user or organization
#' @param type either "user" or "org"
#' @author Scott Chamberlain
#' @export
git_wf <- function (id = "hadley", type = "user") 
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