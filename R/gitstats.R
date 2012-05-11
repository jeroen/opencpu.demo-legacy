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
    #ggplot is easier to load.
    require(ggplot2);
	
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
    out <- plyr::ldply(tt, function(x) t(c(x$name, x$forks, x$watchers)))
    names(out) <- c("Repo", "Forks", "Watchers")
    out$Forks <- as.integer(out$Forks)
    out$Watchers <- as.integer(out$Watchers)
    out2 <- reshape2::melt(out, id = 1)
    out2$value <- as.numeric(out2$value)
    out2$Repo <- as.factor(out2$Repo)
    repoorder <- unique(out2[order(out2$value, decreasing = FALSE), 
        ][, 1])
    out2$Repo <- factor(out2$Repo, levels = repoorder)
    myplot <- ggplot(out2, aes(Repo, value)) + geom_bar() + coord_flip() + 
        facet_wrap(~variable) + theme_bw(base_size = 18)
    print(myplot)
    invisible();
}

