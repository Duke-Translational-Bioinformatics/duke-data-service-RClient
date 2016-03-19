# login.R
# Login to Duke Data Service
# Author: Ben Neely <nigelneely@gmail.com>
#############################################

defaultEndpoint <- "https://dukeds-dev.herokuapp.com/"

login <- function(sessionToken=NA, sa_apiKey=NA, rememberMe=FALSE, endpoint=defaultEndpoint) {
  ## parameters must be of length 1
  if (sum(is.na(sessionToken),is.na(sa_apiKey))==0) {
    sessionToken = .scrapesessioncookie()
  }
}

.scrapesessioncookie <- function(endpoint=defaultEndpoint) {
  browseURL(endpoint);
  readline("Enter the apiToken cookie:");
}


