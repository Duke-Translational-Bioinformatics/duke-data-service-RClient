# login.R
# Login to Duke Data Service
# Author: Ben Neely <nigelneely@gmail.com>
#############################################
login <- function(url_api_token=NA, sa_api_token=NA, url=defaultEndpoint, rememberMe=TRUE) {
  a <- new("Config")
  ## parameters must be of length 1
  if (sum(is.na(url_api_token),is.na(sa_api_token))==0) {
    
    sessionToken = .scrapesessioncookie(url)
  }
  else if (!is.na(url_api_token)) {
    
    
  }
}

.scrapesessioncookie <- function(endpoint) {
  browseURL(endpoint);
  readline("Enter the apiToken cookie:");
}


