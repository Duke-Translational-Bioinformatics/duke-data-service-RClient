# login.R
# Login to Duke Data Service
# Author: Ben Neely <nigelneely@gmail.com>
#############################################
login <- function(url=NA, rememberMe=TRUE) {
  a <- new("Config")
  try(.setConfig(a),silent=TRUE)
  .setCacheConfigObject(a)
  # After reading config, we need to determine how we're going to login:
  # we can either loging through a url_api_token or a sa_api_token
  # We'd rather have a sa_api_token because that's it's purpose and lasts longer
  # so we start there, then search for url_api_token, then finally move to getting a url_api_token
  # What url should we use?
  #################################################################################################
  if (!is.na(url)) {
    #try to match url provided to that of options from config to bypass user choice
    .setCache('defaultEndpoint')=url
  }
  # What token should we use for auth?
  #################################################################################################
  if (a@sa_api_token != "") {


  } else {
    .setCache('url_api_token',.scrapesessioncookie(.getCache('defaultEndpoint')));
    #Since we don't have a sa_api_token and this is the preferred connection method, we'll step through
    #that workflow to obtain a sa_api_token
    #Let's add this information to our cache key 'curlHeader'


  }


}

.getSaApiToken <- function(url_api_token=.getCache('url_api_token')) {
  .setCache('curlHeader',c(.getCache('curlHeader'),'Authorization'=url_api_token))
}

.scrapesessioncookie <- function(endpoint) {
  msg1<-paste0(c("You don't have a software agent key cached.\n",
                 "This is an important token when working with programmtic clients.\n",
                 "I'm going to redirect you to the DDS portal, please login and then\n",
                 "open the developer tools to copy the cookie 'apiToken'.\n",
                 "Press Return to continue>>>>"),
               collapse="")
  ack <-readline(writeLines(msg1))
  browseURL(endpoint);
  url_api_token = readline("Paste the apiToken cookie here: ");
  return(url_api_token)
}
