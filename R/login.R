#' Login function to DDS environment.
#'
#' @param url The URL to a valid DDS portal (https://dukeds.herokuapp.com,https://dukeds-dev.herokuapp.com,https://dukeds-uatest.herokuapp.com). Dev will be used if no url is given.
#' @param silent A logical indicating if the session login information should be communicated through standard output.
#' @return ddsLogin will return with a message detailing where credentials can be found (i.e. variable curlheader).
#' @examples
#' ddsLogin()
#' ddsLogin(url='https://dukeds-uatest.herokuapp.com')

ddsLogin <- function(url=NA,
                     silent=FALSE) {
  ddsLogout();
  #it's possible that the user will provide a url other than default, cache That
  if (!is.na(url)){.setCache('url',url);.setCache('askUserUrl',FALSE)}
  a <- new("Config")
  # Try to get the config and load into cache if there is one, if not at least make sure we have url
  #################################################################################################
  tryCatch({a<-.setConfig(a)},
           silent=TRUE,
           error = function(e){invisible(e)}
           )
  if (a@url=="") {a@url = .getCache('url')}
  .setCacheConfigObject(a)
  # We need a valid JWT to be considered "logged in" - three ways to get there...
  #################################################################################################
  if ((.getCache('sa_api_token') != "") & (as.numeric(.getCache('sa_api_token_expires')) > as.integer(as.POSIXct( Sys.time() ))))   {
    #nothing needs to be done, we have valid token and it hasn't expired
  } else if ((.getCache('sa_api_token') != "") & (as.numeric(.getCache('sa_api_token_expires')) < as.integer(as.POSIXct( Sys.time() )))) {
    #refreshing our sa_api_token
    .setCache('sa_api_token_expires',as.character(as.integer(as.POSIXct( Sys.time() ))))
    .getAndSetToken();
    #################################################################################################
    #update our config option to reflect what our cache now holds
    a=.loadConfigFromCache(a)
    .saveConfig(a)
  } else {
    ##reworking this workflow to align with the python client
    .getAndSetKeys(.getCache('url'));
    .getAndSetToken();
    .getUserInformation();
    #################################################################################################
    #update our config option to reflect what our cache now holds
    a=.loadConfigFromCache(a)
    .saveConfig(a)
  }

  #Welcome the user and let them know they're logged in and remind them which system they are using
  #################################################################################################
  if (silent==FALSE) {
    message(sprintf("Welcome %s %s - you are logged into %s! Please use the global variable 'curlheader' to call DDS endpoints.",.getCache('first_name'),.getCache('last_name'),.getCache('url')))
  }
  assign("curlheader", c(.getCache('curlHeader'),'Authorization'=.getCache('sa_api_token')), envir = .GlobalEnv)
  .setCache("curlheader",c(.getCache('curlHeader'),'Authorization'=.getCache('sa_api_token')))
}

#' Function to sign out of DDS. This amounts to removing the authorization jwt.
#'
#' @examples
#' ddsLogout()
ddsLogout <- function(){
  .setCache('sa_api_token','')
  .setCache('sa_api_token_expires','')
  .setCache("url","https://dukeds-dev.herokuapp.com")
  .setCache('askUserUrl',TRUE)
  assign("curlheader", c(.getCache('curlHeader')), envir = .GlobalEnv)
}

.getUserInformation <- function(api_token=.getCache('sa_api_token')) {
  r = ddsRequest(endpoint='/current_user',
                 httpheader=c(.getCache('curlHeader'),'Authorization'=api_token))
  #set some cached attributes
  .setCache('username',r$body$username)
  .setCache('first_name',r$body$first_name)
  .setCache('last_name',r$body$last_name)
  .setCache('userid',r$body$id)
}

.getAndSetToken <- function(user_key=.getCache('user_key'),sa_key=.getCache("sa_key")){
  body = list(agent_key=sa_key,
              user_key=user_key)
  r = ddsRequest(customrequest="POST",
                 endpoint='/software_agents/api_token',
                 body_list=body,
                 httpheader=.getCache('curlHeader'),
                 pass=TRUE)
  .setCache('sa_api_token',r$body$api_token)
  .setCache('sa_api_token_expires',r$body$expires_on)

}

.getAndSetKeys <- function(endpoint) {
  msg1<-paste0(c("You don't have a software agent key cached.\n",
                 "This is an important token when working with programmtic clients.\n",
                 "I'm going to redirect your browser to the DDS portal, please login and then\n",
                 "provide the requested information. If you need additional assistance,\n",
                 "please use these instructions:\n",
                 "https://github.com/Duke-Translational-Bioinformatics/duke-data-service-RClient \n",
                 "Press Return to continue>>>>"),
               collapse="")
  ack <-readline(writeLines(msg1))
  browseURL(endpoint)
  .setCache('sa_key',readline("Paste the Agent Secret Key: "))
  .setCache('user_key', readline("Paste the User Key: "))
}
