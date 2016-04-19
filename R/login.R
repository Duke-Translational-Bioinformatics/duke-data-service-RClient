#' Login function to DDS environment.
#'
#' @param url The URL to a valid DDS portal (PROD,DEV,UATEST).
#' @param rememberME A logical indicating if the session login information should be stored later.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' ddslogin()
#' ddslogin(url='https://dukeds-uatest.herokuapp.com')

ddslogin <- function(url=NA, rememberMe=TRUE) {
  ddslogout();
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
    .getAndSetToken();
    #We're now "logged in", if rememberMe is true, we need to write out to the config file
    #################################################################################################
    if (rememberMe) {
      #update our config option to reflect what our cache now holds
      a=.loadConfigFromCache(a)
      .saveConfig(a)
    }
  } else {
    ##reworking this workflow to align with the python client
    .getAndSetKeys(.getCache('url'));
    .getAndSetToken();
    .getUserInformation();
    #We're now "logged in", if rememberMe is true, we need to write out to the config file
    #################################################################################################
    if (rememberMe) {
      #update our config option to reflect what our cache now holds
      a=.loadConfigFromCache(a)
      .saveConfig(a)
    }
  }

  #Welcome the user and let them know they're logged in and remind them which system they are using
  #################################################################################################
  message(sprintf("Welcome %s %s - you are logged into %s! Please use the global variable 'curlheader' to call DDS endpoints.",.getCache('first_name'),.getCache('last_name'),.getCache('url')))
  assign("curlheader", c(.getCache('curlHeader'),'Authorization'=.getCache('sa_api_token')), envir = .GlobalEnv)
}

#' Function to sign out of DDS. This amounts to removing the authorization jwt.
#'
#' @examples
#' ddslogout()
ddslogout <- function(){
  .setCache('sa_api_token','')
  .setCache('sa_api_token_expires','')
  .setCache("url","https://dukeds-dev.herokuapp.com")
  .setCache('askUserUrl',TRUE)
  curlheader = .getCache('curlHeader')
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
                 body_list=body)
  .setCache('sa_api_token',r$body$api_token)
  .setCache('sa_api_token_expires',r$body$expires_on)

}

.getAndSetKeys <- function(endpoint) {
  msg1<-paste0(c("You don't have a software agent key cached.\n",
                 "This is an important token when working with programmtic clients.\n",
                 "I'm going to redirect you to the DDS portal, please login and then\n",
                 "provide the requested information. If you need additional assistance,\n",
                 "please use these instructions:\n",
                 "https://github.com/Duke-GCB/DukeDSClient/blob/master/docs/GettingAgentAndUserKeys.md \n",
                 "Press Return to continue>>>>"),
               collapse="")
  ack <-readline(writeLines(msg1))
  browseURL(endpoint)
  .setCache('sa_key',readline("Paste the Agent Secret Key: "))
  .setCache('user_key', readline("Paste the User Key: "))
}
