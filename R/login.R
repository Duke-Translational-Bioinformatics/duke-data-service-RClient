#' Login function to DDS environment.
#'
#' @param url The URL to a valid DDS portal (PROD,DEV,UATEST).
#' @param rememberME A logical indicating if the session login information should be stored later.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' ddslogin()
#' ddslogin(url='https://dukeds-uatest.herokuapp.com')
ddslogin <- function(url=NA, rememberMe=TRUE) {
  a <- new("Config")
  # What url should we use?
  #################################################################################################
  if (!is.na(url)) {
    #try to match url provided to that of options from config to bypass user choice
    .setCache('url',url)
  }
  if (.getCache('url')=='') {.setCache('url',.getCache('defaultUrl'))}
  # If the chosen URL is saved on a config, use it
  #################################################################################################
  try(a<-.setConfig(a),silent=TRUE)
  .setCacheConfigObject(a)

  # We need a valid JWT to be considered "logged in" - three ways to get there...
  #################################################################################################
  if ((.getCache('sa_api_token') != "") & (as.numeric(.getCache('sa_api_token_expires')) > as.integer(as.POSIXct( Sys.time() ))))   {
    #nothing needs to be done, we have valid token and it hasn't expired
  } else if ((.getCache('sa_api_token') != "") & (as.numeric(.getCache('sa_api_token_expires')) < as.integer(as.POSIXct( Sys.time() )))) {
    #refreshing our sa_api_token
    .getSaApiToken()
  } else {
    .setCache('url_api_token',.scrapesessioncookie(.getCache('url')));
    .getUserInformation(api_token=.getCache('url_api_token'));
    #Since we don't have a sa_api_token and this is the preferred connection method, we'll step through
    #that workflow to obtain a sa_api_token
    #Let's add this information to our cache key 'curlHeader'
    .getSaApiToken(url_api_token=.getCache('url_api_token'))
  }
  #We're now "logged in", if rememberMe is true, we need to write out to the config file
  #################################################################################################
  if (rememberMe) {
    #update our config option to reflect what our cache now holds
    a=.loadConfigFromCache(a)
    .saveConfig(a)
  }
  #Welcome the user and let them know they're logged in and remind them which system they are using
  #################################################################################################
  message(sprintf("Welcome %s %s - you are logged into %s! Please use the global variable 'curlheader' to call DDS endpoints.",.getCache('first_name'),.getCache('last_name'),.getCache('url')))
  assign("curlheader", c(.getCache('curlHeader'),'Authorization'=.getCache('sa_api_token')), envir = .GlobalEnv)
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

.getSaApiToken <- function(url_api_token=.getCache('sa_api_token')){
  #Check if there is already a user key if not generate one
  #########################################################
  if (.getCache('user_key')=="") {
    #first check if the system has one, could be that someone created one for python client, don't want to mess that up!
    r = ddsRequest(endpoint='/current_user/api_key',
                   httpheader=c(.getCache('curlHeader'),'Authorization'=url_api_token))
    if (r$status=="404") {
      r = ddsRequest(customrequest="PUT",
                     endpoint='/current_user/api_key',
                     httpheader=c(.getCache('curlHeader'),'Authorization'=url_api_token))
      .setCache('user_key',r$body$key)
    } else {
      .setCache('user_key',r$body$key)
    }
  }
  else {
    #maybe always want to check the server and not trust the config version?
  }
  if (.getCache('sa_key')=="") {
    body = list(name='ddsRClient',
                description='R programmatic Client',
                repo_url='https://github.com/Duke-Translational-Bioinformatics/duke-data-service-RClient')
    r = ddsRequest(customrequest="POST",
                   endpoint='/software_agents',
                   body_list=body,
                   httpheader=c(.getCache('curlHeader'),'Authorization'=url_api_token))
    if (r$status==201) {
      .setCache('sa_id',r$body$id)
      r = ddsRequest(customrequest="GET",
                     endpoint=paste0('/software_agents/',.getCache('sa_id'),'/api_key'),
                     httpheader=c(.getCache('curlHeader'),'Authorization'=url_api_token))
      if (r$status==200) {
        .setCache('sa_key',r$body$key)
      }
    }
  }
  #finally get the sa api token
  body = list(agent_key=.getCache('sa_key'),
              user_key=.getCache('user_key'))
  r = ddsRequest(customrequest="POST",
                 endpoint='/software_agents/api_token',
                 body_list=body)
  .setCache('sa_api_token',r$body$api_token)
  .setCache('sa_api_token_expires',r$body$expires_on)
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
