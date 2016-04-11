# login.R
# Login to Duke Data Service
# Author: Ben Neely <nigelneely@gmail.com>
#############################################
login <- function(url=NA, rememberMe=TRUE) {
  a <- new("Config")
  try(.setConfig(a),silent=TRUE)
  .setCacheConfigObject(a)
  if (.getCache('url')=='') {.setCache('url',.getCache('defaultUrl'))}
  # After reading config, we need to determine how we're going to login:
  # we can either loging through a url_api_token or a sa_api_token
  # We'd rather have a sa_api_token because that's it's purpose and lasts longer
  # so we start there, then search for url_api_token, then finally move to getting a url_api_token
  # What url should we use?
  #################################################################################################
  if (!is.na(url)) {
    #try to match url provided to that of options from config to bypass user choice
    .setCache('url')=url
  }
  # What token should we use for auth?
  #################################################################################################
  if (.getCache('sa_api_token') != "") {


  } else {
    .setCache('url_api_token',.scrapesessioncookie(.getCache('url')));
    .getUserInformation();
    #Since we don't have a sa_api_token and this is the preferred connection method, we'll step through
    #that workflow to obtain a sa_api_token
    #Let's add this information to our cache key 'curlHeader'
    .getSaApiToken()


  }


}
.getUserInformation <- function(api_token=.getCache('url_api_token')) {
  r = ddsRequest(endpoint='/current_user',
                 httpheader=c(.getCache('curlHeader'),'Authorization'=api_token))
  #set some cached attributes
  .setCache('username',r$body$username)
  .setCache('first_name',r$body$first_name)
  .setCache('last_name',r$body$last_name)
  .setCache('userid',r$body$id)
}

.getSaApiToken <- function(url_api_token=.getCache('url_api_token')){
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
    #finally get the sa api token
    body = list(agent_key=.getCache('sa_key'),
                user_key=.getCache('user_key'))
    r = ddsRequest(customrequest="POST",
                   endpoint='/software_agents/api_token',
                   body_list=body,
                   httpheader=c(.getCache('curlHeader')))
    .setCache('sa_api_token',r$body$api_token)
    .setCache('sa_api_token_expires',r$body$expires_on)
  }
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
