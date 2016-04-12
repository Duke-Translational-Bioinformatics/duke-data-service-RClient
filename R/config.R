# config.R
# Login to Duke Data Service
# Author: Ben Neely <nigelneely@gmail.com>
#############################################
setClass(
  # Set the name for the class
  "Config",

  # Define the slots
  slots = c(
    ondisk           = "character",
    sessionfile      = "character",
    username         = "character",
    first_name       = "character",
    last_name        = "character",
    userid           = "character",
    user_key         = "character",
    url              = "character",
    sa_id            = "character",
    sa_key           = "character",
    sa_api_token     = "character",
    sa_api_token_expires = "character"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    ondisk           = "~/.ddsclientR",
    sessionfile      = ".session",
    username         = "",
    first_name       = "",
    last_name        = "",
    userid           = "",
    user_key         = "",
    url              = "",
    sa_id            = "",
    sa_key           = "",
    sa_api_token     = "",
    sa_api_token_expires = ""
  )
  # Optional validity function
  #   validity=function(object)
  #   {
  #     if((object@x < 0) || (object@y < 0)) {
  #       return("A negative number for one of the coordinates was given.")
  #     }
  #     return(TRUE)
  #   }
)

setMethod(f=".saveConfig",
          signature="Config",
          definition=function(Object){
            ################################################
            # New Config file
            ################################################
            if (!dir.exists(Object@ondisk)) {
              dir.create(Object@ondisk)
              ini=.createINIText(Object);
            #Create our configfile
            write(ini,file.path(Object@ondisk,Object@sessionfile),sep="\r");
            }
            ################################################
            # Config file already exist, treading carefully
            ################################################
            else {
              config_list<-.readConfig(Object)
              indx <- which(Object@url %in% config_list$url$V2)
              #This config file already contains information about this url
              if (length(indx)>0) {
                config_list$sa_api_token$V2[indx]=Object@sa_api_token
                config_list$sa_api_token_expires$V2[indx]=Object@sa_api_token_expires
                ini = config_list_to_ini(config_list)
              } else {
                #This is a new url so we can proceed by appending
                ini1 = .createINIText(Object);
                ini2 = config_list_to_ini(config_list);
                ini = paste(c(ini2,ini1),collapse="\r")

              }
            #Create our configfile
            write(ini,file.path(Object@ondisk,Object@sessionfile),sep="\r");
            }
})

setMethod(f=".readConfig",
          signature="Config",
          definition=function(Object)
          {
            #this function needs something like try to see if the file exists and is non-empty
            if (file.exists(file.path(Object@ondisk,Object@sessionfile))) {
              connection <- file(file.path(Object@ondisk,Object@sessionfile))
              Lines  <- readLines(connection)
              close(connection)
              Lines <- gsub("\\[", "url=", Lines)  # change section headers
              Lines <- gsub("\\]", "", Lines)  # change section headers
              Lines <- Lines[Lines!=""]
              connection <- textConnection(Lines)
              d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
              close(connection)
              config_list <- split(d,d$V1)
              return(config_list);
            } else {
              stop(paste0("Config file not found in ",file.path(Object@ondisk,Object@sessionfile), ", first run .saveConfig()"))
            }
          }
)

setMethod(f=".setConfig",
          signature="Config",
          definition=function(Object)
          {
            config_list = .readConfig(Object);
            .urls_list = unlist(lapply(seq_along(config_list$url$V2),function(x) paste0(x,') ',config_list$url$V2[x],'\n')))
            msg1<-paste0(c("You've cached at least one url in the past.\n",
                           .urls_list,
                           "Please enter the number of the url to use:"),
                         collapse="")
            url_indx <-readline(writeLines(msg1))
            chosen <- do.call("rbind",lapply(config_list,function(x) data.frame(x[as.numeric(url_indx),])))
            #Assuming only one software agent per url, but that will change enter code here
            #when that change is made
            #set slot values according to config and user choice
            ToParse  <- paste("Object@",chosen$V1,"<- '",chosen$V2, "'", sep="")
            INI.list <- list()
            eval(parse(text=ToParse))
            return(Object)
          }
)

setMethod(f=".loadConfigFromCache",
          signature="Config",
          definition=function(Object)
          {
            names <- slotNames(Object)
            for (i in 1:length(names)) {
              eval(parse(text=paste0("Object@",names[i],"='",.getCache(names[i]),"'")))
              }
          return(Object)
          }
)

setMethod(f=".createINIText",
          signature="Config",
          definition=function(Object)
          {
ini = sprintf("[%s]\r
\r
sa_id=%s\r
sa_key=%s\r
sa_api_token=%s\r
sa_api_token_expires=%s\r
username=%s\r
first_name=%s\r
last_name=%s\r
userid=%s\r
user_key=%s\r",
Object@url,Object@sa_id,Object@sa_key,Object@sa_api_token,Object@sa_api_token_expires,Object@username,
Object@first_name,Object@last_name,Object@userid,Object@user_key);
return(ini);
})

config_list_to_ini <- function(config_list) {
  temp = list();
  for (i in 1:length(config_list$url$V2)) {
    temp[i] = sprintf("[%s]\r
\r
sa_id=%s\r
sa_key=%s\r
sa_api_token=%s\r
sa_api_token_expires=%s\r
username=%s\r
first_name=%s\r
last_name=%s\r
userid=%s\r
user_key=%s\r",
config_list$url$V2[i],config_list$sa_id$V2[i],config_list$sa_key$V2[i],config_list$sa_api_token$V2[i],config_list$sa_api_token_expires$V2[i],
config_list$username$V2[i],config_list$first_name$V2[i],config_list$last_name$V2[i],config_list$userid$V2[i],config_list$user_key$V2[i]);
  }
  ini = paste(temp,collapse="\r")
  return(ini)
}

