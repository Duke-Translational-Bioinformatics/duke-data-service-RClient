# cache.R
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
    firstname        = "character",
    lastname         = "character",
    userid           = "character",
    user_api_token   = "character",
    url_api_token    = "character",
    url              = "character",
    caching          = "logical",
    sa_key           = "character",
    sa_api_token     = "character"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    ondisk           = "~/.ddsclientR",
    sessionfile      = ".session",
    username         = "",
    firstname        = "",
    lastname         = "",
    userid           = "",
    user_api_token   = "",
    url_api_token    = "",
    url              = "https://dukeds-dev.herokuapp.com",
    caching          = TRUE,
    sa_key           = "",
    sa_api_token     = ""
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
          definition=function(Object)
          {
            if (!dir.exists(Object@ondisk)) {dir.create(Object@ondisk)}
            ini = sprintf("[%s]\r
\r
sa_key=%s\r
sa_api_token=%s\r
username=%s\r
firstname=%s\r
lastname=%s\r
userid=%s\r
user_api_token=%s\r
url_api_token=%s",
                    Object@url,Object@sa_key,Object@sa_api_token,Object@username,
                    Object@firstname,Object@lastname,Object@userid,Object@user_api_token,
                    Object@url_api_token);
            #Create our configfile
            write(ini,file.path(Object@ondisk,Object@sessionfile),sep="\r");
          }
)

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
