#' DDS RESTful API wrapper function.
#'
#' @param url The URL to a valid DDS portal (PROD,DEV,UATEST).
#' @param endpoint The DDS endpoint to call out to
#' @param body_list Takes an R list to supply as the body of a call
#' @param customrequest One of the following ('GET','PUT','POST')
#' @param httpheader Takes an R list to supply as a modified header of a call
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' ddsLogin()
#' ddsLogin(url='https://dukeds-uatest.herokuapp.com')

ddsRequest<-function(
  url=paste0(.getCache('url'),'/api/v1'), # omitting the endpoint
  endpoint=NULL,
  body_list = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=.getCache('curlheader'), # the headers
  curlHandle, # the curl handle
  pass = FALSE #login uses ddsRequest and so we have to bypass the checking of global variables
  ) {

 if (pass==FALSE) {
   .verifyAuth(pass)
 }
 #message(sprintf("%s %s progress:",customrequest,paste0(url,'/api/v1',endpoint)))
 if (customrequest=="GET") {
  r = GET(paste0(url,endpoint),
          add_headers(httpheader))

 } else if (customrequest=="POST") {
   r = POST(paste0(url,endpoint),
           add_headers(httpheader),
           body=body_list,
           encode="json")

 }
  else if (customrequest=="PUT") {
    r = PUT(paste0(url,endpoint),
            add_headers(httpheader),
            body=body_list,
            encode="json")
  }
  else if (customrequest=="DELETE") {
    r = DELETE(paste0(url,endpoint),
            add_headers(httpheader))
  }
  if (!r$status_code %in% c(200,201)) {
    .checkDDSResponseStatus(endpoint,content(r,'parsed'));
  }
  return(list('header'=r$header,
              'body'=content(r,'parsed'),
              'status'=r$status,
              'request'=r$request
              )
         )
}

.verifyAuth <- function(pass) {
  if (is.null(.getCache('sa_api_token_expires')))  {
    url=.getCache('url')
    eval(parse(text=paste0('ddsLogin("',url,'")')))
    Sys.sleep(2)
  }
  else if (.getCache('sa_api_token_expires')=="") {
    url=.getCache('url')
    eval(parse(text=paste0('ddsLogin("',url,'")')))
    Sys.sleep(2)
  }
  else if (as.numeric(.getCache('sa_api_token_expires')) < as.integer(as.POSIXct( Sys.time() ))){
    url=.getCache('url')
    eval(parse(text=paste0('ddsLogin("',url,'")')))
    Sys.sleep(2)
  }
}

.checkDDSResponseStatus <- function(endpoint,body) {
    stop(sprintf('Endpoint %s is throwing error %s because "%s". Try: %s.',endpoint,body$error,body$reason,body$suggestion))
}
