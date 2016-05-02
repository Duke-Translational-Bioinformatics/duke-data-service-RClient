#' DDS RESTful API wrapper function.
#'
#' @param url The URL to a valid DDS portal (PROD,DEV,UATEST).
#' @param endpoint The DDS endpoint to call out to
#' @param body_list Takes an R list to supply as the body of a call
#' @param customrequest One of the following ('GET','PUT','POST')
#' @param httpheader Takes an R list to supply as a modified header of a call
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' ddslogin()
#' ddslogin(url='https://dukeds-uatest.herokuapp.com')

ddsRequest<-function(
  url=.getCache('url'), # omitting the endpoint
  endpoint=NULL,
  body_list = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=.getCache('curlheader'), # the headers
  curlHandle # the curl handle
  ) {
 if (as.numeric(.getCache('sa_api_token_expires')) < as.integer(as.POSIXct( Sys.time() ))) {
   url=.getCache('url')
   eval(parse(text=paste0('ddslogin("',url,'")')))
 }
 message(sprintf("%s %s progress:",customrequest,paste0(url,'/api/v1',endpoint)))
 if (customrequest=="GET") {
  r = GET(paste0(url,'/api/v1',endpoint),
          add_headers(httpheader),
          progress())

 } else if (customrequest=="POST") {
   r = POST(paste0(url,'/api/v1',endpoint),
           add_headers(httpheader),
           body=body_list,
           encode="json",
           progress(type="up"))

 }
  else if (customrequest=="PUT") {
    r = PUT(paste0(url,'/api/v1',endpoint),
            add_headers(httpheader),
            progress())
  }
  else if (customrequest=="DELETE") {
    r = DELETE(paste0(url,'/api/v1',endpoint),
            add_headers(httpheader),
            progress())
  }
  return(list('header'=r$header,
              'body'=content(r,'parsed'),
              'status'=r$status
              )
         )
}
