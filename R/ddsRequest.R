# ddsRequest.R
# Generic Function to interact with DDS
# Author: Ben Neely <nigelneely@gmail.com>
#############################################

ddsRequest<-function(
  url=.getCache('defaultEndpoint'), # omitting the endpoint
  endpoint=NULL,
  body = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=.getCache('curlHeader'), # the headers
  curlHandle # the curl handle
  ) {
 if (customrequest=="GET") {
  print(url);
  r = GET(paste0(url,'/api/v1',endpoint),
          add_headers(httpheader))

 } else if (customrequest=="POST") {

 }
  else if (customrequest=="PUT") {
    r = PUT(paste0(url,'/api/v1',endpoint),
            add_headers(httpheader))
  }
  return(list('header'=r.header,
              'body'=content(r,'parsed')
              )
         )
}
