# ddsRequest.R
# Generic Function to interact with DDS
# Author: Ben Neely <nigelneely@gmail.com>
#############################################

ddsRequest<-function(
  url=.getCache('url'), # omitting the endpoint
  endpoint=NULL,
  body_list = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=.getCache('curlHeader'), # the headers
  curlHandle # the curl handle
  ) {
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
  return(list('header'=r$header,
              'body'=content(r,'parsed'),
              'status'=r$status
              )
         )
}
