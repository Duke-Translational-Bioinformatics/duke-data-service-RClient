# Program: removeUnwantedSoftwareAgents.R
# Date: 4/18/16
# Purpose: During development certain workflows could create many software agents,
#          this script will clean up those agents
###############################################################################################
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Duke-Translational-Bioinformatics/duke-data-service-RClient")

library(ddsRClient)

#development
###############################################################################################
ddslogin()
#how many software agents do we have:
#Some endpoints require a hack
#if the sa jwt doesn't work just use the following hack to get it to work:
# Some endpoints are forbidden to use by the SA tokens, the hack is to use the portal to get
# a portal token and override the SA token, to do this, log into the portal, open developer
# tools and look for the cookie apiToken - copy this jwt and paste the text next to Authorization
#curlheader = c(.getCache('curlHeader'),'Authorization'='')
r = ddsRequest(customrequest="GET",
               endpoint='/software_agents',
               httpheader=curlheader)
remove=character();
.getSAIDs <- function(somelist) {
  if (somelist$audit$created_by$username=='nn31') {
    remove=c(remove,somelist$id)

  }
}
ids <- sapply(r$body$results, function(x) .getSAIDs(x))
ids <-unlist(ids[!sapply(ids, is.null)])
for (i in 1:length(ids)){
solve = paste0('/software_agents/',ids[i])
r = ddsRequest(customrequest="DELETE",
               endpoint=solve,
               httpheader=curlheader)
print(r$status);
}

#UATEST
###############################################################################################
ddslogin(url='https://dukeds-uatest.herokuapp.com')
curlheader = c(.getCache('curlHeader'),'Authorization'='')
r = ddsRequest(customrequest="GET",
               endpoint='/software_agents',
               httpheader=curlheader)
remove=character();
.getSAIDs <- function(somelist) {
  if (somelist$audit$created_by$username=='nn31') {
    remove=c(remove,somelist$id)

  }
}
ids <- sapply(r$body$results, function(x) .getSAIDs(x))
ids <-unlist(ids[!sapply(ids, is.null)])
for (i in 1:length(ids)){
  solve = paste0('/software_agents/',ids[i])
  r = ddsRequest(customrequest="DELETE",
                 endpoint=solve,
                 httpheader=curlheader)
  print(r$status);
}
#PRODUCTION
###############################################################################################
ddslogin(url='https://dukeds.herokuapp.com')
curlheader = c(.getCache('curlHeader'),'Authorization'='')
r = ddsRequest(customrequest="GET",
               endpoint='/software_agents',
               httpheader=curlheader)
remove=character();
.getSAIDs <- function(somelist) {
  if (somelist$audit$created_by$username=='nn31') {
    remove=c(remove,somelist$id)

  }
}
ids <- sapply(r$body$results, function(x) .getSAIDs(x))
ids <-unlist(ids[!sapply(ids, is.null)])
for (i in 1:length(ids)){
  solve = paste0('/software_agents/',ids[i])
  r = ddsRequest(customrequest="DELETE",
                 endpoint=solve,
                 httpheader=curlheader)
  print(r$status);
}
#After a user has logged into these systems once. The url parameter is essentially
#not needed if doing interactive programming as the client will present the
#platforms that are saved, so all one needs to do is:
ddslogin()
