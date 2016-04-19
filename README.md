# duke-data-service-RClient
The official repo for the ddsRClient package - the R programmatic client to the duke-data-service.

![mail](images/logo2.png)

## Installation
To install the R package ddsRClient, follow one of the methods below.
### From GitHub  
Start by installing the package:  
```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Duke-Translational-Bioinformatics/duke-data-service-RClient")
```
Load the package for use:  
```
library(ddsRClient)
```
Please read the TERMS OF USE NOTICE!!
### Exploring Functionality
The R client is a constant work in progress, to see what functions are available
for use list all of the available functions:
```
ls("package:ddsRClient")
```
To learn more about any of these specific functions use the help menu:
```
?ddslogin
```
### Example: workflow to login to various DDS platforms
If the url parameter of `ddslogin` is not used, it will default to our development
environment. Otherwise, pass other portal platform URLs to login to other systems
as such
```
#development
ddslogin()
r = ddsRequest(customrequest="GET",
               endpoint='/current_user',
               httpheader=curlheader)
r$status

#UATEST
ddslogin(url='https://dukeds-uatest.herokuapp.com')
r = ddsRequest(customrequest="GET",
               endpoint='/current_user',
               httpheader=curlheader)
r$status

#PRODUCTION
ddslogin(url='https://dukeds.herokuapp.com')
r = ddsRequest(customrequest="GET",
               endpoint='/current_user',
               httpheader=curlheader)
r$status

#After a user has logged into these systems once. The url parameter is essentially
#not needed if doing interactive programming as the client will present the
#platforms that are saved, so all one needs to do is:
ddslogin()
r = ddsRequest(customrequest="GET",
               endpoint='/current_user',
               httpheader=curlheader)
r$status
```

### Example: Workflow to create a new project in UATEST environment
```
library(ddsRClient)
ddslogin(url='https://dukeds-uatest.herokuapp.com')
body = list(name='ddsRClient project creation',
            description='A project created with the R client')
r = ddsRequest(customrequest="POST",
               endpoint='/projects',
               body_list=body,
               httpheader=curlheader)
r$status
r$header
r$body
```
As you can see, the power of the login function is in creation of the global
variable `curlheader`. At this point in package development, the utility is that
all of the caching, saving, and updating of the curlheader is done by our package.
