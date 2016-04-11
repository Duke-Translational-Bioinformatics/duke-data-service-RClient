.onAttach <-
  function(libname, pkgname)
  {
    tou <- "\nTERMS OF USE NOTICE - Protected Health Information:
    The Health Insurance Portability and Accountability Act of 1996 (HIPAA) established standards for health
    information that must be kept private and secure, called Protected Health Information (PHI). The use of
    PHI within the Duke Data Service is prohibited in this Alpha release. By proceeding, you attest that you
    will not enter PHI. If you are unclear about what constitutes PHI, or are uncertain about the nature of
    the data you use, please discontinue and contact the Duke University IT Security Office (security@duke.edu)
    for further information..\n"

    packageStartupMessage(tou)
  }

.onLoad <-
  function(libname, pkgname)
  {
    # low.speed.time increased to 600 to fix SYNR-949
    .setCache("curlOpts", list(low.speed.time=600, low.speed.limit=1, connecttimeout=300, followlocation=TRUE, ssl.verifypeer=TRUE, verbose = FALSE, cainfo=file.path(libname, pkgname)))
    .setCache("curlHeader", c('Content-Type'="application/json; charset=utf-8", 'Accept' = "application/json", "Accept-Charset"="utf-8"))
    .setCache("sessionRefreshDurationMin", 1440)
    .setCache("anonymous", FALSE)
    .setCache("downloadSuffix", "unpacked")
    .setCache("debug", FALSE)
    # this is the maximum number of times a web request will be tried when there is a temporary outage.  Must be >0
    .setCache("webRequestMaxTries", 10)
    .setCache("webRequestMaxRedirects", 3)
    .setCache("defaultUrl","https://dukeds-dev.herokuapp.com")
  }
