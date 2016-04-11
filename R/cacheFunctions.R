# Obtained from R synapseClient
#
# Author: Matt Furia
# Modifications: Ben Neely
###############################################################################
## package-local 'getter'
.getCache <-
  function(key)
  {
    cache <- new("GlobalCache")
    cache@env[[key]]
  }

## package-local 'setter'
.setCache <-
  function(key, value)
  {
    cache <- new("GlobalCache")
    cache@env[[key]] <- value
  }

.printCache <-
  function()
  {
    cache <- new("GlobalCache")
    attributes <- ls(cache)
   a= sapply(attributes, function(x) {
      c(eval(parse(text=paste0(".getCache('",x,"')"))))
    })
   return(a)
  }
.setCacheConfigObject <-
  function(config)
  {
    if (!isS4(config)) {
      stop(sprintf("Can only set cache for objects with class='Config', this object is not S4"))
    } else if (attributes(config)$class[1]=="Config") {
      #Note, we don't return c because the .setCache saves to memory,
      #c is only here so that nothing is printed
      c <- lapply(slotNames(config), function(x) {.setCache(x,eval(parse(text=paste0("config@",x))))})
    } else {
      stop(sprintf("Can only set cache for objects with class='Config', not %s",attributes(config)$class[1]))
    }
  }

.deleteCache <-
  function(keys)
  {
    cache <- new("GlobalCache")
    indx <- which(keys %in% objects(cache@env))
    if(length(indx) > 0)
      rm(list=keys[indx], envir=cache@env)
  }

as.environment.GlobalCache <-
  function(x)
  {
    x@env
  }

