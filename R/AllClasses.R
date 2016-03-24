## Obtained from R synapseClient
## the global cache is a singleton
##
setClass(
  Class = "GlobalCache",
  representation = representation(env = "environment"),
  prototype = prototype(
    env = new.env(parent=emptyenv())
  )
)