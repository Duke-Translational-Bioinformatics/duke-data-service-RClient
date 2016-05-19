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

setClass(
  # Set the name for the classes
  "transaction",
  # Define the slots
  slots = c(
    isfolder         = "logical",
    local_object    = "character",
    local            = "list",
    dds              = "list",
    project          = "character",
    projectid        = "character"
  ),
  # Set the default values for the slots. (optional)
  prototype=list(
    isfolder         = FALSE,
    local_object    = "",
    local            = list("filenames"="",
                            "dirs"=""),
    dds              = list("filenames"="",
                            "dirs"="",
                            "dir_ids"=""),
    project          = "",
    projectid        = ""
  )
)
