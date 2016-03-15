## classes.R
## Class Definitions.
## Author: Ben Neely <nigelneely@gmail.com>
###############################################################################
##
## the global cache is a singleton
##
setClass(
  Class = "GlobalCache",
  representation = representation(env = "environment"),
  prototype = prototype(
    env = new.env(parent=emptyenv())
  )
)

##
## a file cache factory makes sure that all in-memory copies
## of a file cache object hold a reference to the same copy
##
setClass(
  Class = "FileCacheFactory",
  representation = representation(load = "environment", get="environment"),
  prototype = prototype(load=new.env(parent = emptyenv()), get= new.env(parent=emptyenv()))
)

##
## class for storing typed properties. Right now this is only
## used for storing synapse annotations, but in the future it will also be
## used to store typed synapse properties once the JSON schema is integrated
## with the R Synapse client
##

emptyNamedList<-structure(list(), names = character()) # copied from RJSONIO

setClass(
  Class="Config",
  representation=list(data="list")
)

# This is an abstract class for a typed list
# Concrete extensions should fill the 'type' slot
# with the class of the elements in the list
setClass(
  Class="TypedList",
  contains = "VIRTUAL",
  representation=representation(type="character", content="list"),
  prototype=list(content=list())
)

# This class is used to represent a null S4 value in a slot in another S4 class
# Normally S4 classes don't allow S4 slots to be null.  The recommended workaround
# is to create a class union, e.g. setClassUnion("myClassOrNULL", c("MyClass", "NULL"))
# However since this makes the class union a super-class of NULL, R 'breaks' when
# the package namespace is unloaded.  So rather than use NULL to represent a NULL
# object we use an instance of this special placeholder class.
setClass(
  Class = "NullS4Object",
  representation=representation(placeholder="character") # if omitted it's a 'virtual' class!!
)

